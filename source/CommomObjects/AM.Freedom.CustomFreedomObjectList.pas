unit AM.Freedom.CustomFreedomObjectList;

interface

uses
  System.Generics.Collections,
  AM.Freedom.CustomFreedomObject,
  AM.Freedom.InterfacedObjects,
  AM.Freedom.GroupCriteria,
  AM.Freedom.IFreedomObject,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.MapperToObject,
  System.JSON,
  AM.Freedom.JSONAttributes;

type
  TCustomFreedomObjectList<T: TCustomFreedomObject, constructor> = class abstract(TInterfacedObjectList<T>)
  strict private
    [JSONUnparsed]
    FCreated: Boolean;
    [JSONListOnlyItens]
    FTrashList: TObjectList<T>;
    procedure AddToTrash(pObject: T);
    procedure FinalizeInsert(pObject: T);
    procedure CreateTrashList;
  strict protected
    procedure DoSearch(pGroupCriteria: TGroupCriteria); virtual; abstract;
    procedure DoBeforeInsert; virtual;
    procedure DoBeforeDelete(pObject: T; out pCanDelete: Boolean); virtual;
    procedure DoAfterInsert(pObject: T); virtual;
    procedure DoAfterDelete(pObject: T); virtual;
    function DoCreateObject: T; virtual;
    procedure DoBeforeClear; virtual;
    procedure MakeCreate; virtual;
  protected
    procedure SetObjectState(pObject: T; pState: TObjectState);
    procedure SetOldObjectState(pObject: T; pState: TObjectState);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    // Persiste
    function Insert: T; overload;
    function Insert(pObject: T): T; overload;
    function Insert(pIndex: Integer; pObject: T): T; overload;
    procedure Delete(pObject: T); reintroduce; overload;
    procedure Clear; reintroduce;
    // Não persiste
    function Add: T; overload;
    procedure Remove(pObject: T); reintroduce;
    procedure Empty;
    procedure PersistObjects; virtual;
    procedure UnpersistObjects; virtual;
    property TrashList: TObjectList<T> read FTrashList;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.ObjectMapper,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.Exceptions,
  AM.Freedom.JSONFreedomObject;


{ TCustomFreedomObjectList<T> }

function TCustomFreedomObjectList<T>.Add: T;
begin
  Result := Insert;
  SetObjectState(Result, TObjectState.Unknown);
end;

procedure TCustomFreedomObjectList<T>.AddToTrash(pObject: T);
var
  lCanDelete: Boolean;
  lItem: T;
begin
  CreateTrashList;
  lCanDelete := True;
  DoBeforeDelete(pObject, lCanDelete);
  if lCanDelete then
  begin
    lItem := Extract(pObject);
    if Assigned(lItem) then
    begin
      FTrashList.Add(lItem);
      SetObjectState(lItem, TObjectState.Deleted);
      DoAfterDelete(lItem);
    end;
  end;
end;

procedure TCustomFreedomObjectList<T>.AfterConstruction;
begin
  inherited;
  if not FCreated then
  begin
    inherited Create;
    MakeCreate;
  end;
  CreateTrashList;
end;

procedure TCustomFreedomObjectList<T>.Clear;
begin
  DoBeforeClear;
  while Count > 0 do
  begin
    AddToTrash(Items[0]);
  end;
end;

constructor TCustomFreedomObjectList<T>.Create;
begin
  inherited Create(True);
  MakeCreate;
end;

procedure TCustomFreedomObjectList<T>.CreateTrashList;
begin
  if (not Assigned(FTrashList)) then
  begin
    FTrashList := TObjectList<T>.Create;
  end;
end;

procedure TCustomFreedomObjectList<T>.Delete(pObject: T);
begin
  if Assigned(pObject) then
  begin
    AddToTrash(pObject);
  end;
end;

destructor TCustomFreedomObjectList<T>.Destroy;
begin
  FreeAndNil(FTrashList);
  inherited;
end;

procedure TCustomFreedomObjectList<T>.Empty;
begin
  DoBeforeClear;
  inherited Clear;
  CreateTrashList;
  FTrashList.Clear;
end;

function TCustomFreedomObjectList<T>.Insert: T;
begin
  DoBeforeInsert;
  Result := DoCreateObject;
  FinalizeInsert(Result);
end;

function TCustomFreedomObjectList<T>.Insert(pObject: T): T;
begin
  if Assigned(pObject) then
  begin
    DoBeforeInsert;
    FinalizeInsert(pObject);
  end;
  Result := pObject;
end;

procedure TCustomFreedomObjectList<T>.FinalizeInsert(pObject: T);
begin
  SetObjectState(pObject, TObjectState.Inserted);
  inherited Add(pObject);
  DoAfterInsert(pObject);
end;

function TCustomFreedomObjectList<T>.Insert(pIndex: Integer; pObject: T): T;
begin
  if Assigned(pObject) then
  begin
    DoBeforeInsert;
    SetObjectState(pObject, TObjectState.Inserted);
    inherited Insert(pIndex, pObject);
    DoAfterInsert(pObject);
  end;
  Result := pObject;
end;

procedure TCustomFreedomObjectList<T>.PersistObjects;
begin
  raise EInvalidMethodCallOnClass.Create('PersistObjects', ClassName);
end;

procedure TCustomFreedomObjectList<T>.Remove(pObject: T);
var
  ACanDelete: Boolean;
begin
  ACanDelete := True;
  DoBeforeDelete(pObject, ACanDelete);
  if ACanDelete then
  begin
    inherited Remove(pObject);
  end;
end;

procedure TCustomFreedomObjectList<T>.SetObjectState(pObject: T; pState: TObjectState);
var
  lFreedomObject: IFreedomObject;
begin
  if Supports(pObject, IFreedomObject, lFreedomObject) then
  begin
    lFreedomObject.SetObjectState(pState);
  end;
end;

procedure TCustomFreedomObjectList<T>.SetOldObjectState(pObject: T; pState: TObjectState);
var
  lFreedomObject: IFreedomObject;
begin
  if Supports(pObject, IFreedomObject, lFreedomObject) then
  begin
    lFreedomObject.SetOldObjectState(pState);
  end;
end;

procedure TCustomFreedomObjectList<T>.UnpersistObjects;
begin
  raise EInvalidMethodCallOnClass.Create('UnpersistObjects', ClassName);
end;

procedure TCustomFreedomObjectList<T>.DoAfterDelete(pObject: T);
begin
  // do when necessary
end;

procedure TCustomFreedomObjectList<T>.DoAfterInsert(pObject: T);
begin
  // do when necessary
end;

procedure TCustomFreedomObjectList<T>.DoBeforeClear;
begin
  // do when necessary
end;

procedure TCustomFreedomObjectList<T>.DoBeforeDelete(pObject: T; out pCanDelete: Boolean);
begin
  // do when necessary
end;

procedure TCustomFreedomObjectList<T>.DoBeforeInsert;
begin
  // do when necessary
end;

function TCustomFreedomObjectList<T>.DoCreateObject: T;
begin
  Result := T.Create;
end;

procedure TCustomFreedomObjectList<T>.MakeCreate;
begin
  FCreated := True;
  CreateTrashList;
end;

end.
