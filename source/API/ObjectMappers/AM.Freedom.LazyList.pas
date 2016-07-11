unit AM.Freedom.LazyList;

interface

uses
  AM.Freedom.ILazy,
  AM.Freedom.FreedomObjectList,
  AM.Freedom.FreedomObject,
  AM.Freedom.InterfacedObjects,
  AM.Freedom.GroupCriteria,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.ObjectMapper.LiveRefresh,
  AM.Freedom.JSONFreedomObject,
  AM.Freedom.JSONAttributes;

type
  TLazyList<T: TFreedomObject, constructor> = class(TJSONFreedomObject, ILazy)
  strict private
    [JSONUnparsed]
    FClassType: T;
    [JSONUnparsed]
    FValue: TFreedomObjectList<T>;
    [JSONUnparsed]
    FLazySearch: TGroupCriteria;
    [JSONUnparsed]
    FLazyID: Variant;
    procedure SetValue(const pValue: TFreedomObjectList<T>);
    procedure SetLazySearch(const pSearch: TGroupCriteria);
    function GetLoaded: Boolean;
    function GetObject: TObject;
    procedure SetFreeInternalValue;
    procedure SetOnGetLiveRefreshColumnValue(pOnGetLiveRefreshColumnValue: TOnGetLiveRefreshColumnValue);
  strict protected
    procedure Load;
    function GetLazyID: Variant;
    procedure SetLazyID(pLazyID: Variant);
    function GetLazySearch: TGroupCriteria;
    function GetInternalValue: TFreedomObjectList<T>;
    procedure SetInternalValue(pValue: TFreedomObjectList<T>);
    function GetLazyType: TLazyType;
    function GetValue: TFreedomObjectList<T>;
    function GetClassType: TClass;
    procedure SetNilLazySearch;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Value: TFreedomObjectList<T> read GetValue write SetValue;
    property LazySearch: TGroupCriteria read GetLazySearch write SetLazySearch;
    property LazyType: TLazyType read GetLazyType;
    property Loaded: Boolean read GetLoaded;
  end;

implementation

uses
  System.Variants,
  AM.Freedom.IFreedomObject,
  System.SysUtils;

{ TLazyList<T> }

constructor TLazyList<T>.Create;
begin
  FLazyID := Null;
end;

destructor TLazyList<T>.Destroy;
var
  lItem: T;
begin
  SetLazySearch(nil);
  FreeAndNil(FValue);
  inherited;
end;

function TLazyList<T>.GetClassType: TClass;
begin
  if Assigned(FClassType) then
  begin
    Result := FClassType;
  end
  else
  begin
    Result := T;
  end;
end;

function TLazyList<T>.GetInternalValue: TFreedomObjectList<T>;
begin
  Result := FValue;
end;

function TLazyList<T>.GetLazyID: Variant;
begin
  Result := FLazyID;
end;

function TLazyList<T>.GetLazySearch: TGroupCriteria;
begin
  Result := FLazySearch;
end;

function TLazyList<T>.GetLazyType: TLazyType;
begin
  Result := TLazyType.List;
end;

function TLazyList<T>.GetLoaded: Boolean;
begin
  Result := Assigned(FValue);
end;

function TLazyList<T>.GetObject: TObject;
begin
  Result := FValue;
end;

function TLazyList<T>.GetValue: TFreedomObjectList<T>;
var
  lList: IFreedomObjectList;
begin
  if not Assigned(FValue) then
  begin
    Load;
  end;
  Result := FValue;
end;

procedure TLazyList<T>.Load;
var
  lList: IFreedomObjectList;
begin
  if Supports(GetClassType, IFreedomObject) then
  begin
    FValue := TFreedomObjectList<T>.Create;
    if Supports(FValue, IFreedomObjectList, lList) then
    begin
      lList.DoSearch(GetLazySearch);
    end;
  end
  else
  begin
    raise Exception.Create('LazyList load only supports TFreedomObject');
  end;
end;

procedure TLazyList<T>.SetFreeInternalValue;
begin
  if Assigned(FValue) then
  begin
    FValue.Free;
  end;
  FValue := nil;
end;

procedure TLazyList<T>.SetInternalValue(pValue: TFreedomObjectList<T>);
begin
  if Assigned(FValue) then
  begin
    FValue.Free;
  end;
  FValue := pValue;
end;

procedure TLazyList<T>.SetLazyID(pLazyID: Variant);
begin
  if pLazyID <> Unassigned then
  begin
    FLazyID := pLazyID;
  end else
  begin
    FLazyID := Null;
  end;
end;

procedure TLazyList<T>.SetLazySearch(const pSearch: TGroupCriteria);
begin
  if Assigned(FLazySearch) then
  begin
    FLazySearch.Free;
  end;
  FLazySearch := pSearch;
end;

procedure TLazyList<T>.SetNilLazySearch;
begin
  FLazySearch := nil;
end;

procedure TLazyList<T>.SetOnGetLiveRefreshColumnValue(pOnGetLiveRefreshColumnValue: TOnGetLiveRefreshColumnValue);
begin
  // only interface implemetation
end;

procedure TLazyList<T>.SetValue(const pValue: TFreedomObjectList<T>);
begin
  FValue.Free;
  FValue := pValue;
end;

end.

