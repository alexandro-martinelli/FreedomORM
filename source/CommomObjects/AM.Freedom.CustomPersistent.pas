unit AM.Freedom.CustomPersistent;

interface

uses
  AM.Freedom.InterfacedObjects,
  AM.Freedom.IPersistent,
  System.Generics.Collections,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.FreedomObjectList,
  AM.Freedom.FreedomObject,
  AM.Freedom.ObjectMapper,
  AM.Freedom.Persistent.SetCursorResult,
  AM.Freedom.GroupCriteria,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.SQLMappers.CustomSelect,
  AM.Freedom.ICursor,
  AM.Freedom.RoundObject;

type
  TOnModifyRoundObject = procedure(pRoundObject: TRoundObject) of object;

  TCustomPersistent = class(TFreedomInterfacedObject, IPersistent)
  strict private
    FUpdateObjectMode: TUpdateObjectMode;
    FPersistedObjects: TDictionary<TClass, TPersistObjectMode>;
    FInUpdate: Boolean;
    FOwnsUpdate: TObject;
    FPoolList: TFreedomObjectList<TFreedomObject>;
    FUpdateObjectOptions: TUpdateObjectOptions;
    FOnModifyRoundObject: TOnModifyRoundObject;
    function CanPersistObject(pObjectClass: TClass; pMode: TPersistObjectMode): Boolean;
    {$IFDEF USEPOOL}
    procedure VerifyUpdateTime(pObject: TFreedomObject);
    {$IFEND}
    procedure ModifyRoundObject(pRoundObject: TRoundObject);
  strict protected
    procedure ExecuteAfterTriggers(pObjectMapper: TObjectMapper; pObjectState: TObjectState);
    procedure ExecuteBeforeTriggers(pObject: TObject; pObjectState: TObjectState);
    function SetCursor(pObject: TObject; pObjectState: TObjectState): TSetCursorResult; virtual;
    function GetCursor(pClass: TClass; pGroupCriteria: TGroupCriteria; pMapper: TObjectMapper): ICursor; virtual;
    function GetCursorList(pClass: TClass; pGroupCriteria: TGroupCriteria; pMapper: TObjectMapper): TCursorList; virtual;
    function GetCursorWithSelect(pSelect: TCustomSelect): ICursor; virtual;
    function GetCursorListWithSelect(pSelect: TCustomSelect): TCursorList; virtual;
    function GetLastUpdateTime(pClass: TClass; pID: Variant): TDateTime; virtual;
    procedure DropPersistent(pClass: TClass); virtual;
    procedure UpdatePersistent(pClass: TClass; pUpdateObjectOptions: TUpdateObjectOptions); virtual;
    function CompareDDLColumnTypes(pSourceDDLColumnType: TColumnType; pDestinyDDLColumnType: TColumnType): Boolean; virtual;
    function AdjustNameLength(pName: string): string; virtual;
    procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate; virtual;
    procedure DoEndUpdateOnError; virtual;
    function FixIntColumnTypeForDDLColumn(pColumnType: TColumnType): TColumnType; virtual;
  public
    constructor Create(pAsDefault: Boolean = False); virtual;
    destructor Destroy; override;
    procedure UpdateObject(pObjectClass: TClass; pUpdateObjectOptions: TUpdateObjectOptions = [uooJoins, uooDetails]);
    procedure DropObject(pObjectClass: TClass);
    procedure BeginUpdate(pOwnsUpdate: TObject);
    procedure EndUpdate(pOwnsUpdate: TObject);
    procedure EndUpdateOnError(pOwnsUpdate: TObject);
    function GetObject<T: TFreedomObject, constructor>(pID: Variant): T;
    function GetObjectOnList<T: TFreedomObject, constructor>(pPropertyNames: String; pPropertyValue: Variant): T;
    procedure AddNonPersistentObjectClass(pObjectClass: TClass; pMode: TPersistObjectMode);

    property UpdateObjectMode: TUpdateObjectMode read FUpdateObjectMode write FUpdateObjectMode default uomManual;
    property UpdateObjectOptions: TUpdateObjectOptions read FUpdateObjectOptions write FUpdateObjectOptions default [uooJoins, uooDetails];
    property OnModifyRoundObject: TOnModifyRoundObject read FOnModifyRoundObject write FOnModifyRoundObject;
  end;

implementation

uses
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.DefaultsClassRegister,
  AM.Freedom.Exceptions,
  AM.Freedom.ObjectMapper.TriggerMapper,
  AM.Freedom.ObjectMapper.CustomTrigger,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.IFreedomObject,
  System.SysUtils;

procedure TCustomPersistent.AddNonPersistentObjectClass(pObjectClass: TClass; pMode: TPersistObjectMode);
begin
  if CanPersistObject(pObjectClass, pMode) then
  begin
    FPersistedObjects.AddOrSetValue(pObjectClass, pMode);
  end;
end;

function TCustomPersistent.AdjustNameLength(pName: string): string;
const
  cByteLength = 255;
begin
  Result := Copy(pName, 1, cByteLength);
end;

procedure TCustomPersistent.BeginUpdate(pOwnsUpdate: TObject);
begin
  if not FInUpdate then
  begin
    FOwnsUpdate := pOwnsUpdate;
    FInUpdate := True;
    DoBeginUpdate;
  end;
end;

function TCustomPersistent.CanPersistObject(pObjectClass: TClass; pMode: TPersistObjectMode): Boolean;
begin
  Result := not FPersistedObjects.ContainsKey(pObjectClass);
  if (pMode = pomDrop) and not Result then
  begin
    Result := FPersistedObjects.Items[pObjectClass] <> pomDrop;
  end;
end;

function TCustomPersistent.CompareDDLColumnTypes(pSourceDDLColumnType, pDestinyDDLColumnType: TColumnType): Boolean;
begin
  Result := pDestinyDDLColumnType.IsEquals(pSourceDDLColumnType);
end;

constructor TCustomPersistent.Create(pAsDefault: Boolean);
begin
  FUpdateObjectMode := uomManual;
  FUpdateObjectOptions := [uooJoins, uooDetails];
  FPersistedObjects := TDictionary<TClass, TPersistObjectMode>.Create;
  FPoolList := TFreedomObjectList<TFreedomObject>.Create;
  if pAsDefault then
  begin
    TDefaultsClassRegister.DefaultPersistent := Self;
  end;
end;

destructor TCustomPersistent.Destroy;
begin
  FPersistedObjects.Free;
  if TDefaultsClassRegister.DefaultPersistent = IPersistent(Self) then
  begin
    TDefaultsClassRegister.DefaultPersistent := nil;
  end;
  FPoolList.Free;
  inherited;
end;

procedure TCustomPersistent.DoBeginUpdate;
begin
end;

procedure TCustomPersistent.DoEndUpdate;
begin
end;

procedure TCustomPersistent.DoEndUpdateOnError;
begin
end;

procedure TCustomPersistent.DropObject(pObjectClass: TClass);
begin
  if CanPersistObject(pObjectClass, pomDrop) then
  begin
    DropPersistent(pObjectClass);
    FPersistedObjects.AddOrSetValue(pObjectClass, pomDrop);
  end;
end;

procedure TCustomPersistent.DropPersistent(pClass: TClass);
begin
  raise EInvalidMethodCallOnClass.Create('DropPersistent', ClassName);
end;

procedure TCustomPersistent.EndUpdate(pOwnsUpdate: TObject);
begin
  if FInUpdate and (FOwnsUpdate = pOwnsUpdate) then
  begin
    FOwnsUpdate := nil;
    FInUpdate := False;
    DoEndUpdate;
  end;
end;

procedure TCustomPersistent.EndUpdateOnError(pOwnsUpdate: TObject);
begin
  if FInUpdate and (FOwnsUpdate = pOwnsUpdate) then
  begin
    FOwnsUpdate := nil;
    FInUpdate := False;
    DoEndUpdateOnError;
  end;
end;

procedure TCustomPersistent.ExecuteAfterTriggers(pObjectMapper: TObjectMapper; pObjectState: TObjectState);
var
  lTrigger: TTriggerMapper;
  lMetaTrigger: TCustomTrigger;
begin
  for lTrigger in pObjectMapper.Triggers do
  begin
    lMetaTrigger := lTrigger.TriggerClass.Create;
    try
      case pObjectState of
        Clean:
          if AfterUpdate in lTrigger.TriggerOptions then
          begin
            lMetaTrigger.AfterUpdate(pObjectMapper.RttiOptions.RttiObject);
          end;
        Inserted:
          if AfterInsert in lTrigger.TriggerOptions then
          begin
            lMetaTrigger.AfterInsert(pObjectMapper.RttiOptions.RttiObject);
          end;
        Deleted:
          if AfterDelete in lTrigger.TriggerOptions then
          begin
            lMetaTrigger.AfterDelete(pObjectMapper.RttiOptions.RttiObject);
          end;
      end;
    finally
      lMetaTrigger.Free;
    end;
  end;
end;

procedure TCustomPersistent.ExecuteBeforeTriggers(pObject: TObject; pObjectState: TObjectState);
var
  lTrigger: TTriggerMapper;
  lMetaTrigger: TCustomTrigger;
  lObjectMapper: TObjectMapper;
  pParams: TObjectToMapperParams;
begin
  pParams := TObjectToMapperParams.Create;
  pParams.ObjectInstance := pObject;
  lObjectMapper := TObjectToMapper.ObjectToMapper(pParams);
  try
    for lTrigger in lObjectMapper.Triggers do
    begin
      lMetaTrigger := lTrigger.TriggerClass.Create;
      try
        case pObjectState of
          Clean:
            if BeforeUpdate in lTrigger.TriggerOptions then
            begin
              lMetaTrigger.BeforeUpdate(lObjectMapper.RttiOptions.RttiObject);
            end;
          Inserted:
            if BeforeInsert in lTrigger.TriggerOptions then
            begin
              lMetaTrigger.BeforeInsert(lObjectMapper.RttiOptions.RttiObject);
            end;
          Deleted:
            if BeforeDelete in lTrigger.TriggerOptions then
            begin
              lMetaTrigger.BeforeDelete(lObjectMapper.RttiOptions.RttiObject);
            end;
        end;
      finally
        lMetaTrigger.Free;
      end;
    end;
  finally
    TObjectToMapper.UnlockMapper(lObjectMapper.GetHashCode);
  end;
end;

function TCustomPersistent.FixIntColumnTypeForDDLColumn(pColumnType: TColumnType): TColumnType;
begin
  Result := pColumnType;
end;

function TCustomPersistent.GetCursor(pClass: TClass; pGroupCriteria: TGroupCriteria; pMapper: TObjectMapper): ICursor;
begin
  raise EInvalidMethodCallOnClass.Create('GetCursor', ClassName);
end;

function TCustomPersistent.GetCursorList(pClass: TClass; pGroupCriteria: TGroupCriteria; pMapper: TObjectMapper): TCursorList;
begin
  raise EInvalidMethodCallOnClass.Create('GetCursorList', ClassName);
end;

function TCustomPersistent.GetCursorListWithSelect(pSelect: TCustomSelect): TCursorList;
begin
  raise EInvalidMethodCallOnClass.Create('GetCursorListWithSelect', ClassName);
end;

function TCustomPersistent.GetCursorWithSelect(pSelect: TCustomSelect): ICursor;
begin
  raise EInvalidMethodCallOnClass.Create('GetCursorWithSelect', ClassName);
end;

function TCustomPersistent.GetLastUpdateTime(pClass: TClass; pID: Variant): TDateTime;
begin
  raise EInvalidMethodCallOnClass.Create('GetLastUpdateTime', ClassName);
end;

function TCustomPersistent.GetObject<T>(pID: Variant): T;
var
  lFreedomObject: IFreedomObject;
begin
  Result := T(FPoolList.FindObjectByID(TClass(T), pID));
  if Assigned(Result) then
  begin
    {$IFDEF USEPOOL}
    VerifyUpdateTime(Result);
    {$IFEND}
  end
  else
  begin
    Result := T.Create;
    FPoolList.Add(Result);
    if Supports(Result, IFreedomObject, lFreedomObject) then
    begin
      lFreedomObject.DoFindWithID(pID);
    end;
  end;
end;

function TCustomPersistent.GetObjectOnList<T>(pPropertyNames: String; pPropertyValue: Variant): T;
begin
  Result := T(FPoolList.FindObjectByPropertyNames(TClass(T), pPropertyNames, pPropertyValue));
  {$IFDEF USEPOOL}
  if Assigned(Result) then
  begin
    VerifyUpdateTime(Result);
  end;
  {$IFEND}
end;

procedure TCustomPersistent.ModifyRoundObject(pRoundObject: TRoundObject);
begin
  if (Assigned(FOnModifyRoundObject)) then
  begin
    FOnModifyRoundObject(pRoundObject);
  end;
end;

function TCustomPersistent.SetCursor(pObject: TObject; pObjectState: TObjectState): TSetCursorResult;
begin
  raise EInvalidMethodCallOnClass.Create('SetCursor', ClassName);
end;

procedure TCustomPersistent.UpdateObject(pObjectClass: TClass; pUpdateObjectOptions: TUpdateObjectOptions);
begin
  if CanPersistObject(pObjectClass, pomUpdate) then
  begin
    UpdatePersistent(pObjectClass, pUpdateObjectOptions);
    FPersistedObjects.AddOrSetValue(pObjectClass, pomUpdate);
  end;
end;

procedure TCustomPersistent.UpdatePersistent(pClass: TClass; pUpdateObjectOptions: TUpdateObjectOptions);
begin
  raise EInvalidMethodCallOnClass.Create('UpdatePersistent', ClassName);
end;

{$IFDEF USEPOOL}
procedure TCustomPersistent.VerifyUpdateTime(pObject: TFreedomObject);
var
  lFreedomObject: IFreedomObject;
  pParams: TObjectToMapperParams;
  lMapper: TObjectMapper;
  lLastUpdateTime: TDateTime;
  lID: Variant;
begin
  pParams := TObjectToMapperParams.Create;
  pParams.ObjectInstance := pObject;
  lMapper := TObjectToMapper.ObjectToMapper(pParams);
  try
    lID := lMapper.Columns.IDColumn.ColumnValue;
    lLastUpdateTime := GetLastUpdateTime(pObject.ClassType, lID);
    if lLastUpdateTime > pObject.LastUpdateTime then
    begin
      if Supports(pObject, IFreedomObject, lFreedomObject) then
      begin
        lFreedomObject.DoFindWithID(lID);
      end;
    end;
  finally
    TObjectToMapper.UnlockMapper(lMapper.GetHashCode);
  end;
end;
{$IFEND}

end.
