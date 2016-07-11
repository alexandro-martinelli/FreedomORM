unit AM.Freedom.FreedomObject;

interface

uses
  System.Generics.Collections,
  AM.Freedom.CustomFreedomObject,
  AM.Freedom.GroupCriteria,
  AM.Freedom.ObjectMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.IPersistent,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.Attributes,
  AM.Freedom.FreedomObjectCloner,
  AM.Freedom.JSONAttributes;

type
  TFreedomObjectClass = class of TFreedomObject;

  TFreedomObject = class abstract(TCustomFreedomObject)
  strict private
    [NoMapping]
    [JSONUnparsed]
    FInitialID: Variant;
    [NoMapping]
    [JSONUnparsed]
    FInitialSearch: TGroupCriteria;
    [NoMapping]
    [JSONUnparsed]
    FCreatedObjects: TList<TObject>;
    function PopulateObjectMapper(pID: Integer; pMapper: TObjectMapper): Boolean;
    function DoSetCursor(pObjectState: TObjectState; pPersistent: IPersistent): Boolean;
    procedure DoAfterSetCursorOnObjectState(pMapper: TObjectMapper; pObjectState: TObjectState);
    procedure DoUpdateColumnValueList(pObjectMapper: TObjectMapper);
    procedure SetObjectStateOnSetCursor(pObjectState: TObjectState);
    procedure PersistDetails(pObjectState: TObjectState; pPersistent: IPersistent);
    function CanPersistDetail(pColumn: TCustomColumnMapper; pObjectState: TObjectState): Boolean;
    function ExtractDetailObject(pColumn: TCustomColumnMapper; pObjectState: TObjectState): TObject;
    function ExtractObjectMapperFrom(pObject: TFreedomObject = nil): TObjectMapper;
    procedure FinalizeObjectMapperFrom(pMapperHash: Integer);
    procedure DestroyInternalObjects;
    procedure RegisterFreedomConstructors;
    procedure DoLoad;
    procedure ClearLazys;
    function GetCurrentId: Variant;
    function DoRoundValue(pValue: Extended; pColumn: TCustomColumnMapper): Variant;
    function DoBeforePersist(pObjectState: TObjectState): Boolean;
    procedure DoAfterPersist(pObjectState: TObjectState);
  strict protected
    procedure DoFindWithID(pID: Variant); override;
    procedure Persist(pObjectState: TObjectState; pPersistent: IPersistent = nil); override;
    procedure DoSearch(pGroupCriteria: TGroupCriteria); override;
    procedure DoFixGroupCriteriaForLoad(pGroupCriteria: TGroupCriteria); virtual;
    procedure DoAssignInitialValues; override;
    procedure AssignNonCreatedObjects(pObjectMapper: TObjectMapper); override;
    function DoBeforeUpdate: Boolean; virtual;
    function DoBeforeInsert: Boolean; virtual;
    function DoBeforeDelete: Boolean; virtual;
    procedure DoAfterUpdate; virtual;
    procedure DoAfterInsert; virtual;
    procedure DoAfterDelete; virtual;
    procedure DoBeforeReload; virtual;
    procedure DoAfterReload; virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(pID: Integer); overload;
    constructor Create(pSearch: TGroupCriteria); overload;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure CopyFrom(pSourceObject: TFreedomObject; pIgnoreColumnNames: Array of String; pDetails: Boolean = False; pClearId: Boolean = False);
    procedure Delete(pPersistent: IPersistent = nil);
    procedure Insert(pPersistent: IPersistent = nil);
    procedure Update(pPersistent: IPersistent = nil);
    procedure Reload;
    procedure ReloadWithCurrentId;
    property CurrentId: Variant read GetCurrentId;
  end;

implementation

uses
  System.Variants,
  System.SysUtils,
  System.Rtti,
  System.StrUtils,
  System.Classes,
  AM.Freedom.DefaultsClassRegister,
  AM.Freedom.ObjectMapper.MapperToObject,
  AM.Freedom.ObjectMapper.ObjectWriter,
  AM.Freedom.Persistent.SetCursorResult,
  AM.Freedom.ILazy,
  AM.Freedom.Helper.RttiField,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.ValueArgumentFactory,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.FreedomObjectList,
  AM.Freedom.INullable,
  AM.Freedom.Helper.RttiProperty,
  AM.Freedom.ICursor,
  AM.Freedom.ObjectMapper.ColumnValueItem,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.Exceptions,
  AM.Freedom.Helper.Variant,
  AM.Freedom.ObjectFactory,
  AM.Freedom.FreedomObjectsFactory, AM.Freedom.RoundObject,
  AM.Freedom.CustomRounder;

{ TFreedomObject }

constructor TFreedomObject.Create(pID: Integer);
begin
  Create;
  FInitialID := pID;
end;

procedure TFreedomObject.AfterConstruction;
begin
  FCreatedObjects := TList<TObject>.Create;
  if (FInitialID.IsNotNull or Assigned(FInitialSearch)) then
  begin
    DisableDefaults;
  end;
  try
    inherited AfterConstruction;
    DoLoad;
  finally
    EnableDefaults;
  end;
end;

procedure TFreedomObject.AssignNonCreatedObjects(pObjectMapper: TObjectMapper);
var
  lColumn: TCustomColumnMapper;
  lObject: TObject;
begin
  RegisterFreedomConstructors;
  for lColumn in pObjectMapper.Columns do
  begin
    if lColumn.RttiOptions.RttiField.FieldType.IsInstance then
    begin
      if (lColumn.RttiOptions.RttiObject <> nil) then
      begin
        lObject := lColumn.RttiOptions.RttiFieldHelper.GetObjectValue(lColumn.RttiOptions.RttiObject);
        if (not Assigned(lObject)) then
        begin
          lObject := TObjectFactory.CreateNewObject(lColumn.RttiOptions.RttiField.FieldType.AsInstance.MetaclassType);
          if (Assigned(lObject)) then
          begin
            FCreatedObjects.Add(lObject);
            lColumn.RttiOptions.RttiFieldHelper.SetObjectValue(lColumn.RttiOptions.RttiObject, lObject);
          end;
        end;
      end;
    end;
  end;
end;

function TFreedomObject.CanPersistDetail(pColumn: TCustomColumnMapper; pObjectState: TObjectState): Boolean;
begin
  if pColumn.LazyOptions.IsLazy and not pColumn.LazyOptions.IsLazyLoaded then
  begin
    Result := pObjectState = Deleted;
  end
  else
  begin
    Result := (not pColumn.LazyOptions.IsLazy or pColumn.LazyOptions.IsLazyLoaded);
  end;
  if Result then
  begin
    case pObjectState of
      Clean:
        Result := not(NoUpdate in pColumn.ColumnOptions);
      Inserted:
        Result := not(NoInsert in pColumn.ColumnOptions);
      Deleted:
        Result := not(NoDelete in pColumn.ColumnOptions);
    end;
  end;
end;

procedure TFreedomObject.CopyFrom(pSourceObject: TFreedomObject; pIgnoreColumnNames: Array of String; pDetails: Boolean; pClearId: Boolean);
begin
  TFreedomObjectCloner.MakeCopy(pSourceObject, Self, pIgnoreColumnNames, pDetails, pClearId);
end;

constructor TFreedomObject.Create;
begin
  FInitialID := Null;
  FInitialSearch := nil;
end;

constructor TFreedomObject.Create(pSearch: TGroupCriteria);
begin
  Create;
  FInitialSearch := pSearch;
end;

procedure TFreedomObject.Delete(pPersistent: IPersistent);
begin
  Persist(Deleted, pPersistent);
end;

destructor TFreedomObject.Destroy;
begin
  DestroyInternalObjects;
  FCreatedObjects.Free;
  FreeAndNil(FInitialSearch);
  inherited;
end;

procedure TFreedomObject.DestroyInternalObjects;
var
  lObject: TObject;
begin
  while FCreatedObjects.COunt > 0 do
  begin
    lObject := FCreatedObjects.Items[0];
    FCreatedObjects.Extract(lObject);
    try
      lObject.Free;
    except
      on E:Exception do
      begin
        if ((not E.InheritsFrom(EAccessViolation)) and (not E.InheritsFrom(EInvalidPointer))) then
        begin
          FCreatedObjects.Add(lObject);
          raise;
        end;
      end;
    end;
  end;
end;

procedure TFreedomObject.DoAfterPersist(pObjectState: TObjectState);
begin
  case pObjectState of
    Clean: DoAfterUpdate;
    Inserted: DoAfterInsert;
    Deleted: DoAfterDelete;
  end;
end;

procedure TFreedomObject.DoAfterDelete;
begin
  // override when necessary
end;

procedure TFreedomObject.DoAfterInsert;
begin
  // override when necessary
end;

procedure TFreedomObject.DoAfterUpdate;
begin
  // override when necessary
end;

procedure TFreedomObject.DoAfterSetCursorOnObjectState(pMapper: TObjectMapper; pObjectState: TObjectState);
begin
  if pObjectState in [Inserted, Clean] then
  begin
    SetObjectStateOnSetCursor(Clean);
    DoUpdateColumnValueList(pMapper);
  end
  else
  begin
    SetObjectStateOnSetCursor(Unknown);
  end;
end;

procedure TFreedomObject.DoAssignInitialValues;
var
  lMapper: TObjectMapper;
  lColumnValueItem: TColumnValueItem;
  lColumn: TCustomColumnMapper;
  lObject: TObject;
  lLazy: ILazy;

  function CreateArgumentForReferenceColumn: TCustomArgument;
  begin
    case TReferenceColumnMapper(lColumn).RefColumnType of
      ctyByte, ctySmallint:
        Result := TValueArgument.CreateAsSmallint(lColumnValueItem.ColumnValue);
      ctyInteger:
        Result := TValueArgument.CreateAsInteger(lColumnValueItem.ColumnValue);
      ctyInt64:
        Result := TValueArgument.CreateAsInt64(lColumnValueItem.ColumnValue);
      ctyChar, ctyString:
        Result := TValueArgument.CreateAsString(lColumnValueItem.ColumnValue);
      ctySingle, ctyDouble:
        Result := TValueArgument.CreateAsDouble(lColumnValueItem.ColumnValue);
      ctyCurrency:
        Result := TValueArgument.CreateAsCurrency(lColumnValueItem.ColumnValue);
      ctyExtended:
        Result := TValueArgument.CreateAsExtended(lColumnValueItem.ColumnValue);
      ctyDate:
        Result := TValueArgument.CreateAsDate(lColumnValueItem.ColumnValue);
      ctyTime:
        Result := TValueArgument.CreateAsTime(lColumnValueItem.ColumnValue);
      ctyDateTime:
        Result := TValueArgument.CreateAsDateTime(lColumnValueItem.ColumnValue);
    else
      raise EInvalidReferencedColumnColumnType.Create(lColumn.Name, TReferenceColumnMapper(lColumn).RefColumnType.ToString,
            lMapper.MetaClassType.ClassName);
    end;
  end;


  function CreateLazySearch: TGroupCriteria;
  begin
    Result := TGroupCriteria.Create;
    Result.AddCriteria(TCriteria.CreateAsEqual(
        TFieldArgument.Create(TReferenceColumnMapper(lColumn).RefColumnName, TReferenceColumnMapper(lColumn).RefObjectAlias),
        CreateArgumentForReferenceColumn));
  end;
begin
  lMapper := ExtractObjectMapperFrom;
  try
    for lColumnValueItem in ColumnValueList do
    begin
      lColumn := lMapper.Columns.FindColumn(lColumnValueItem.ColumnName, lColumnValueItem.ColumnType);
      if (Assigned(lColumn)) then
      begin
        if (lColumn.ColumnType.IsSimpleType or lColumn.ColumnType.IsOrdinal or (not lColumn.RttiOptions.RttiField.FieldType.IsInstance)) then
        begin
          if (lColumn.IsNullable) then
          begin
            lColumn.RttiOptions.RttiFieldHelper.SetNullableVariantValue(lColumn.RttiOptions.RttiObject, lColumnValueItem.ColumnValue);
          end
          else
          begin
            lColumn.ColumnValue := lColumnValueItem.ColumnValue;
            lColumn.RttiOptions.RttiField.SetValue(lColumn.RttiOptions.RttiObject, lColumn.AsTValue);
          end;
        end
        else if ((lColumn.ColumnType in [ctyJoin, ctyDetail]) and (lColumn.RttiOptions.RttiField.FieldType.IsInstance)) then
        begin
          lObject := lColumn.RttiOptions.RttiFieldHelper.GetObjectValue(lColumn.RttiOptions.RttiObject);
          if (Assigned(lObject)) then
          begin
            if (Supports(lObject, ILazy, lLazy)) then
            begin
              lLazy.SetFreeInternalValue;
              if (lColumn.LazyOptions.LazyType = Simple) then
              begin
                lLazy.SetLazyID(lColumnValueItem.ColumnValue);
              end
              else
              begin
                lLazy.SetLazySearch(CreateLazySearch);
              end;
            end;
          end;
        end
        else if (lColumn.ColumnType in [ctyBlob, ctyMemo]) then
        begin
          lObject := lColumn.RttiOptions.RttiFieldHelper.GetObjectValue(lColumn.RttiOptions.RttiObject);
          if (Assigned(lObject)) then
          begin
            if (lObject.InheritsFrom(TStrings)) then
            begin
              lColumnValueItem.StreamColumnValue.Position := 0;
              TStrings(lObject).LoadFromStream(lColumnValueItem.StreamColumnValue);
            end
            else if (lObject.InheritsFrom(TStream)) then
            begin
              lColumnValueItem.StreamColumnValue.Position := 0;
              TStream(lObject).CopyFrom(lColumnValueItem.StreamColumnValue, 0);
            end;
          end;
        end;
      end;
    end;
  finally
    FinalizeObjectMapperFrom(lMapper.GetHashCode);
  end;
end;

procedure TFreedomObject.DoFindWithID(pID: Variant);
var
  lPersistent: IPersistent;
  lMapper: TObjectMapper;
begin
  lPersistent := TDefaultsClassRegister.DefaultPersistent;
  lMapper := ExtractObjectMapperFrom;
  try
    if Assigned(lMapper) then
    begin
      PopulateObjectMapper(pID, lMapper)
    end;
  finally
    FinalizeObjectMapperFrom(lMapper.GetHashCode);
  end;
end;

procedure TFreedomObject.DoFixGroupCriteriaForLoad(pGroupCriteria: TGroupCriteria);
begin
  // override when necessary
end;

procedure TFreedomObject.DoLoad;
begin
  if (ColumnValueList <> nil) then
  begin
    ColumnValueList.Clear;
  end;
  ClearLazys;
  if FInitialID <> Null then
  begin
    DoFindWithID(FInitialID);
  end
  else if Assigned(FInitialSearch) then
  begin
    DoSearch(FInitialSearch);
  end;
end;

procedure TFreedomObject.DoSearch(pGroupCriteria: TGroupCriteria);
var
  lPersistent: IPersistent;
  lMapper: TObjectMapper;
  lCursor: ICursor;
begin
  if Assigned(pGroupCriteria) then
  begin
    DoFixGroupCriteriaForLoad(pGroupCriteria);
    pGroupCriteria.LimitRows := 1;
    lPersistent := TDefaultsClassRegister.DefaultPersistent;
    lMapper := ExtractObjectMapperFrom;
    lMapper.CurrentSchema := pGroupCriteria.SchemaName;
    if (lMapper.CurrentSchema = '') and (lMapper.Schemas.Count > 0) then
    begin
      lMapper.CurrentSchema := lMapper.Schemas.DefaultSchema.Name;
    end;
    try
      pGroupCriteria.ChangeAlias(lMapper.Alias);
      lCursor := lPersistent.GetCursor(ClassType, pGroupCriteria, lMapper);
      TObjectWriter.PersistentCursorToObject(lCursor, lMapper, lMapper.CurrentSchema, GetColumnValueList);
      SetObjectStateWithoutChangeOld(TObjectState.Clean);
      SetOldObjectState(TObjectState.Clean);
    finally
      FinalizeObjectMapperFrom(lMapper.GetHashCode);
    end;
  end;
end;

function TFreedomObject.DoSetCursor(pObjectState: TObjectState; pPersistent: IPersistent): Boolean;
var
  lSetCursor: TSetCursorResult;
begin
  Result := False;
  if not Assigned(pPersistent) then
  begin
    pPersistent := TDefaultsClassRegister.DefaultPersistent;
  end;
  lSetCursor := pPersistent.SetCursor(Self, pObjectState);
  try
    try
      Result := lSetCursor.Result;
      if Result then
      begin
        DoAfterSetCursorOnObjectState(lSetCursor.ObjectMapper, pObjectState);
      end;
      if not(pObjectState in [Inserted, Clean]) or not Result then
      begin
        TObjectToMapper.UnlockMapper(lSetCursor.ObjectMapper.GetHashCode);
      end;
    except
      TObjectToMapper.UnlockMapper(lSetCursor.ObjectMapper.GetHashCode);
      raise;
    end;
  finally
    lSetCursor.Free;
  end;
end;

function TFreedomObject.DoRoundValue(pValue: Extended; pColumn: TCustomColumnMapper): Variant;
var
  lRoundObject: TRoundObject;
  lRounder: TCustomRounder;
begin
  lRoundObject := TRoundObject.Create;
  try
    lRoundObject.RoundValue := pValue;
    lRoundObject.RoundDecimals := pColumn.RoundOptions.RoundDecimals;
    lRoundObject.RoundDecimalsMode := pColumn.RoundOptions.RoundDecimalsMode;
    lRoundObject.ColumnName := pColumn.Name;
    lRoundObject.ObjectClass := pColumn.RttiOptions.RttiObject.ClassType;
    if (pColumn.RoundOptions.CanBeModified) then
    begin
      TDefaultsClassRegister.DefaultPersistent.ModifyRoundObject(lRoundObject);
    end;
    try
      if (lRoundObject.Handled) then
      begin
        Result := lRoundObject.RoundValue;
      end
      else
      begin
        lRounder := TDefaultsClassRegister.DefaultRounderClass.Create;
        try
          Result := lRounder.RoundValue(lRoundObject);
        finally
          lRounder.Free;
        end;
      end;
    except
    end;
  finally
    FreeAndNil(lRoundObject);
  end;
end;


procedure TFreedomObject.DoUpdateColumnValueList(pObjectMapper: TObjectMapper);
var
  lColumnValueList: TColumnValueList;
  lColumn: TCustomColumnMapper;
  lColumnValue: Variant;
  lName: string;
begin
  lColumnValueList := GetColumnValueList;
  lColumnValueList.Clear;
  if not pObjectMapper.IsList then
  begin
    for lColumn in pObjectMapper.Columns do
    begin
      lColumnValue := Null;
      if (not lColumn.IsExtension) and ((pObjectMapper.CurrentSchema = '') or
         (lColumn.Schemas.FindSchema(pObjectMapper.CurrentSchema) <> nil)) then
      begin
        if lColumn.ColumnType.IsBlob then
        begin
          lColumnValueList.AddValue(lColumn.Name, Null, lColumn.ColumnType, lColumn.CurrentStream);
        end
        else
        begin
          if lColumn.ColumnType in [ctyJoin, ctyDetail] then
          begin
            if lColumn.RttiOptions.RttiField.FieldType.IsInstance then
            begin
              lName := lColumn.Name;
            end
            else
            begin
              lName := TJoinedColumnMapper(lColumn).RefResultColumnName;
            end;
            lColumnValueList.AddValue(lName, lColumn.CurrentValue, lColumn.ColumnType);
          end
          else
          begin
            lColumnValue := lColumn.CurrentValue;
            if (lColumn.ColumnType.IsFloat) and lColumn.RoundOptions.IsRounded and not VarIsNull(lColumnValue) then
            begin
              lColumnValue := DoRoundValue(lColumnValue, lColumn);
            end;
            lColumnValueList.AddValue(lColumn.Name, lColumnValue, lColumn.ColumnType);
          end;
        end;
      end;
    end;
  end;
end;

function TFreedomObject.ExtractDetailObject(pColumn: TCustomColumnMapper; pObjectState: TObjectState): TObject;
var
  lObject: TObject;
  lLazyList: ILazy;
  procedure DoLoadIfDeleted;
  begin
    if (pObjectState = Deleted) and not pColumn.LazyOptions.IsLazyLoaded then
    begin
      lLazyList.Load;
    end;
  end;
begin
  Result := nil;
  lObject := pColumn.RttiOptions.RttiFieldHelper.GetObjectValue(Self);
  if pColumn.LazyOptions.IsLazy then
  begin
    if Supports(lObject, ILazy, lLazyList) then
    begin
      DoLoadIfDeleted;
      Result := lLazyList.GetObject;
    end
    else
    begin
      Result := lObject;
    end;
  end;
end;

function TFreedomObject.ExtractObjectMapperFrom(pObject: TFreedomObject): TObjectMapper;
var
  lParams: TObjectToMapperParams;
begin
  if not Assigned(pObject) then
  begin
    pObject := Self;
  end;
  lParams := TObjectToMapperParams.Create;
  lParams.ObjectInstance := pObject;
  Result := TObjectToMapper.ObjectToMapper(lParams);
end;

procedure TFreedomObject.FinalizeObjectMapperFrom(pMapperHash: Integer);
begin
  TObjectToMapper.UnlockMapper(pMapperHash);
end;

function TFreedomObject.GetCurrentId: Variant;
var
  lObjectMapper: TObjectMapper;
begin
  lObjectMapper := ExtractObjectMapperFrom;
  try
    Result := lObjectMapper.Columns.IDColumn.CurrentValue;
  finally
    FinalizeObjectMapperFrom(lObjectMapper.GetHashCode);
  end;
end;

procedure TFreedomObject.Insert(pPersistent: IPersistent);
begin
  Persist(Inserted, pPersistent);
  FInitialID := GetCurrentId;
  FreeAndNil(FInitialSearch);
end;

procedure TFreedomObject.ClearLazys;
var
  lMapper: TObjectMapper;
  lColumn: TCustomColumnMapper;
  lLazy: ILazy;
  lObject: TObject;
begin
  lMapper := ExtractObjectMapperFrom;
  try
    for lColumn in lMapper.Columns do
    begin
      if (lColumn.LazyOptions.IsLazy) then
      begin
        lObject := lColumn.RttiOptions.RttiFieldHelper.GetObjectValue(lColumn.RttiOptions.RttiObject);
        if Supports(lObject, ILazy, lLazy) then
        begin
          lLazy.SetFreeInternalValue;
        end;
      end;
    end;
  finally
    FinalizeObjectMapperFrom(lMapper.GetHashCode);
  end;
end;

procedure TFreedomObject.Persist(pObjectState: TObjectState; pPersistent: IPersistent);
var
  lCursorSeted: Boolean;
begin
  if (IsWriteable) then
  begin
    if DoBeforePersist(pObjectState) then
    begin
      if not Assigned(pPersistent) then
      begin
        pPersistent := TDefaultsClassRegister.DefaultPersistent;
      end;
      pPersistent.BeginUpdate(Self);
      try
        lCursorSeted := DoSetCursor(pObjectState, pPersistent);
        if lCursorSeted and HasDetails then
        begin
          PersistDetails(pObjectState, pPersistent);
        end;
        pPersistent.EndUpdate(Self);
        DoAfterPersist(pObjectState);
      except
        pPersistent.EndUpdateOnError(Self);
        raise;
      end;
    end;
  end;
end;

function TFreedomObject.DoBeforePersist(pObjectState: TObjectState): Boolean;
begin
  case pObjectState of
    Clean: Result := DoBeforeUpdate;
    Inserted: Result := DoBeforeInsert;
    Deleted: Result := DoBeforeDelete;
    else
      Result := True;
  end;
end;

function TFreedomObject.DoBeforeDelete: Boolean;
begin
  Result := True;
end;

function TFreedomObject.DoBeforeInsert: Boolean;
begin
  Result := True;
end;

function TFreedomObject.DoBeforeUpdate: Boolean;
begin
  Result := True;
end;

procedure TFreedomObject.PersistDetails(pObjectState: TObjectState; pPersistent: IPersistent);
var
  lMapper: TObjectMapper;
  lColumn: TCustomColumnMapper;
  lObject: TObject;
  lFreedomObjectList: IFreedomObjectList;
  procedure AdjustDetailsCaseObjectState;
  begin
    case pObjectState of
      Deleted:
        lFreedomObjectList.Clear;
      Inserted:
        if (Self.ObjectState <> Inserted) then
        begin
          lFreedomObjectList.SetPersistState(Inserted);
        end;
    end;
  end;
  procedure DoPersistDetails;
  var
    lIdColumn: TCustomColumnMapper;
  begin
    if Supports(lObject, IFreedomObjectList, lFreedomObjectList) then
    begin
      AdjustDetailsCaseObjectState;
      lIdColumn := lMapper.Columns.FindColumn(lColumn.Name, TReferenceColumnMapper(lColumn).RefColumnType);
      lFreedomObjectList.SetRefColumnValue(TReferenceColumnMapper(lColumn).RefColumnName, lIdColumn.CurrentValue);
      lFreedomObjectList.PersistObjects;
    end;
  end;

begin
  lMapper := ExtractObjectMapperFrom;
  try
    for lColumn in lMapper.Columns.DetailColumns do
    begin
      if (lColumn.ColumnType = ctyDetail) and CanPersistDetail(lColumn, pObjectState) then
      begin
        lObject := ExtractDetailObject(lColumn, pObjectState);
        if Assigned(lObject) then
        begin
          DoPersistDetails;
        end;
      end;
    end;
  finally
    FinalizeObjectMapperFrom(lMapper.GetHashCode);
  end;
end;

function TFreedomObject.PopulateObjectMapper(pID: Integer; pMapper: TObjectMapper): Boolean;
var
  lColumn: TCustomColumnMapper;
  lSearch: TGroupCriteria;
  lPersistent: IPersistent;
  lCursor: ICursor;
begin
  lColumn := pMapper.Columns.IDColumn;
  Result := False;
  if Assigned(lColumn) then
  begin
    lPersistent := TDefaultsClassRegister.DefaultPersistent;
    if Assigned(lPersistent) then
    begin
      lSearch := TGroupCriteria.Create;
      try
        DoFixGroupCriteriaForLoad(lSearch);
        lSearch.AddCriteria(TCriteria.CreateAsEqual(TFieldArgument.Create(lColumn.Name, pMapper.Alias, lColumn.Alias),
            TValueArgumentFactory.CreateAsColumnType(lColumn.ColumnType, pID)));
        lSearch.LimitRows := 1;
        lCursor := lPersistent.GetCursor(ClassType, lSearch, pMapper);
        TObjectWriter.PersistentCursorToObject(lCursor, pMapper, TObjectToMapper.ExtractDefaultSchema(Self.ClassInfo), GetColumnValueList);
        SetObjectStateOnSetCursor(Clean);
        Result := True;
      finally
        lSearch.Free;
        lCursor := nil;
      end;
    end;
  end
  else
  begin
    raise ECannotLoadWithoutIDColumn.Create(ClassName);
  end;
end;

procedure TFreedomObject.RegisterFreedomConstructors;
begin
  TFreedomObjectsFactory.RegisterFreedomObject;
end;

procedure TFreedomObject.Reload;
begin
  DoBeforeReload;
  if VarIsNull(FInitialID) then
  begin
    ReloadWithCurrentId;
  end
  else
  begin
    DoLoad;
  end;
  DoAfterReload;
end;

procedure TFreedomObject.ReloadWithCurrentId;
begin
  DoBeforeReload;
  FreeAndNil(FInitialSearch);
  FInitialID := GetCurrentId;
  DoLoad;
  DoAfterReload;
end;

procedure TFreedomObject.SetObjectStateOnSetCursor(pObjectState: TObjectState);
begin
  SetObjectStateWithoutChangeOld(pObjectState);
  SetOldObjectState(pObjectState);
end;

procedure TFreedomObject.Update(pPersistent: IPersistent);
begin
  Persist(Clean, pPersistent);
end;

procedure TFreedomObject.DoBeforeReload;
begin
  // override when necessary
end;

procedure TFreedomObject.DoAfterReload;
begin
  // override when necessary
end;

end.
