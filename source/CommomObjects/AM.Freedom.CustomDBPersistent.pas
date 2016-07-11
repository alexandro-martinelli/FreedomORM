unit AM.Freedom.CustomDBPersistent;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SyncObjs,
  AM.Freedom.SQLCommands.TableRowCommands,
  AM.Freedom.CustomPersistent,
  AM.Freedom.IDBPersistent,
  AM.Freedom.ObjectMapper,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.GroupCriteria,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.TriggerMapper,
  AM.Freedom.SQLMappers.SelectClause,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.ObjectMapper.DDLObjects,
  AM.Freedom.SQLCommands.TableCommands,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLMappers.IDDLExtracter,
  AM.Freedom.Consts,
  AM.Freedom.SQLCommands.SequenceCommands,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.Persistent.SetCursorResult,
  AM.Freedom.DBPersistent.IDBConnector,
  AM.Freedom.DBPersistent.DBParam,
  AM.Freedom.SQLMappers.CustomSQLMapper,
  AM.Freedom.SQLMappers.SQLLinker,
  AM.Freedom.FreedomObject,
  AM.Freedom.DefaultsClassRegister,
  AM.Freedom.SQLMappers.CustomSelect,
  AM.Freedom.ObjectMapper.Schemas,
  AM.Freedom.ValueArgumentFactory,
  AM.Freedom.ICursor,
  AM.Freedom.Helper.RttiField;

type
  TOnGetObjectStateFromSchemaControl = function(pCurrentObjectState: TObjectState; pDefaultSchemaName,
      pCurrentSchemaName: String): TObjectState{$IFNDEF ORMTEST} of object {$IFEND};

  TDBPersistentClass = class of TCustomDBPersistent;

  TCustomDBPersistent = class abstract(TCustomPersistent, IDBPersistent)
  strict private
    FISQLMapper: ISQLMapper;
    FIDDLExtracter: IDDLExtracter;
    FDBConnector: IDBConnector;
    FCriticalSection: TCriticalSection;
    FWaitingForUpdate: TList<TClass>;
    FOnGetObjectStateFromSchemaControl: TOnGetObjectStateFromSchemaControl;
    FMakeForeignConstraintsWithJoinColumn: Boolean;
    procedure EnterCriticalSection;
    procedure LeaveCriticalSection;
    function ExtractDBStatement(pCommand: TCustomCommand): IDBStatement;
    function DoSetCursor(pObjectMapper: TObjectMapper; pObjectState: TObjectState): Boolean;
    function DoSetCursorWithSchema(pObjectMapper: TObjectMapper; pObjectState: TObjectState): Boolean;
    function DoGetObjectStateFromSchemaControl(pObjectState: TObjectState; pSchema: TSchemaItem; pObjectMapper: TObjectMapper): TObjectState;
    function DoCaseObjectState(pObjectMapper: TObjectMapper; pObjectState: TObjectState; pRowCommand: TCustomCommand): Boolean;
    function DoSetCursorOnInserted(pObjectMapper: TObjectMapper; pObjectState: TObjectState; pRowCommand: TCustomCommand): Boolean;
    function GetReturningValueForInsert(pCursor: ICursor; pObjectMapper: TObjectMapper): Boolean;
    function ExtractSelectClause(pObjectMapper: TObjectMapper; pGroupCriteria: TGroupCriteria): TSelectClause;
    function ExtractCursorFromSelect(pSelect: TSelectClause): ICursor;
    function ExtractCursorListFromSelect(pSelect: TSelectClause): TCursorList;
    function GenerateDropTableCommand(pDDLEntity: TDDLEntity): TCommandList;
    function CompareDDLs(pSourceDDL, pCompareDDL: TDDLEntity): TCommandList;
    procedure DoExecuteCommands(pCommands: TCommandList);
    function ExtractParametersFromCommand(pCommand: TCustomCommand): TDBParams;
    function ExtractParametersFromInsertCommand(pCommand: TInsertCommand): TDBParams;
    function ExtractParametersFromUpdateCommand(pCommand: TUpdateCommand): TDBParams;
    function ExtractMapperFrom(pMetaClassType: TClass; pObject: TObject = nil; pOptions: TObjectToMapperOptions = [SubLevels]): TObjectMapper;
    procedure UnlockMapper(pMapperHash: Integer);
    procedure DoDropDetails(pMapper: TObjectMapper; pClass: TClass);
    procedure DoUpdateJoins(pMapper: TObjectMapper);
    procedure DoUpdateDetails(pMapper: TObjectMapper; pClass: TClass);
    procedure DoUpdatePersistent(pClass: TClass; pUpdateObjectOptions: TUpdateObjectOptions);
    function DoInternalUpdatePersistent(pMapper: TObjectMapper): Boolean;
    function DoUpdatePersistentSchema(pSchemaMapper: TObjectMapper): Boolean;
    procedure DoDropPersistent(pMapper: TObjectMapper);
    function ExtractCursorFromMapper(pMapper: TObjectMapper; pGroupCriteria: TGroupCriteria): ICursor;
    function ExtractSchemaMapper(pMapper: TObjectMapper; pSchema: TSchemaItem): TObjectMapper;
    function GetMakeForeignConstraintsWithJoinColumn: Boolean;
    procedure SetMakeForeignConstraintsWithJoinColumn(const Value: Boolean);
    function UserFieldClassTypeToString(pClassType: TClass): String;
    function UserColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass;
  strict protected
    function NewDBStatement: IDBStatement;
    function GetDBConnector: IDBConnector;
    function GetCursor(pClass: TClass; pGroupCriteria: TGroupCriteria; pMapper: TObjectMapper): ICursor; override; final;
    function GetCursorList(pClass: TClass; pGroupCriteria: TGroupCriteria; pMapper: TObjectMapper): TCursorList; override; final;
    function GetCursorWithSelect(pSelect: TCustomSelect): ICursor; override;
    function GetCursorListWithSelect(pSelect: TCustomSelect): TCursorList; override;
    function SetCursor(pObject: TObject; pObjectState: TObjectState): TSetCursorResult; override; final;
    function ExtractDBDDL(pObjectMapper: TObjectMapper; pDDLOptions: TDDLOptions = TConsts.cDDLAll): TDDLEntity; overload; virtual;
    function ExtractDBDDL(pName: String; pSchemaName: String; pDDLOptions: TDDLOptions = [Fields, Constraints]): TObjectMapper; overload; virtual;
    procedure UpdatePersistent(pClass: TClass; pUpdateObjectOptions: TUpdateObjectOptions); override;
    procedure DropPersistent(pClass: TClass); override;
    function GetLastUpdateTime(pClass: TClass; pID: Variant): TDateTime; override;
    function ColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass; virtual;
    function FixIntColumnTypeForDDLColumn(pColumnType: TColumnType): TColumnType; override;
  strict protected
    function GetSQLMapper: ISQLMapper;
    function GetDDLExtracter: IDDLExtracter;
    function CreateSQLMapper(pAsDefault: Boolean): ISQLMapper; virtual;
    function CreateDDLExtracter: IDDLExtracter; virtual;
    function CompareDDLColumnTypes(pSourceDDLColumnType: TColumnType; pDestinyDDLColumnType: TColumnType): Boolean; override;
    function AdjustNameLength(pName: string): string; override;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    procedure DoEndUpdateOnError; override;
    function SupportedCommand(pCommand: TCustomCommand): Boolean; virtual;
  public
    constructor Create(pDBConnector: IDBConnector; pAsDefault: Boolean = False); reintroduce; virtual;
    destructor Destroy; override;
    procedure Reconnect;
    procedure Commit;
    procedure Roolback;
    procedure StartTransaction;
    function InTransaction: Boolean;
    function Execute(pCommand: TCustomCommand; pFreeOnExecute: Boolean = True): Boolean;
    function ExecuteQuery(pCommand: TCustomCommand; pFreeOnExecute: Boolean = True): ICursor;
    function CreateLinker<T: TFreedomObject, constructor>: TSQLLinker<T>;
    procedure RegisterUserColumnTypeToFieldCommandClass(pColumnType: TColumnType; pFieldCommandClass: TFieldCommandClass);
    procedure RegisterUserCommandClassToString(pFieldCommandClass: TFieldCommandClass; pAsString: String);
    procedure UnregisterUserColumnTypeToFieldCommandClass(pColumnType: TColumnType; pFieldCommandClass: TFieldCommandClass);
    procedure UnregisterUserCommandClassToString(pFieldCommandClass: TFieldCommandClass; pAsString: String);
    property DBConnector: IDBConnector read GetDBConnector;
    property SQLMapper: ISQLMapper read FISQLMapper;
    property OnGetObjectStateFromSchemaControl: TOnGetObjectStateFromSchemaControl read FOnGetObjectStateFromSchemaControl write FOnGetObjectStateFromSchemaControl;
    property DDLExtracter: IDDLExtracter read GetDDLExtracter;
    property MakeForeignConstraintsWithJoinColumn: Boolean read GetMakeForeignConstraintsWithJoinColumn write SetMakeForeignConstraintsWithJoinColumn;
  end;

implementation

{ TCustomDBPersistent }

uses
  AM.Freedom.Exceptions,
  AM.Freedom.TextGenerator.MasterTextGenerator,
  AM.Freedom.ObjectMapper.MapperToObject,
  System.SysUtils,
  AM.Freedom.ObjectMapper.MapperToSelectClause,
  AM.Freedom.ObjectMapperToDDLEntity,
  AM.Freedom.ObjectMapper.DDLCompare,
  AM.Freedom.InterfacedObjects,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.ObjectMapper.ObjectMapperToSchemaMapper,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.Helper.Variant,
  AM.Freedom.IFreedomObject,
  AM.Freedom.ILazy,
  AM.Freedom.RegisterUserFieldCommandToString,
  AM.Freedom.ObjectMapper.CustomObjectCursor, 
  AM.Freedom.ColumnTypeDBConverter;

function TCustomDBPersistent.AdjustNameLength(pName: string): string;
begin
  Result := GetSQLMapper.AdjustNameLength(pName);
end;

procedure TCustomDBPersistent.Commit;
begin
  FDBConnector.Commit;
end;

function TCustomDBPersistent.CompareDDLColumnTypes(pSourceDDLColumnType, pDestinyDDLColumnType: TColumnType): Boolean;
begin
  case pSourceDDLColumnType of
    ctyByte:
      Result := pDestinyDDLColumnType in [ctyByte, ctySmallint, ctyInteger, ctyInt64];
    ctySmallint:
      Result := pDestinyDDLColumnType in [ctySmallint, ctyInteger, ctyInt64];
    ctyInteger:
      Result := pDestinyDDLColumnType in [ctyInteger, ctyInt64];
    ctySingle, ctyDouble, ctyExtended, ctyCurrency:
      Result := pDestinyDDLColumnType in [ctySingle, ctyDouble, ctyExtended, ctyCurrency];
    ctyChar:
      Result := pDestinyDDLColumnType in [ctyChar, ctyString];
  else
    Result := inherited;
  end;
end;

function TCustomDBPersistent.CompareDDLs(pSourceDDL, pCompareDDL: TDDLEntity): TCommandList;
var
  lDDLCompareParams: TDDLCompareParams;
begin
  lDDLCompareParams := TDDLCompareParams.Create;
  lDDLCompareParams.ObjectDDL := pSourceDDL;
  lDDLCompareParams.DataBaseDDL := pCompareDDL;
  lDDLCompareParams.GetFieldCommandClass := ColumnTypeToFieldCommandClass;
  lDDLCompareParams.Persistent := Self;
  lDDLCompareParams.SQLMapper := GetSQLMapper;
  lDDLCompareParams.DDLExtracter := GetDDLExtracter;
  Result := TDDLCompare.CompareDDLs(lDDLCompareParams);
end;

constructor TCustomDBPersistent.Create(pDBConnector: IDBConnector; pAsDefault: Boolean);
begin
  inherited Create(pAsDefault);
  FMakeForeignConstraintsWithJoinColumn := True;
  FDBConnector := pDBConnector;
  FCriticalSection := TCriticalSection.Create;
  FWaitingForUpdate := TList<TClass>.Create;
  FISQLMapper := CreateSQLMapper(pAsDefault);
  FISQLMapper.OnGetUserFieldClassTypeToString := UserFieldClassTypeToString;
  FISQLMapper.OnGetUserColumnTypeToFieldCommandClass := UserColumnTypeToFieldCommandClass;
  FIDDLExtracter := CreateDDLExtracter;
end;

function TCustomDBPersistent.CreateDDLExtracter: IDDLExtracter;
begin
  raise EInvalidMethodCallOnClass.Create('CreateDDLExtracter', ClassName);
end;

function TCustomDBPersistent.CreateSQLMapper(pAsDefault: Boolean): ISQLMapper;
begin
  raise EInvalidMethodCallOnClass.Create('CreateSQLMapper', ClassName);
end;

function TCustomDBPersistent.NewDBStatement: IDBStatement;
begin
  Result := FDBConnector.NewStatement;
end;

destructor TCustomDBPersistent.Destroy;
begin
  Roolback;
  FDBConnector := nil;
  FISQLMapper := nil;
  FIDDLExtracter := nil;
  FCriticalSection.Leave;
  FCriticalSection.Free;
  FWaitingForUpdate.Free;
  inherited;
end;

function TCustomDBPersistent.DoSetCursorOnInserted(pObjectMapper: TObjectMapper; pObjectState: TObjectState; pRowCommand: TCustomCommand): Boolean;
var
  lCursor: ICursor;
  lDBStatement: IDBStatement;
begin
  lDBStatement := ExtractDBStatement(pRowCommand);
  lCursor := nil;
  try
    lCursor := lDBStatement.ExecuteQuery;
    Result := GetReturningValueForInsert(lCursor, pObjectMapper);
  finally
    lDBStatement := nil;
    lCursor := nil;
    pRowCommand.Free;
  end;
end;

function TCustomDBPersistent.DoSetCursorWithSchema(pObjectMapper: TObjectMapper; pObjectState: TObjectState): Boolean;
var
  lSchema: TSchemaItem;
  lSchemaMapper: TObjectMapper;
  lObjectState: TObjectState;
  lRowCommand: TCustomCommand;
  {$IFDEF USEPOOL}
  lColumn: TCustomColumnMapper;
  {$IFEND}
  procedure ExecuteBeforeTrrigersIfStateChange;
  begin
    if lObjectState <> pObjectState then
    begin
      ExecuteBeforeTriggers(pObjectMapper.RttiOptions.RttiObject, lObjectState);
    end;
  end;
  procedure ExecuteAfterTrrigersIfStateChange;
  begin
    if lObjectState <> pObjectState then
    begin
      ExecuteAfterTriggers(pObjectMapper, lObjectState);
    end;
  end;

begin
  Result := True;
  {$IFDEF USEPOOL}
  lColumn := pObjectMapper.Columns.FindColumn(TConsts.cLastUpdateTimeColumn);
  if (Assigned(lColumn)) then
  begin
    lColumn.RttiOptions.RttiFieldHelper.SetVariantValue(lColumn.RttiOptions.RttiObject, Now);
  end;
  {$IFEND}
  for lSchema in pObjectMapper.Schemas do
  begin
    lSchemaMapper := ExtractSchemaMapper(pObjectMapper, lSchema);
    try
      lObjectState := DoGetObjectStateFromSchemaControl(pObjectState, lSchema, pObjectMapper);
      lRowCommand := FISQLMapper.GenerateCommand(lSchemaMapper, lObjectState);
      if Assigned(lRowCommand) then
      begin
        ExecuteBeforeTrrigersIfStateChange;
        Result := DoCaseObjectState(lSchemaMapper, lObjectState, lRowCommand);
        ExecuteAfterTrrigersIfStateChange;
      end
      else
      begin
        if lObjectState = pObjectState then
        begin
          Break;
        end;
      end;
    finally
      lSchemaMapper.Free;
    end;
  end;
end;

procedure TCustomDBPersistent.DoUpdateDetails(pMapper: TObjectMapper; pClass: TClass);
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in pMapper.Columns.DetailColumns do
  begin
    if TReferenceColumnMapper(lColumn).RefMetaClass = nil then
    begin
      raise EInvalidReferenceClass.Create(lColumn.Name, pClass.ClassName, lColumn.RttiOptions.RttiField.Name);
    end;
    UpdateObject(TReferenceColumnMapper(lColumn).RefMetaClass);
  end;
end;

procedure TCustomDBPersistent.DoUpdateJoins(pMapper: TObjectMapper);
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in pMapper.Columns.JoinColumns do
  begin
    if TReferenceColumnMapper(lColumn).RefMetaClass <> nil then
    begin
      UpdateObject(TReferenceColumnMapper(lColumn).RefMetaClass);
    end;
  end;
end;

procedure TCustomDBPersistent.DoUpdatePersistent(pClass: TClass; pUpdateObjectOptions: TUpdateObjectOptions);
var
  lMapper: TObjectMapper;
  lCorrectExecuted: Boolean;
  lJoinUpdated: Boolean;
  procedure DoUpdateSelf;
  begin
    lCorrectExecuted := DoInternalUpdatePersistent(lMapper);
  end;
  procedure TryUpdateJoins;
  begin
    if (uooJoins in pUpdateObjectOptions) then
    begin
      try
        DoUpdateJoins(lMapper);
      except
        lJoinUpdated := False;
      end;
    end;
  end;
  function ReTryUpdateJoinAndSelf: Boolean;
  begin
    Result := True;
    if (uooJoins in pUpdateObjectOptions) and (not lJoinUpdated) then
    begin
      try
        DoUpdateJoins(lMapper);
        lJoinUpdated := True;
      except
        Result := False;
      end;
      if lJoinUpdated then
      begin
        DoUpdateSelf;
      end;
    end
    else
    begin
      Result := False;
    end;
  end;
  procedure TryUpdateDetails;
  begin
    if lCorrectExecuted then
    begin
      if (uooDetails in pUpdateObjectOptions) then
      begin
        DoUpdateDetails(lMapper, pClass);
      end;
    end;
  end;
begin
  lMapper := ExtractMapperFrom(pClass);
  try
    lCorrectExecuted := False;
    lJoinUpdated := True;
    try
      TryUpdateJoins;
      try
        DoUpdateSelf;
      except
        if not ReTryUpdateJoinAndSelf then
        begin
          raise;
        end;
      end;
    finally
      TryUpdateDetails;
    end;
  finally
    UnlockMapper(lMapper.GetHashCode);
  end;
end;

function TCustomDBPersistent.DoUpdatePersistentSchema(pSchemaMapper: TObjectMapper): Boolean;
var
  lSourceDDL: TDDLEntity;
  lCompareDDL: TDDLEntity;
  lCommands: TCommandList;
  lCanReconnect: Boolean;
  procedure VerifyReconnect;
  begin
    if lCanReconnect then
    begin
      Reconnect;
    end;
  end;
begin
  lSourceDDL := TObjectMapperToDDLEntity.ExtractDDL(pSchemaMapper, Self);
  lCompareDDL := ExtractDBDDL(pSchemaMapper);
  try
    lCommands := CompareDDLs(lSourceDDL, lCompareDDL);
    lCanReconnect := lCommands.Count > 0;
    VerifyReconnect;
    DoExecuteCommands(lCommands);
    Result := True;
    VerifyReconnect;
  finally
    FreeAndNil(lCompareDDL);
    FreeAndNil(lSourceDDL);
  end;
end;

function TCustomDBPersistent.DoInternalUpdatePersistent(pMapper: TObjectMapper): Boolean;
var
  lSchema: TSchemaItem;
  lSchemaMapper: TObjectMapper;
begin
  Result := False;
  if pMapper.Schemas.Count = 0 then
  begin
    pMapper.Schemas.AddSchema('', True);
  end;
  for lSchema in pMapper.Schemas do
  begin
    lSchemaMapper := ExtractSchemaMapper(pMapper, lSchema);
    try
      Result := DoUpdatePersistentSchema(lSchemaMapper);
    finally
      lSchemaMapper.Free;
    end;
  end;
end;

procedure TCustomDBPersistent.DoBeginUpdate;
begin
  StartTransaction;
end;

function TCustomDBPersistent.DoCaseObjectState(pObjectMapper: TObjectMapper; pObjectState: TObjectState; pRowCommand: TCustomCommand): Boolean;
begin
  Result := False;
  case pObjectState of
    Clean, Deleted:
      begin
        Result := Execute(pRowCommand);
      end;
    Inserted:
      begin
        Result := DoSetCursorOnInserted(pObjectMapper, pObjectState, pRowCommand);
      end;
  end;
end;

procedure TCustomDBPersistent.DoDropDetails(pMapper: TObjectMapper; pClass: TClass);
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in pMapper.Columns.DetailColumns do
  begin
    if TReferenceColumnMapper(lColumn).RefMetaClass = nil then
    begin
      raise EInvalidReferenceClass.Create(lColumn.Name, pClass.ClassName, lColumn.RttiOptions.RttiField.Name);
    end;
    DropObject(TReferenceColumnMapper(lColumn).RefMetaClass);
  end;
end;

procedure TCustomDBPersistent.DoDropPersistent(pMapper: TObjectMapper);
var
  lDDLEntity: TDDLEntity;
  lCommands: TCommandList;
begin
  lDDLEntity := ExtractDBDDL(pMapper, [Fields, Sequences]);
  try
    if Assigned(lDDLEntity) then
    begin
      lCommands := GenerateDropTableCommand(lDDLEntity);
      lDDLEntity.Free;
      if lCommands.Count > 0 then
      begin
        Reconnect;
        DoExecuteCommands(lCommands);
      end;
    end;
  finally
    pMapper.Free;
  end;
end;

procedure TCustomDBPersistent.DoEndUpdate;
begin
  Commit;
end;

procedure TCustomDBPersistent.DoEndUpdateOnError;
begin
  Roolback;
end;

procedure TCustomDBPersistent.DoExecuteCommands(pCommands: TCommandList);
begin
  try
    while pCommands.Count > 0 do
    begin
      Execute(pCommands.Items[0]);
    end;
    pCommands.Free;
  except
    while pCommands.Count > 0 do
    begin
      pCommands.Items[0].Free;
    end;
    pCommands.Free;
    raise;
  end;
end;

function TCustomDBPersistent.DoGetObjectStateFromSchemaControl(pObjectState: TObjectState; pSchema: TSchemaItem; pObjectMapper: TObjectMapper): TObjectState;
begin
  Result := pObjectState;
  if pObjectMapper.Schemas.Count > 1 then
  begin
    if (pObjectMapper.Schemas.DefaultSchema <> nil) and (pObjectMapper.Schemas.DefaultSchema.Name <> pSchema.Name) then
    begin
      if Assigned(FOnGetObjectStateFromSchemaControl) then
      begin
        Result := FOnGetObjectStateFromSchemaControl(pObjectState, pObjectMapper.Schemas.DefaultSchema.Name, pSchema.Name);
      end;
    end;
  end;
end;

function TCustomDBPersistent.DoSetCursor(pObjectMapper: TObjectMapper; pObjectState: TObjectState): Boolean;
begin
  Result := pObjectState <> Unknown;
  if Result then
  begin
    if pObjectMapper.Schemas.Count = 0 then
    begin
      pObjectMapper.Schemas.AddSchema('', True);
    end;
    Result := DoSetCursorWithSchema(pObjectMapper, pObjectState);
  end;
end;

procedure TCustomDBPersistent.DropPersistent(pClass: TClass);
var
  lMapper, lSchemaMapper: TObjectMapper;
  lSchema: TSchemaItem;
begin
  EnterCriticalSection;
  try
    lMapper := ExtractMapperFrom(pClass);
    try
      DoDropDetails(lMapper, pClass);
      if lMapper.Schemas.Count = 0 then
      begin
        lMapper.Schemas.AddSchema('', True);
      end;
      for lSchema in lMapper.Schemas do
      begin
        lSchemaMapper := ExtractSchemaMapper(lMapper, lSchema);
        DoDropPersistent(lSchemaMapper);
      end;
    finally
      UnlockMapper(lMapper.GetHashCode);
    end;
  finally
    LeaveCriticalSection;
  end;
end;

procedure TCustomDBPersistent.EnterCriticalSection;
begin
  if (not Assigned(FCriticalSection)) then
  begin
    FCriticalSection := TCriticalSection.Create;
  end;
  while not FCriticalSection.TryEnter do ;
end;

function TCustomDBPersistent.Execute(pCommand: TCustomCommand; pFreeOnExecute: Boolean): Boolean;
var
  lDBQuery: IDBStatement;
begin
  EnterCriticalSection;
  try
    Result := False;
    BeginUpdate(Self);
    try
      if SupportedCommand(pCommand) then
      begin
        try
          lDBQuery := ExtractDBStatement(pCommand);
          try
            Result := lDBQuery.Execute;
          finally
            lDBQuery := nil;
          end;
        except
          EndUpdateOnError(Self);
          raise;
        end;
      end;
    finally
      EndUpdate(Self);
      if pFreeOnExecute then
      begin
        pCommand.Free;
      end;
    end;
  finally
    LeaveCriticalSection;
  end;
end;

function TCustomDBPersistent.ExecuteQuery(pCommand: TCustomCommand; pFreeOnExecute: Boolean): ICursor;
var
  lDBQuery: IDBStatement;
begin
  EnterCriticalSection;
  try
    lDBQuery := ExtractDBStatement(pCommand);
    try
      Result := lDBQuery.ExecuteQuery;
    finally
      lDBQuery := nil;
      if pFreeOnExecute then
      begin
        pCommand.Free;
      end;
    end;
  finally
    LeaveCriticalSection;
  end;
end;

function TCustomDBPersistent.ExtractCursorFromMapper(pMapper: TObjectMapper; pGroupCriteria: TGroupCriteria): ICursor;
var
  lObjectCursor: TCustomObjectCursor;
  lSelect: TSelectClause;
  lSchemaMapper: TObjectMapper;
  lSchema: TSchemaItem;
begin
  if (pMapper.ObjectCursorClass <> nil) then
  begin
    lObjectCursor := TObjectCursorClass(pMapper.ObjectCursorClass).Create;
    try
      lObjectCursor.GroupCriteria := pGroupCriteria;
      Result := lObjectCursor.ExtractCursor(Self);
    finally
      lObjectCursor.Free;
    end;
  end
  else
  begin
    lSchema := nil;
    if Assigned(pGroupCriteria) and (pGroupCriteria.SchemaName <> '') then
    begin
      lSchema := pMapper.Schemas.FindSchema(pGroupCriteria.SchemaName);
    end;
    if (not Assigned(lSchema) and (pMapper.Schemas.Count > 0)) then
    begin
      lSchema := pMapper.Schemas.DefaultSchema
    end;
    lSchemaMapper := TObjectMapperToSchemaMapper.ExtractSchemaMapper(pMapper, lSchema);

    lSelect := ExtractSelectClause(lSchemaMapper, pGroupCriteria);
    try
      Result := ExtractCursorFromSelect(lSelect);
    finally
      lSchemaMapper.Free;
      lSelect.Free;
    end;
  end;
end;

function TCustomDBPersistent.ExtractCursorFromSelect(pSelect: TSelectClause): ICursor;
var
  lDBQuery: IDBStatement;
begin
  lDBQuery := ExtractDBStatement(pSelect);
  try
    Result := lDBQuery.ExecuteQuery;
  finally
    lDBQuery := nil;
  end;
end;

function TCustomDBPersistent.ExtractCursorListFromSelect(pSelect: TSelectClause): TCursorList;
var
  lDBQuery: IDBStatement;
begin
  lDBQuery := ExtractDBStatement(pSelect);
  try
    try
      Result := lDBQuery.ExtractQueryList;
    except
      Result := nil;
      raise;
    end;
  finally
    lDBQuery := nil;
  end;
end;

function TCustomDBPersistent.ExtractSchemaMapper(pMapper: TObjectMapper; pSchema: TSchemaItem): TObjectMapper;
begin
  Result := TObjectMapperToSchemaMapper.ExtractSchemaMapper(pMapper, pSchema);
end;

function TCustomDBPersistent.ExtractSelectClause(pObjectMapper: TObjectMapper; pGroupCriteria: TGroupCriteria): TSelectClause;
begin
  Result := TMapperToSelectClause.ExtractSelect(pObjectMapper, pGroupCriteria);
end;

function TCustomDBPersistent.UserColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass;
begin
  Result := TRegisterColumnTypeDBConverter.ColumnTypeToFieldCommandClass(pColumnType, TDBPersistentClass(Self.ClassType));
end;

function TCustomDBPersistent.UserFieldClassTypeToString(pClassType: TClass): String;
begin
  Result := TRegisterUserFieldCommandToString.FieldCommandClassToString(TFieldCommandClass(pClassType), TDBPersistentClass(Self.ClassType));
end;

function TCustomDBPersistent.FixIntColumnTypeForDDLColumn(pColumnType: TColumnType): TColumnType;
begin
  Result := FISQLMapper.FixIntColumnTypeForDDLColumn(pColumnType);
end;

function TCustomDBPersistent.GenerateDropTableCommand(pDDLEntity: TDDLEntity): TCommandList;
var
  lColumn: TDDLColumn;
begin
  Result := TCommandList.Create;
  Result.Add(TDropTableCommand.Create(pDDLEntity.Name, pDDLEntity.Schema));
  for lColumn in pDDLEntity.Columns do
  begin
    if lColumn.IdOptions.IsValidSequence(pDDLEntity.Schema) then
    begin
      Result.Add(TDropSequenceCommand.Create(lColumn.IdOptions.SequenceName, pDDLEntity.Schema));
    end;
  end;
end;

function TCustomDBPersistent.GetCursor(pClass: TClass; pGroupCriteria: TGroupCriteria; pMapper: TObjectMapper): ICursor;
begin
  EnterCriticalSection;
  try
    if UpdateObjectMode = uomOnPersist then
    begin
      UpdateObject(pClass, UpdateObjectOptions);
    end;
    Result := ExtractCursorFromMapper(pMapper, pGroupCriteria);
  finally
    LeaveCriticalSection;
  end;
end;

function TCustomDBPersistent.GetCursorList(pClass: TClass; pGroupCriteria: TGroupCriteria; pMapper: TObjectMapper): TCursorList;
var
  lSelect: TSelectClause;
  lObjectCursor: TCustomObjectCursor;
begin
  EnterCriticalSection;
  try
    if UpdateObjectMode = uomOnPersist then
    begin
      UpdateObject(pClass);
    end;
    if (pMapper.ObjectCursorClass = nil) then
    begin
      lSelect := ExtractSelectClause(pMapper, pGroupCriteria);
      try
        Result := ExtractCursorListFromSelect(lSelect);
      finally
        lSelect.Free;
      end;
    end
    else
    begin
      lObjectCursor := TObjectCursorClass(pMapper.ObjectCursorClass).Create;
      try
        lObjectCursor.GroupCriteria := pGroupCriteria;
        Result := lObjectCursor.ExtractCursorList(Self);
      finally
        lObjectCursor.Free;
      end;
    end;
  finally
    LeaveCriticalSection;
  end;
end;

function TCustomDBPersistent.GetCursorListWithSelect(pSelect: TCustomSelect): TCursorList;
begin
  EnterCriticalSection;
  try
    Result := ExtractCursorListFromSelect(TSelectClause(pSelect));
  finally
    LeaveCriticalSection;
  end;
end;

function TCustomDBPersistent.GetCursorWithSelect(pSelect: TCustomSelect): ICursor;
begin
  EnterCriticalSection;
  try
   Result := ExtractCursorFromSelect(TSelectClause(pSelect));
  finally
    LeaveCriticalSection;
  end;
end;

function TCustomDBPersistent.ExtractDBDDL(pName: String; pSchemaName: String; pDDLOptions: TDDLOptions): TObjectMapper;
begin
  Result := nil;
  if Assigned(FIDDLExtracter) then
  begin
    Result := FIDDLExtracter.ExtractObjectMapper(pName, pSchemaName, pDDLOptions);
  end;
end;

function TCustomDBPersistent.ExtractDBStatement(pCommand: TCustomCommand): IDBStatement;
var
  lParams: TDBParams;
begin
  Result := NewDBStatement;
  Result.SetCommand(pCommand, FISQLMapper);
  lParams := ExtractParametersFromCommand(pCommand);
  try
    Result.SetParams(lParams);
  finally
    lParams.Free;
  end;
end;

function TCustomDBPersistent.ExtractMapperFrom(pMetaClassType: TClass; pObject: TObject; pOptions: TObjectToMapperOptions): TObjectMapper;
var
  lParams: TObjectToMapperParams;
begin
  lParams := TObjectToMapperParams.Create;
  if (Assigned(pObject)) then
  begin
    lParams.ObjectInstance := pObject;
  end
  else
  begin
    lParams.MetaClassType := pMetaClassType;
  end;
  lParams.Options := pOptions;
  Result := TObjectToMapper.ObjectToMapper(lParams);
end;

function TCustomDBPersistent.ExtractDBDDL(pObjectMapper: TObjectMapper; pDDLOptions: TDDLOptions): TDDLEntity;
begin
  Result := nil;
  if Assigned(FIDDLExtracter) then
  begin
    Result := FIDDLExtracter.ExtractDDL(pObjectMapper, pObjectMapper.CurrentSchema, pDDLOptions);
  end;
end;

function TCustomDBPersistent.ExtractParametersFromCommand(pCommand: TCustomCommand): TDBParams;
begin
  if pCommand.InheritsFrom(TInsertCommand) then
  begin
    Result := ExtractParametersFromInsertCommand(TInsertCommand(pCommand));
  end
  else if pCommand.InheritsFrom(TUpdateCommand) then
  begin
    Result := ExtractParametersFromUpdateCommand(TUpdateCommand(pCommand));
  end
  else
  begin
    Result := TDBParams.Create;
  end;
end;

function TCustomDBPersistent.ExtractParametersFromInsertCommand(pCommand: TInsertCommand): TDBParams;
var
  lCounter: Integer;
  lValueArgument: TValueArgument;
begin
  Result := TDBParams.Create;
  for lCounter := 0 to pCommand.Values.Count - 1 do
  begin
    if pCommand.Values.Items[lCounter].InheritsFrom(TValueArgument) then
    begin
      lValueArgument := TValueArgument(pCommand.Values.Items[lCounter]);
      if lValueArgument.ValueType.IsBlob then
      begin
        Result.Add(TDBParam.Create(pCommand.Fields.Items[lCounter].Name, lValueArgument.ValueType.ToFieldType, lValueArgument.AsStream));
      end
      else
      begin
        Result.Add(TDBParam.Create(pCommand.Fields.Items[lCounter].Name, lValueArgument.ValueType.ToFieldType, lValueArgument.Value));
      end
    end;
  end;
end;

function TCustomDBPersistent.ExtractParametersFromUpdateCommand(pCommand: TUpdateCommand): TDBParams;
var
  lCounter: Integer;
  lValueArgument: TValueArgument;
begin
  Result := TDBParams.Create;
  for lCounter := 0 to pCommand.FieldsValues.Count - 1 do
  begin
    if pCommand.FieldsValues.Items[lCounter].Value.InheritsFrom(TValueArgument) then
    begin
      lValueArgument := TValueArgument(pCommand.FieldsValues.Items[lCounter].Value);
      if lValueArgument.ValueType.IsBlob then
      begin
        Result.Add(TDBParam.Create(pCommand.FieldsValues.Items[lCounter].Field.Name, lValueArgument.ValueType.ToFieldType, lValueArgument.AsStream));
      end
      else
      begin
        Result.Add(TDBParam.Create(pCommand.FieldsValues.Items[lCounter].Field.Name, lValueArgument.ValueType.ToFieldType, lValueArgument.Value));
      end;
    end;
  end;
end;

function TCustomDBPersistent.GetDBConnector: IDBConnector;
begin
  Result := FDBConnector;
end;

function TCustomDBPersistent.GetDDLExtracter: IDDLExtracter;
begin
  Result := FIDDLExtracter;
end;

function TCustomDBPersistent.GetLastUpdateTime(pClass: TClass; pID: Variant): TDateTime;
var
  lMapper: TObjectMapper;
  lSelect: TSelectClause;
  lCursor: ICursor;
  lColumn: TCustomColumnMapper;
begin
  lCursor := nil;
  lMapper := ExtractMapperFrom(pClass, nil, []);
  lMapper.CurrentSchema := TObjectToMapper.ExtractDefaultSchema(pClass.ClassInfo);
  lSelect := TSelectClause.CreateFromTable(lMapper.Name, lMapper.Alias);
  try
    lSelect.Field(TConsts.cLastUpdateTimeColumn, lMapper.Alias);
    lColumn := lMapper.Columns.IDColumn;
    lSelect.Where(TCriteria.CreateAsEqual(TFieldArgument.Create(lColumn.Name),
        TValueArgumentFactory.CreateAsColumnType(lColumn.ColumnType, pID)));
    lCursor := GetCursorWithSelect(lSelect);
    Result := lCursor.Values[TConsts.cLastUpdateTimeColumn].TryToDateTime;
  finally
    UnlockMapper(lMapper.GetHashCode);
    lSelect.Free;
    lCursor := nil;
  end;
end;

function TCustomDBPersistent.GetMakeForeignConstraintsWithJoinColumn: Boolean;
begin
  Result := FMakeForeignConstraintsWithJoinColumn;
end;

function TCustomDBPersistent.GetReturningValueForInsert(pCursor: ICursor; pObjectMapper: TObjectMapper): Boolean;
var
  lColumn: TCustomColumnMapper;
  lLazy: ILazy;
  lObject: TObject;
begin
  Result := not pCursor.IsEmpty;
  pCursor.First;
  pObjectMapper.Columns.StartNext;
  try
    lColumn := pObjectMapper.Columns.FindColumn(pCursor.Columns[0]);
    while Assigned(lColumn) do
    begin
      if (lColumn.ColumnType.IsSimpleType) then
      begin
        if (lColumn.IsNullable) then
        begin
          lColumn.RttiOptions.RttiFieldHelper.SetNullableVariantValue(lColumn.RttiOptions.RttiObject, pCursor.Values[0]);
        end
        else
        begin
          lColumn.ColumnValue := pCursor.Values[0];
          lColumn.RttiOptions.RttiField.SetValue(lColumn.RttiOptions.RttiObject, lColumn.AsTValue);
        end;
      end
      else
      begin
        if (lColumn.ColumnType.IsEquals(ctyJoin)) then
        begin
          if lColumn.LazyOptions.IsLazy then
          begin
            lObject := lColumn.RttiOptions.RttiFieldHelper.GetObjectValue(lColumn.RttiOptions.RttiObject);
            if (Assigned(lObject)) then
            begin
              if (Supports(lObject, ILazy, lLazy)) then
              begin
                lLazy.SetLazyID(pCursor.Values[0]);
                lLazy.SetFreeInternalValue;
              end;
            end;
          end;
        end;
      end;
      lColumn := pObjectMapper.Columns.FindNextColumn;
    end;
  finally
    pObjectMapper.Columns.ResetNext;
  end;
end;

function TCustomDBPersistent.GetSQLMapper: ISQLMapper;
begin
  Result := FISQLMapper;
end;

function TCustomDBPersistent.InTransaction: Boolean;
begin
  Result := FDBConnector.InTransaction;
end;

procedure TCustomDBPersistent.LeaveCriticalSection;
begin
  FCriticalSection.Leave;
end;

function TCustomDBPersistent.CreateLinker<T>: TSQLLinker<T>;
begin
  Result := TSQLLinker<T>.Create(Self, True);
end;

function TCustomDBPersistent.ColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass;
begin
  Result := FISQLMapper.ColumnTypeToFieldCommandClass(pColumnType);
end;

procedure TCustomDBPersistent.Reconnect;
begin
  FDBConnector.Reconnect;
end;

procedure TCustomDBPersistent.RegisterUserColumnTypeToFieldCommandClass(pColumnType: TColumnType;
  pFieldCommandClass: TFieldCommandClass);
begin
  TRegisterColumnTypeDBConverter.RegisterColumnTypeDBConverter(pColumnType, pFieldCommandClass, TDBPersistentClass(Self.ClassType));
end;

procedure TCustomDBPersistent.RegisterUserCommandClassToString(pFieldCommandClass: TFieldCommandClass; pAsString: String);
begin
  TRegisterUserFieldCommandToString.RegisterCommandClassToString(pFieldCommandClass, pAsString, TDBPersistentClass(Self.ClassType));
end;

procedure TCustomDBPersistent.Roolback;
begin
  FDBConnector.Roolback;
end;

function TCustomDBPersistent.SetCursor(pObject: TObject; pObjectState: TObjectState): TSetCursorResult;
var
  lMapper: TObjectMapper;
begin
  EnterCriticalSection;
  try
    if UpdateObjectMode = uomOnPersist then
    begin
      UpdateObject(pObject.ClassType);
    end;
    ExecuteBeforeTriggers(pObject, pObjectState);
    Result := TSetCursorResult.Create;
    lMapper := ExtractMapperFrom(pObject.ClassType, pObject);
    try
      try
        Result.ObjectMapper := lMapper;
        if not Assigned(lMapper.Columns.IDColumn) then
        begin
          raise ECannotPersistWithoutIDColumn.Create(lMapper.MetaClassType.ClassName);
        end;
        Result.Result := not lMapper.Columns.IDColumn.IsNull or (pObjectState = Inserted);
        if Result.Result then
        begin
          Result.Result := DoSetCursor(lMapper, pObjectState);
          if (Result.Result) then
          begin
            ExecuteAfterTriggers(lMapper, pObjectState);
          end;
        end;
      except
        Result.Free;
        raise;
      end;
    finally
      UnlockMapper(lMapper.GetHashCode);
    end;
  finally
    LeaveCriticalSection;
  end;
end;

procedure TCustomDBPersistent.SetMakeForeignConstraintsWithJoinColumn(const Value: Boolean);
begin
  FMakeForeignConstraintsWithJoinColumn := Value;
end;

procedure TCustomDBPersistent.StartTransaction;
begin
  if not FDBConnector.InTransaction then
  begin
    FDBConnector.StartTransaction;
  end;
end;

function TCustomDBPersistent.SupportedCommand(pCommand: TCustomCommand): Boolean;
begin
  Result := pCommand.InheritsFrom(TCustomCommand);
end;

procedure TCustomDBPersistent.UnlockMapper(pMapperHash: Integer);
begin
  TObjectToMapper.UnlockMapper(pMapperHash);
end;

procedure TCustomDBPersistent.UnregisterUserColumnTypeToFieldCommandClass(pColumnType: TColumnType;
  pFieldCommandClass: TFieldCommandClass);
begin
  TRegisterColumnTypeDBConverter.UnregisterColumnTypeDBConverter(pColumnType, pFieldCommandClass, TDBPersistentClass(Self.ClassType));
end;

procedure TCustomDBPersistent.UnregisterUserCommandClassToString(pFieldCommandClass: TFieldCommandClass; pAsString: String);
begin
  TRegisterUserFieldCommandToString.UnregisterCommandClassToString(pFieldCommandClass, pAsString, TDBPersistentClass(Self.ClassType));
end;

procedure TCustomDBPersistent.UpdatePersistent(pClass: TClass; pUpdateObjectOptions: TUpdateObjectOptions);
begin
  EnterCriticalSection;
  try
    if not FWaitingForUpdate.Contains(pClass) then
    begin
      FWaitingForUpdate.Add(pClass);
      try
        DoUpdatePersistent(pClass, pUpdateObjectOptions);
      finally
        FWaitingForUpdate.Extract(pClass);
      end;
    end;
  finally
    LeaveCriticalSection;
  end;
end;

end.
