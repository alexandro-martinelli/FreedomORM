unit AM.Freedom.SQLMappers.CustomSQLMapper;

interface

uses
  System.Generics.Collections,
  System.Classes,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLMappers.ISQLFormatter,
  AM.Freedom.SQLMappers.SQLFormatter,
  AM.Freedom.SQLCommands.Fields,
  AM.Freedom.DefaultsClassRegister,
  AM.Freedom.ObjectMapper,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.SQLCommands.TableRowCommands,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.Exceptions,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.SQLCommands.DomainCommands,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLMappers.SQLFormatSettings,
  AM.Freedom.CustomNullable;

type
  TSQLMapperClass = class of TCustomSQLMapper;

  TCustomSQLMapper = class(TInterfacedObject, ISQLMapper)
  strict private type
    TTestPersistParams = class sealed
    private
      FObjectMapper: TObjectMapper;
      FObjectState: TObjectState;
      FColumn: TCustomColumnMapper;
    public
      property Column: TCustomColumnMapper read FColumn write FColumn;
      property ObjectMapper: TObjectMapper read FObjectMapper write FObjectMapper;
      property ObjectState: TObjectState read FObjectState write FObjectState;
    end;
  strict private
    FISQLFormater: ISQLFormatter;
    FSQLFormatSettings: TSQLFormatSettings;
    FOnGetUserFieldClassTypeToString: TOnGetUserFieldClassTypeToString;
    FOnGetUserColumnTypeToFieldCommandClass: TOnGetUserColumnTypeToFieldCommandClass;
    function GenerateInsertCommand(pObjectMapper: TObjectMapper): TCustomCommand;
    procedure DoInsertColumn(pInsert: TInsertCommand; pParams: TTestPersistParams);
    procedure DoInsertFromColumnType(pInsert: TInsertCommand; pParams: TTestPersistParams);
    function GenerateUpdateCommand(pObjectMapper: TObjectMapper): TCustomCommand;
    procedure GenerateSetUpdate(pUpdateCommand: TUpdateCommand; pObjectMapper: TObjectMapper);
    function GenerateDeleteCommand(pObjectMapper: TObjectMapper): TCustomCommand;
    function CanBePersisted(pParams: TTestPersistParams): Boolean;
    function ExtractJoinColumnValue(pParams: TTestPersistParams): Variant;
    function CanBePersistedWithObjectState(pParams: TTestPersistParams): Boolean;
    function CanBePersistedWithColumnType(pParams: TTestPersistParams): Boolean;
    function CanBePersistedWithBlob(pParams: TTestPersistParams): Boolean;
    function CanBePersistedWithNoBlob(pParams: TTestPersistParams): Boolean;
    function IsDiferrentValue(pParams: TTestPersistParams; pOldValue: Variant): Boolean;
    function CompareBytes(pCurrent, pInitial: TStream): Boolean;
    function GetCriteriaFromIDColumn(pObjectMapper: TObjectMapper; pObjectState: TobjectState): TCriteria;
    function CreateValueArgumentFromColumnMapper(pColumn: TCustomColumnMapper; pValue: Variant): TValueArgument;
    function GetSQLFormatSettings: TSQLFormatSettings;
    function CurrentDateTimeExpression: String;
    function CurrentDateExpression: String;
    function CurrentTimeExpression: String;
    function GetOnGetUserFieldClassTypeToString: TOnGetUserFieldClassTypeToString;
    procedure SetOnGetUserFieldClassTypeToString(pValue: TOnGetUserFieldClassTypeToString);
    function DoGetUserFieldCommandClassToString(pFieldCommandClass: TFieldCommandClass): String;
    function ColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass;
    function GetUserColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass;
    function GetOnGetUserColumnTypeToFieldCommandClass: TOnGetUserColumnTypeToFieldCommandClass;
    procedure SetOnGetUserColumnTypeToFieldCommandClass(pValue: TOnGetUserColumnTypeToFieldCommandClass);
  strict protected
    function GetSQLFormater: ISQLFormatter;
    function GetKeyWords: TStrings; virtual;
    procedure FormatSQLText(var pSQL: string); virtual;
    procedure IncludeLimitRows(var pSQL: string; const pLimitRows: Integer); virtual;
    procedure IncludeReturningClause(var pSQLText: string; const pFieldNames: TList<String>); virtual;
    function IncludeDirectives(pFrom: string): string; virtual;
    function FieldClassTypeToString(pClassType: TClass): string;
    function DoGetFieldClassTypeToString(pClassType: TClass): string; virtual;
    function TableFieldCommandToString(pCommandType: TCommandType): String; virtual;
    function TableCommandToString(pCommandType: TCommandType): String; virtual;
    function TableRowCommandToString(pCommandType: TCommandType): String; virtual;
    function TableConstraintCommandToString(pCommandType: TCommandType): String; virtual;
    function GetAlterTableSingleCommand: Boolean; virtual;
    function DescriptionText(pDescriptionTextType: TDescriptionTextType): string; virtual;
    function GenerateCommand(pObjectMapper: TObjectMapper; pObjectState: TObjectState): TCustomCommand; virtual;
    function SequenceSQL(pSequenceName: string; pSchemaName: string; pRestartWith: Integer; pCommandType: TCommandType): string; virtual;
    procedure WriteSQLFormatSettings(pSQLFormatSettings: TSQLFormatSettings); virtual;
    function DoGetCurrentDateExpression: String; virtual;
    function DoGetCurrentDateTimeExpression: String; virtual;
    function DoGetCurrentTimeExpression: String; virtual;
    function BooleanToDefaultExpression(pBooleanValue: Boolean): Variant; virtual;
    function GetColumnTypeFromEnumType(pEnumType: TEnumerationType): TColumnType; virtual;
    function AdjustNameLength(pName: string): string; virtual;
    function FormatFieldCommand(pFieldCommandText: String): String; virtual;
    function FormatLikeExpression(pExpression: String): String; virtual;
    function AlterFieldCommandParams: TAlterFieldCommandParams; virtual;
    function GetAlwaysAlterDataTypeOnChangeProperties: Boolean; virtual;
    function GetSQLForCreateDomain: String; virtual;
    function GetSQLForDropDomain: String; virtual;
    procedure AdjustIndexCommand(lIndexCommand: TCustomCommand); virtual;
    function GenerateDropIndexCommand(lIndexCommand: TCustomCommand): String; virtual;
    function FixIntColumnTypeForDDLColumn(pColumnType: TColumnType): TColumnType; virtual;
    function AllowedColumnTypeConversion(pSourceColumnType, pDestinyColumnType: TColumnType): Boolean; virtual;
    function DoGetColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass; virtual;
  public
    constructor Create(pAsDefault: Boolean = False; pISQLFormater: ISQLFormatter = nil); virtual;
    destructor Destroy; override;
    property AlterTableSingleCommand: Boolean read GetAlterTableSingleCommand;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.Variants,
  System.Rtti,
  AM.Freedom.Helper.CommandType,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.Helper.Variant, AM.Freedom.NullableCompare,
  AM.Freedom.ObjectMapper.ColumnValueItem,
  AM.Freedom.Helper.RttiField,
  AM.Freedom.ILazy,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.Consts,
  AM.Freedom.ColumnTypeDBConverter,
  AM.Freedom.RegisterUserFieldCommandToString;

{ TCustomSQLMapper }

function TCustomSQLMapper.TableConstraintCommandToString(pCommandType: TCommandType): String;
begin
  Result := pCommandType.ToString;
  if pCommandType <> TCommandType.Add then
  begin
    Result := Result + ' constraint';
  end;
end;

procedure TCustomSQLMapper.AdjustIndexCommand(lIndexCommand: TCustomCommand);
begin
  raise EInvalidMethodCallOnClass.Create('AdjustIndexCommand', ClassName);
end;

function TCustomSQLMapper.AdjustNameLength(pName: string): string;
const
  cByteLength = 255;
begin
  Result := Copy(pName, 1, cByteLength);
end;

function TCustomSQLMapper.AllowedColumnTypeConversion(pSourceColumnType, pDestinyColumnType: TColumnType): Boolean;
begin
  case pSourceColumnType  of
    ctyByte: Result := pDestinyColumnType in [ctyByte, ctySmallint, ctyInteger, ctyInt64, ctySingle, ctyDouble, ctyExtended, ctyCurrency, ctyString];
    ctySmallint: Result := pDestinyColumnType in [ctySmallint, ctyInteger, ctyInt64, ctySingle, ctyDouble, ctyExtended, ctyCurrency, ctyString];
    ctyInteger: Result := pDestinyColumnType in [ctyInteger, ctyInt64, ctySingle, ctyDouble, ctyExtended, ctyCurrency, ctyString];
    ctyInt64: Result := pDestinyColumnType in [ctyInt64, ctySingle, ctyDouble, ctyExtended, ctyCurrency, ctyString];
    ctyEnumerator: Result := pDestinyColumnType in [ctyByte, ctySmallint, ctyInteger, ctyInt64, ctyString];
    ctyChar: Result := pDestinyColumnType in [ctyChar, ctyString];
    ctyString: Result := pDestinyColumnType in [ctyString];
    ctySingle: Result := pDestinyColumnType in [ctySingle, ctyDouble, ctyExtended, ctyCurrency, ctyString];
    ctyDouble: Result := pDestinyColumnType in [ctyDouble, ctyExtended, ctyCurrency, ctyString];
    ctyExtended: Result := pDestinyColumnType in [ctyExtended, ctyCurrency, ctyString];
    ctyCurrency: Result := pDestinyColumnType in [ctyCurrency, ctyString];
    ctyDate: Result := pDestinyColumnType in [ctyDate, ctyDateTime, ctyString];
    ctyTime: Result := pDestinyColumnType in [ctyTime, ctyDateTime, ctyString];
    ctyDateTime: Result := pDestinyColumnType in [ctyDateTime, ctyString];
    ctyBoolean: Result := pDestinyColumnType in [ctyBoolean, ctyString];
    else
     Result := False;
  end;
end;

function TCustomSQLMapper.AlterFieldCommandParams: TAlterFieldCommandParams;
begin
  Result := [];
end;

function TCustomSQLMapper.BooleanToDefaultExpression(pBooleanValue: Boolean): Variant;
begin
  raise EInvalidMethodCallOnClass.Create('BooleanToDefaultExpression', ClassName);
end;

function TCustomSQLMapper.CanBePersisted(pParams: TTestPersistParams): Boolean;
begin
  Result := pParams.Column.ColumnType <> ctyDetail;
  if Result then
  begin
    Result := (pParams.Column.ColumnType <> ctyJoin) or ExtractJoinColumnValue(pParams).IsNotNull;
    if Result then
    begin
      Result := CanBePersistedWithObjectState(pParams);
      if Result then
      begin
        Result := CanBePersistedWithColumnType(pParams);
      end;
    end;
  end;
end;

function TCustomSQLMapper.CanBePersistedWithBlob(pParams: TTestPersistParams): Boolean;
var
  lCurrentStream, lInitialStream: TStream;
  lColumnValueItem: TColumnValueItem;
  lDestroyStream: Boolean;
begin
  lInitialStream := nil;
  lCurrentStream := pParams.Column.CurrentStream;
  lDestroyStream := False;
  if (pParams.ObjectState = Clean) then
  begin
    lColumnValueItem := pParams.ObjectMapper.InitialValues.FindColumnValue(pParams.Column.Name, pParams.Column.ColumnType);
    if (Assigned(lColumnValueItem)) then
    begin
      lInitialStream := lColumnValueItem.StreamColumnValue
    end;
  end;
  if (not Assigned(lInitialStream)) then
  begin
    lInitialStream := TMemoryStream.Create;
    lInitialStream.Position := 0;
    lDestroyStream := True;
  end;
  try
    Result := lCurrentStream.Size <> lInitialStream.Size;
    if not Result then
    begin
      Result := CompareBytes(lCurrentStream, lInitialStream);
    end;
  finally
    lCurrentStream.Free;
    if (lDestroyStream) then
    begin
      lInitialStream.Free;
    end;
  end;
end;

function TCustomSQLMapper.CanBePersistedWithColumnType(pParams: TTestPersistParams): Boolean;
begin
  Result := True;
  if (pParams.ObjectMapper.CurrentSchema = '') or
     ((pParams.Column.IdOptions.Schemas.FindSchema(pParams.ObjectMapper.CurrentSchema) <> nil) or
     (pParams.Column.IdOptions.Schemas.Count = 0)) then
  begin
    Result := not pParams.Column.IdOptions.IsId or (pParams.Column.IdOptions.IdOption <> Identity);
  end;
  if Result then
  begin
    Result := not pParams.Column.IdOptions.IsId or ((pParams.Column.IdOptions.IdOption <> Sequence) or (pParams.ObjectState = TObjectState.Inserted));
    if (Result) and (not pParams.Column.IdOptions.IsId or (pParams.Column.IdOptions.IdOption <> Sequence)) then
    begin
      if pParams.Column.ColumnType.IsBlob then
      begin
        Result := CanBePersistedWithBlob(pParams);
      end
      else
      begin
        Result := CanBePersistedWithNoBlob(pParams);
      end;
    end;
  end;
end;

function TCustomSQLMapper.CanBePersistedWithNoBlob(pParams: TTestPersistParams): Boolean;
var
  lColumnValueItem: TColumnValueItem;
  lColumn: TCustomColumnMapper;
begin
  if pParams.ObjectState = Inserted then
  begin
    Result := pParams.Column.IsNullable or pParams.Column.CurrentValue.IsNotNull or
      (pParams.Column.ColumnType.IsEquals(ctyJoin) and ExtractJoinColumnValue(pParams).IsNotNull);
  end
  else
  begin
    lColumnValueItem := pParams.ObjectMapper.InitialValues.FindColumnValue(pParams.Column.Name, pParams.Column.ColumnType);
    Result := True;
    if (Assigned(lColumnValueItem)) then
    begin
      if (pParams.Column.ColumnType = ctyJoin) then
      begin
        lColumn := pParams.ObjectMapper.Columns.FindColumn(pParams.Column.Name, ctyUnknow, [ctyJoin]);
        if (Assigned(lColumn)) then
        begin
          Result := False;
        end;
      end;
      if (Result) then
      begin
        Result := IsDiferrentValue(pParams, lColumnValueItem.ColumnValue);
      end;
    end;
  end;
end;

function TCustomSQLMapper.CanBePersistedWithObjectState(pParams: TTestPersistParams): Boolean;
begin
  case pParams.ObjectState of
    Clean:
      Result := not (NoUpdate in pParams.Column.ColumnOptions);
    Inserted:
      Result := not (NoInsert in pParams.Column.ColumnOptions);
    else
      Result := False;
  end;
end;

function TCustomSQLMapper.CompareBytes(pCurrent, pInitial: TStream): Boolean;
var
  lCurrentBuffer, lInitialBuffer: TBytes;
  lCounter: Integer;

  function GetByteLength(pStream: TStream): Integer;
  const
    MaxBufSize = $F000;
  begin
    pStream.Position := 0;
    Result := pStream.Size;
    if Result > MaxBufSize then
    begin
      Result := MaxBufSize
    end;
  end;
begin
  SetLength(lCurrentBuffer, GetByteLength(pCurrent));
  SetLength(lInitialBuffer, GetByteLength(pInitial));
  pCurrent.Read(lCurrentBuffer, Length(lCurrentBuffer));
  pInitial.Read(lInitialBuffer, Length(lInitialBuffer));
  Result := Length(lCurrentBuffer) <> length(lInitialBuffer);
  if Result then
  begin
    for lCounter := Low(lCurrentBuffer) to High(lCurrentBuffer) do
    begin
      Result := lCurrentBuffer[lCounter] <> lInitialBuffer[lCounter];
      if Result then
      begin
        Break;
      end;
    end;
  end;
end;

constructor TCustomSQLMapper.Create(pAsDefault: Boolean; pISQLFormater: ISQLFormatter);
begin
  FISQLFormater := pISQLFormater;
  FSQLFormatSettings := TSQLFormatSettings.Create;
  WriteSQLFormatSettings(FSQLFormatSettings);
  if pAsDefault then
  begin
    TDefaultsClassRegister.DefaultSQLMapper := Self;
  end;
end;

function TCustomSQLMapper.CreateValueArgumentFromColumnMapper(pColumn: TCustomColumnMapper; pValue: Variant): TValueArgument;
begin
  case pColumn.ColumnType of
    ctySmallint: Result := TValueArgument.CreateAsSmallint(pValue.TryToInt);
    ctyInteger: Result := TValueArgument.CreateAsInteger(pValue.TryToInt);
    ctyInt64: Result := TValueArgument.CreateAsInt64(pValue.TryToInt64);
    ctyString: Result := TValueArgument.CreateAsString(pValue.ToString);
    ctyDouble, ctySingle: Result := TValueArgument.CreateAsDouble(pValue.TryToDouble);
    ctyCurrency: Result := TValueArgument.CreateAsCurrency(pValue.TryToCurrency);
    ctyExtended: Result := TValueArgument.CreateAsExtended(pValue.TryToExtended);
    ctyDate: Result := TValueArgument.CreateAsDate(pValue.TryToDate);
    ctyTime: Result := TValueArgument.CreateAsTime(pValue.TryToTime);
    ctyDateTime: Result := TValueArgument.CreateAsDateTime(pValue.TryToDateTime);
    ctyEnumerator, ctyByte: Result := TValueArgument.CreateAsByte(pValue.TryToByte);
    else
      raise EInvalidArgumentValue.Create(pColumn.ColumnType.ToString);
  end;
end;

function TCustomSQLMapper.CurrentDateExpression: String;
begin
  Result := DoGetCurrentDateExpression;
end;

function TCustomSQLMapper.CurrentDateTimeExpression: String;
begin
  Result := DoGetCurrentDateTimeExpression;
end;

function TCustomSQLMapper.CurrentTimeExpression: String;
begin
  Result := DoGetCurrentTimeExpression;
end;

function TCustomSQLMapper.TableFieldCommandToString(pCommandType: TCommandType): String;
begin
  Result := pCommandType.ToString;
  if pCommandType in [Alter, Drop] then
  begin
    Result := Result + ' column';
  end;
end;

function TCustomSQLMapper.DescriptionText(pDescriptionTextType: TDescriptionTextType): string;
begin
  raise EInvalidMethodCallOnClass.Create('DescriptionText', ClassName);
end;

destructor TCustomSQLMapper.Destroy;
begin
  if Assigned(FISQLFormater) then
  begin
    FISQLFormater := nil;
  end;
  if TDefaultsClassRegister.DefaultSQLMapper = ISQLMapper(Self) then
  begin
    TDefaultsClassRegister.DefaultSQLMapper := nil;
  end;
  FSQLFormatSettings.Free;
  inherited;
end;

function TCustomSQLMapper.DoGetColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass;
begin
  Result := nil;
end;

function TCustomSQLMapper.DoGetCurrentDateExpression: String;
begin
  raise EInvalidMethodCallOnClass.Create('DoGetCurrentDateExpression', ClassName);
end;

function TCustomSQLMapper.DoGetCurrentDateTimeExpression: String;
begin
  raise EInvalidMethodCallOnClass.Create('DoGetCurrentDateTimeExpression', ClassName);
end;

function TCustomSQLMapper.DoGetCurrentTimeExpression: String;
begin
  raise EInvalidMethodCallOnClass.Create('DoGetCurrentTimeExpression', ClassName);
end;

function TCustomSQLMapper.DoGetFieldClassTypeToString(pClassType: TClass): string;
begin
  Result := '';
end;

function TCustomSQLMapper.DoGetUserFieldCommandClassToString(pFieldCommandClass: TFieldCommandClass): String;
begin
  if (Assigned(FOnGetUserFieldClassTypeToString)) then
  begin
    Result := FOnGetUserFieldClassTypeToString(pFieldCommandClass);
  end;
end;

procedure TCustomSQLMapper.DoInsertColumn(pInsert: TInsertCommand; pParams: TTestPersistParams);
begin
  if pParams.Column.IdOptions.IsValidSequence(pInsert.Schema) then
  begin
    pInsert.FieldValue(pParams.Column.Name, SequenceSQL(pParams.Column.IdOptions.SequenceName, pInsert.Schema, 0, TCommandType.Select));
  end else
  begin
    DoInsertFromColumnType(pInsert, pParams);
  end;
end;

procedure TCustomSQLMapper.DoInsertFromColumnType(pInsert: TInsertCommand; pParams: TTestPersistParams);
var
  lColumnValue: Variant;
  lStream: TStream;
  lColumnType: TColumnType;
begin
  if pParams.Column.ColumnType.IsBlob then
  begin
    lStream := pParams.Column.CurrentStream;
    pInsert.Field(pParams.Column.Name).Values.Add(TValueArgument.CreateAsStream(lStream, pParams.Column.ColumnType));
  end
  else
  begin
    lColumnValue := pParams.Column.CurrentValue;
    lColumnType := pParams.Column.ColumnType;
    case pParams.Column.ColumnType of
      ctyChar, ctyString:
        if pParams.Column.Size > 0 then
        begin
          lColumnValue := Copy(lColumnValue, 1, pParams.Column.Size);
        end;
      ctyJoin:
        begin
          lColumnValue := ExtractJoinColumnValue(pParams);
          lColumnType := TReferenceColumnMapper(pParams.Column).RefColumnType;
        end;
      ctyEnumerator:
        lColumnType := GetColumnTypeFromEnumType(TEnumerationColumnMapper(pParams.Column).EnumType);
      ctyBoolean:
        begin
          lColumnType := TBooleanColumnMapper(pParams.Column).InternalColumnType;
        end;
    end;
    pInsert.FieldColumnType(lColumnType, pParams.Column.Name, lColumnValue);
  end;
end;

function TCustomSQLMapper.ExtractJoinColumnValue(pParams: TTestPersistParams): Variant;
var
  lRealColumn: TCustomColumnMapper;
  lObject, lRealObject: TObject;
  lLazy: ILazy;
  lParams: TObjectToMapperParams;
  lMapper: TObjectMapper;
begin
  lRealObject := nil;
  Result := Null;
  if pParams.Column.RttiOptions.RttiField.FieldType.IsInstance and pParams.Column.InheritsFrom(TReferenceColumnMapper) then
  begin
    lRealColumn := pParams.ObjectMapper.Columns.FindColumn(pParams.Column.Name, TReferenceColumnMapper(pParams.Column).RefColumnType);
    if (Assigned(lRealColumn)) then
    begin
      Result := lRealColumn.CurrentValue;
    end
    else
    begin
      lObject := pParams.Column.RttiOptions.RttiFieldHelper.GetObjectValue(pParams.Column.RttiOptions.RttiObject);
      if (Assigned(lObject)) then
      begin
        if (Supports(lObject, ILazy, lLazy)) then
        begin
          if not lLazy.Loaded and (pParams.ObjectState <> Clean) then
          begin
            lLazy.Load;
          end;
          lRealObject := lLazy.GetObject;
        end
        else
        begin
          lRealObject := lObject;
        end;
      end;
      if (Assigned(lRealObject)) then
      begin
        lParams := TObjectToMapperParams.Create;
        lParams.ObjectInstance := lRealObject;
        lMapper := TObjectToMapper.ObjectToMapper(lParams);
        try
          lRealColumn := lMapper.Columns.FindColumn(TReferenceColumnMapper(pParams.Column).RefColumnName, TReferenceColumnMapper(pParams.Column).RefColumnType);
          if (Assigned(lRealColumn)) then
          begin
            Result := lRealColumn.CurrentValue;
          end;
        finally
          TObjectToMapper.UnlockMapper(lMapper.GetHashCode);
        end;
      end;
    end;
  end;
end;

function TCustomSQLMapper.FieldClassTypeToString(pClassType: TClass): string;
begin
  Result := DoGetUserFieldCommandClassToString(TFieldCommandClass(pClassType));
  if (Result = '') then
  begin
    Result := DoGetFieldClassTypeToString(pClassType);
    if (Result = '') then
    begin
      if pClassType = TVarcharFieldCommand then
      begin
        Result := 'varchar';
      end
      else if pClassType = TCharFieldCommand then
      begin
        Result := 'char';
      end
      else if pClassType = TSmallintFieldCommand then
      begin
        Result := 'smallint';
      end
      else if pClassType = TIntegerFieldCommand then
      begin
        Result := 'integer';
      end
      else if pClassType = TInt64FieldCommand then
      begin
        Result := 'bigint';
      end
      else if pClassType = TNumericFieldCommand then
      begin
        Result := 'numeric';
      end
      else if pClassType = TDateFieldCommand then
      begin
        Result := 'date';
      end
      else if pClassType = TTimeFieldCommand then
      begin
        Result := 'time';
      end
      else
      begin
        raise EInvalidFieldCommandForPersistentClass.Create(pClassType.ClassName, ClassName);
      end;
    end;
  end;
end;

function TCustomSQLMapper.FixIntColumnTypeForDDLColumn(pColumnType: TColumnType): TColumnType;
begin
  Result := pColumnType;
end;

function TCustomSQLMapper.FormatFieldCommand(pFieldCommandText: String): String;
begin
  Result := pFieldCommandText;
end;

function TCustomSQLMapper.FormatLikeExpression(pExpression: String): String;
begin
  Result := pExpression;
end;

procedure TCustomSQLMapper.FormatSQLText(var pSQL: string);
begin
  if GetSQLFormater <> nil then
  begin
    pSQL := GetSQLFormater.FormatSQLText(pSQL, GetKeyWords)
  end;
end;

function TCustomSQLMapper.GenerateCommand(pObjectMapper: TObjectMapper; pObjectState: TObjectState): TCustomCommand;
begin
  case pObjectState of
    Clean:
      Result := GenerateUpdateCommand(pObjectMapper);
    Inserted:
      Result := GenerateInsertCommand(pObjectMapper);
    Deleted:
      Result := GenerateDeleteCommand(pObjectMapper);
  else
    Result := nil;
  end;
end;

function TCustomSQLMapper.GenerateDeleteCommand(pObjectMapper: TObjectMapper): TCustomCommand;
var
  lDelete: TDeleteCommand;
begin
  lDelete := TDeleteCommand.Create;
  try
    lDelete.From(pObjectMapper.Name);
    lDelete.Schema := pObjectMapper.CurrentSchema;
    lDelete.Where(GetCriteriaFromIDColumn(pObjectMapper, Deleted));
  except
    FreeAndNil(lDelete);
    raise;
  end;
  Result := lDelete;
end;

function TCustomSQLMapper.GenerateDropIndexCommand(lIndexCommand: TCustomCommand): String;
begin
  raise EInvalidMethodCallOnClass.Create('GenerateDropIndexCommand', ClassName);
end;

function TCustomSQLMapper.GenerateInsertCommand(pObjectMapper: TObjectMapper): TCustomCommand;
var
  lInsert: TInsertCommand;
  lColumn: TCustomColumnMapper;
  lParams: TTestPersistParams;

  procedure DoAddColumn(pColumn: TCustomCOlumnMapper);
  begin
    lParams.Column := lColumn;
    if (lInsert.FindField(lColumn.Name) = nil) and CanBePersisted(lParams) then
    begin
      DoInsertColumn(lInsert, lParams);
    end;
  end;
begin
  lInsert := TInsertCommand.Create;
  lInsert.IntoTable(pObjectMapper.Name);
  lInsert.ReturningField(pObjectMapper.Columns.IDColumn.Name);
  lInsert.Schema := pObjectMapper.CurrentSchema;
  lParams := TTestPersistParams.Create;
  try
    lParams.ObjectMapper := pObjectMapper;
    lParams.ObjectState := TObjectState.Inserted;
    for lColumn in pObjectMapper.Columns.SimpleColumns do
    begin
      DoAddColumn(lColumn);
    end;
    for lColumn in pObjectMapper.Columns.OrdinalColumns do
    begin
      DoAddColumn(lColumn);
    end;
    for lColumn in pObjectMapper.Columns.BlobColumns do
    begin
      DoAddColumn(lColumn);
    end;
    for lColumn in pObjectMapper.Columns.JoinColumns do
    begin
      DoAddColumn(lColumn);
    end;
  finally
    lParams.Free;
  end;
  Result := lInsert;
end;

procedure TCustomSQLMapper.GenerateSetUpdate(pUpdateCommand: TUpdateCommand; pObjectMapper: TObjectMapper);
var
  lColumn: TCustomColumnMapper;
  lStream: TStream;
  lParams: TTestPersistParams;

  procedure DoAddColumn(pColumn: TCustomColumnMapper);
  begin
    lParams.Column := pColumn;
    if not pColumn.IsExtension and (pUpdateCommand.FindField(pColumn.Name) = nil) and CanBePersisted(lParams) then
    begin
      if not pColumn.ColumnType.IsBlob then
      begin
        pUpdateCommand.FieldColumnType(pColumn.ColumnType, pColumn.Name, pColumn.CurrentValue);
      end
      else
      begin
        lStream := pColumn.CurrentStream;
        pUpdateCommand.FieldValue<TStream>(pColumn.Name, lStream, '', pColumn.ColumnType);
      end;
    end;
  end;
begin
  lParams := TTestPersistParams.Create;
  try
    lParams.ObjectMapper := pObjectMapper;
    lParams.ObjectState := TObjectState.Clean;
    for lColumn in pObjectMapper.Columns.SimpleColumns do
    begin
      DoAddColumn(lColumn);
    end;
    for lColumn in pObjectMapper.Columns.OrdinalColumns do
    begin
      DoAddColumn(lColumn);
    end;
    for lColumn in pObjectMapper.Columns.BlobColumns do
    begin
      DoAddColumn(lColumn);
    end;
    for lColumn in pObjectMapper.Columns.JoinColumns do
    begin
      DoAddColumn(lColumn);
    end;
  finally
    lParams.Free;
  end;
end;

function TCustomSQLMapper.GenerateUpdateCommand(pObjectMapper: TObjectMapper): TCustomCommand;
var
  lUpdate: TUpdateCommand;
begin
  lUpdate := TUpdateCommand.Create;
  try
    lUpdate.TableName(pObjectMapper.Name);
    lUpdate.Schema := pObjectMapper.CurrentSchema;
    GenerateSetUpdate(lUpdate, pObjectMapper);
    {$IFDEF USEPOOL}
    if (lUpdate.FieldsValues.Count = 1) and (lUpdate.FieldsValues.First.Field.Name = TConsts.cLastUpdateTimeColumn) then
    begin
      lUpdate.FieldsValues.Clear;
    end;
    {$IFEND}
    if lUpdate.FieldsValues.Count = 0 then
    begin
      FreeAndNil(lUpdate);
    end
    else
    begin
      lUpdate.Where(GetCriteriaFromIDColumn(pObjectMapper, Clean));
    end;
  except
    FreeAndNil(lUpdate);
    raise;
  end;
  Result := lUpdate;
end;

function TCustomSQLMapper.GetAlterTableSingleCommand: Boolean;
begin
  Result := False;
end;

function TCustomSQLMapper.GetAlwaysAlterDataTypeOnChangeProperties: Boolean;
begin
  Result := False;
end;

function TCustomSQLMapper.GetColumnTypeFromEnumType(pEnumType: TEnumerationType): TColumnType;
begin
  case pEnumType of
    emChar:
      Result := ctyString;
    else
      Result := ctyByte;
  end;
end;

function TCustomSQLMapper.GetCriteriaFromIDColumn(pObjectMapper: TObjectMapper; pObjectState: TobjectState): TCriteria;
var
  lIdValue, lInitialValue: Variant;
  lColumnValueItem: TColumnValueItem;
begin
  lIdValue := pObjectMapper.Columns.IDColumn.CurrentValue;
  lInitialValue := Null;
  if (pObjectState <> Inserted) then
  begin
    lColumnValueItem := pObjectMapper.InitialValues.FindColumnValue(pObjectMapper.Columns.IDColumn.Name, pObjectMapper.Columns.IDColumn.ColumnType);
    if (Assigned(lColumnValueItem)) then
    begin
      lInitialValue := lColumnValueItem.ColumnValue;
      if not VarSameValue(lIdValue, lInitialValue) then
      begin
        lIdValue := lInitialValue;
      end;
    end;
  end;
  if (VarIsNull(lIdValue)) then
  begin
    raise ECannotPersistWithoutIDColumnValue.Create(pObjectMapper.RttiOptions.RttiObject.ClassName, pObjectMapper.Columns.IDColumn.Name);
  end;
  Result := TCriteria.CreateAsEqual(
      TFieldArgument.Create(pObjectMapper.Columns.IDColumn.Name),
        CreateValueArgumentFromColumnMapper(pObjectMapper.Columns.IDColumn, lIdValue));
end;

function TCustomSQLMapper.GetKeyWords: TStrings;
begin
  Result := TStringList.Create;
  Result.Delimiter := ';';
  Result.StrictDelimiter := True;
  Result.DelimitedText := 'BEGIN;SELECT;INSERT;DELETE;UPDATE;WHERE;CASE;OF;FOR;DO;ON;ELSE;REFERENCES;SEQUENCE;' +
    'WHEN;SET;VALUES;OR;AND;NOT;IN;LIKE;BETWEEN;FROM;CREATE;ALTER;TABLE;DROP;EXISTS;DEFAULT;TYPE;' +
    'ADD;LEFT;INNER;OUTER;CROSS;NATURAL;JOIN;GROUP;BY;ORDER;NULL;IS;AS;THEN;NUMERIC;COLUMN;VIEW;' +
    'PRIMARY;KEY;CONSTRAINT;FOREIGN;HAVING;VARCHAR;INTEGER;SMALLINT;DATE;CASCADE;ACTION;TRIGGER;' +
    'TIME;CAST;CHAR;INTO;END;UPPER;LOWER;COALESCE;SUM;MIN;MAX;AVG;COUNT;BIGINT;FLOAT;TIMESTAMP;VALUE;CHECK;IDENTITY;' +
    'DOUBLE;PRECISION;DECIMAL;MINUTE;SECOND;HOUR;DAY;MONTH,YEAR;ASC;DESC;ASCENDING;DESCENDING;INDEX';
end;

function TCustomSQLMapper.GetOnGetUserFieldClassTypeToString: TOnGetUserFieldClassTypeToString;
begin
  Result := FOnGetUserFieldClassTypeToString;
end;

function TCustomSQLMapper.GetOnGetUserColumnTypeToFieldCommandClass: TOnGetUserColumnTypeToFieldCommandClass;
begin
  Result := FOnGetUserColumnTypeToFieldCommandClass;
end;

function TCustomSQLMapper.GetSQLForCreateDomain: String;
begin
  Result := 'create domain %s %s %s';
end;

function TCustomSQLMapper.GetSQLForDropDomain: String;
begin
  Result := 'drop domain %s';
end;

function TCustomSQLMapper.GetSQLFormater: ISQLFormatter;
begin
  Result := FISQLFormater;
  if not Assigned(Result) then
  begin
    Result := TDefaultsClassRegister.DefaultSQLFormatter;
  end;
end;

function TCustomSQLMapper.GetSQLFormatSettings: TSQLFormatSettings;
begin
  Result := FSQLFormatSettings;
end;

function TCustomSQLMapper.GetUserColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass;
begin
  Result := nil;
  if (Assigned(FOnGetUserColumnTypeToFieldCommandClass)) then
  begin
    Result := FOnGetUserColumnTypeToFieldCommandClass(pColumnType);
  end;
end;

function TCustomSQLMapper.IncludeDirectives(pFrom: string): string;
begin
  Result := pFrom;
end;

procedure TCustomSQLMapper.IncludeLimitRows(var pSQL: string; const pLimitRows: Integer);
begin
  raise EInvalidMethodCallOnClass.Create('IncludeLimitRows', ClassName);
end;

procedure TCustomSQLMapper.IncludeReturningClause(var pSQLText: string; const pFieldNames: TList<String>);
begin
  raise EInvalidMethodCallOnClass.Create('IncludeReturningClause', ClassName);
end;

function TCustomSQLMapper.IsDiferrentValue(pParams: TTestPersistParams; pOldValue: Variant): Boolean;
var
  lCurrentValue: Variant;
begin
  lCurrentValue := pParams.Column.CurrentValue;
  if (pParams.Column.ColumnType.IsEquals(ctyJoin)) then
  begin
    lCurrentValue := ExtractJoinColumnValue(pParams);
  end;
  lCurrentValue := TNullableCompare.NullIfNullable(lCurrentValue, pParams.Column);
  Result := VarIsNull(pOldValue) <> VarIsNull(lCurrentValue);
  if (not Result) then
  begin
    Result := True;
    if (not VarIsNull(pOldValue) and not VarIsNull(lCurrentValue)) then
    begin
      Result := not VarSameValue(pOldValue, lCurrentValue);
    end
    else if (VarIsNull(pOldValue) and VarIsNull(lCurrentValue)) then
    begin
      Result := False;
    end;
  end;
end;

function TCustomSQLMapper.ColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass;
begin
  Result := GetUserColumnTypeToFieldCommandClass(pColumnType);
  if (Result = nil) then
  begin
    Result := DoGetColumnTypeToFieldCommandClass(pColumnType);
    if (Result = nil) then
    begin
      case pColumnType of
        ctyByte, ctySmallint, ctyEnumerator:
          Result := TSmallintFieldCommand;
        ctyInteger:
          Result := TIntegerFieldCommand;
        ctyInt64:
          Result := TInt64FieldCommand;
        ctyChar:
          Result := TCharFieldCommand;
        ctyString:
          Result := TVarcharFieldCommand;
        ctyDate:
          Result := TDateFieldCommand;
        ctyTime:
          Result := TTimeFieldCommand;
        ctyDateTime:
          Result := TDateTimeFieldCommand;
        ctyBlob:
          Result := TBlobFieldCommand;
        ctyMemo:
          Result := TMemoFieldCommand;
        ctySingle, ctyDouble, ctyExtended, ctyCurrency:
          Result := TFloatFieldCommand;
        ctyBoolean:
          Result := TBooleanFieldCommand;
        else
          raise EInvalidFieldColumnTypeDBConversion.Create(pColumnType.ToString, ClassName);
      end;
    end;
  end;
end;

function TCustomSQLMapper.SequenceSQL(pSequenceName: string; pSchemaName: string; pRestartWith: Integer; pCommandType: TCommandType): string;
  function CompleteSequenceName: String;
  begin
    Result := ifthen(pSchemaName <> '', pSchemaName + '.') + pSequenceName;
  end;
begin
  case pCommandType of
    Alter: Result := Format('alter sequence %s restart with %d', [CompleteSequenceName, pRestartWith]);
    Drop: Result := Format('drop sequence %s', [CompleteSequenceName]);
    TCommandType.Create: Result := Format('create sequence %s', [CompleteSequenceName]);
    else
      raise EInvalidMethodCallOnClass.Create('SequenceSQL', ClassName);
  end;
end;

procedure TCustomSQLMapper.SetOnGetUserFieldClassTypeToString(pValue: TOnGetUserFieldClassTypeToString);
begin
  FOnGetUserFieldClassTypeToString := pValue;
end;

procedure TCustomSQLMapper.SetOnGetUserColumnTypeToFieldCommandClass(pValue: TOnGetUserColumnTypeToFieldCommandClass);
begin
  FOnGetUserColumnTypeToFieldCommandClass := pValue;
end;

function TCustomSQLMapper.TableRowCommandToString(pCommandType: TCommandType): String;
begin
  Result := EmptyStr;
  case pCommandType of
    Add:
      Result := 'insert into';
    Alter:
      Result := 'update';
    Drop:
      Result := 'delete from';
  end;
end;

procedure TCustomSQLMapper.WriteSQLFormatSettings(pSQLFormatSettings: TSQLFormatSettings);
begin
  pSQLFormatSettings.TimeFormat := 'HH:nn:ss';
end;

function TCustomSQLMapper.TableCommandToString(pCommandType: TCommandType): String;
begin
  Result := pCommandType.ToString + ' table';
end;

end.
