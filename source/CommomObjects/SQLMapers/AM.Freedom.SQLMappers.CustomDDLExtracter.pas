unit AM.Freedom.SQLMappers.CustomDDLExtracter;

interface

uses
  System.Generics.Collections,
  AM.Freedom.SQLMappers.IDDLExtracter,
  AM.Freedom.ObjectMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.DDLObjects,
  AM.Freedom.Exceptions,
  AM.Freedom.IDBPersistent,
  AM.Freedom.SQLMappers.SelectClause,
  AM.Freedom.Consts,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.SQLCommands.Constraints,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.ObjectMapper.ConstraintMapper,
  AM.Freedom.ICursor, AM.Freedom.SQLCommands.CustomTableCommand;

type
  TCustomDDLExtracter = class(TInterfacedObject, IDDLExtracter)
  strict private
    FDBPersistent: IDBPersistent;
    FObjectMapper: TObjectMapper;
    FDDLEntity: TDDLEntity;
    FCurrentSchema: string;
    function ExtractDDL(pObjectMapper: TObjectMapper; pSchemaName: string; pDDLOptions: TDDLOptions = TConsts.cDDLAll): TDDLEntity;
    function ExtractObjectMapper(pName: string; pSchemaName: string = ''; pDDLOptions: TDDLOptions = TConsts.cDDLAll): TObjectMapper;
    function ExtractDDLDomains(pSchemaName: string): TDDLDomains;
    function ExtractDDLSchemas: TList<string>;
    procedure ExtractDomainFromCursor(pDomain: TDDLDomain; pCursor: ICursor);
    function ExtractDDLTable(pTableName: string): TDDLEntity;
    function ExtractObjectTable(pTableName: string): TObjectMapper;
    procedure ExtractDDLFields;
    procedure ExtractColumnFromCursor(pColumn: TDDLColumn; pCursor: ICursor);
    function ExtractObjectColumnFromCursor(pCursor: ICursor): TCustomColumnMapper;
    procedure ExtractDDLConstraints;
    procedure AdjustIDColumnIfPrimaryConstraint(pConstraint: TCustomConstraint);
    function ExtractConstraintFromCursor(pCursor: ICursor): TCustomConstraint;
    function GetConstraintClass(pConstraintType: Integer): TConstraintClass;
    procedure ExtractConstraintFieldsFromCursor(pConstraint: TCustomConstraint; pCursor: ICursor);
    procedure ExtractForeignConstraintData(pConstraint: TCustomConstraint; pCursor: ICursor);
    procedure ExtractDDLSequence;
    function VerifyDropField(pTableName, pFieldName: String): TList<TCustomTableCommand>;
  strict protected
    function GetSQLFromObject(pObject: TObject): string;
    function GetDBPersistent: IDBPersistent;
    function GetObjectMapper: TObjectMapper;
    function GetDDLEntity: TDDLEntity;
    function GetCurrentSchema: string;
    function GetSQLForTable(const pName: string): TSelectClause; virtual;
    function GetSQLForDomains: TSelectClause; virtual;
    function GetSQLForFields(const pTableName: string): TSelectClause; virtual;
    function GetColumnTypeFromCursor(pCursor: ICursor): TColumnType; virtual;
    function GetSQLForConstraints(const pTableName: string): TSelectClause; virtual;
    function GetSQLForSequence(const pSequenceName: string): TSelectClause; virtual;
    function GetSQLForSchemas: TSelectClause; virtual;
    function NeedsVerifyDropField: Boolean; virtual;
    function DoVerifyDropField(const pTableName, pFieldName: String): TList<TCustomTableCommand>; virtual;
    function GetSelectForChecks(const pTableName, pFieldName: String): TSelectClause; virtual;
    function GetSelectForDefaults(const pTableName, pFieldName: String): TSelectClause; virtual;
  public
    constructor Create(pPersistent: IDBPersistent); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.TextGenerator.MasterTextGenerator,
  AM.Freedom.DBPersistent.IDBConnector,
  AM.Freedom.Helper.Variant;

{ TCustomDLLExtracter }

procedure TCustomDDLExtracter.ExtractConstraintFieldsFromCursor(pConstraint: TCustomConstraint; pCursor: ICursor);
begin
  if not pConstraint.Fields.Contains(Trim(pCursor.Values['CONSTRAINT_COLUMN'].ToString)) then
  begin
    pConstraint.Fields.Add(Trim(pCursor.Values['CONSTRAINT_COLUMN'].ToString));
  end;
end;

procedure TCustomDDLExtracter.ExtractForeignConstraintData(pConstraint: TCustomConstraint; pCursor: ICursor);
begin
  if pConstraint.ConstraintType = TConstraintType.ForeignKey then
  begin
    TForeignKey(pConstraint).ReferencesTo(Trim(pCursor.Values['FOREIGN_TABLE'].ToString));
    if not TForeignKey(pConstraint).ReferenceFields.Contains(Trim(pCursor.Values['FOREIGN_COLUMN'].ToString)) then
    begin
      TForeignKey(pConstraint).ReferenceFields.Add(Trim(pCursor.Values['FOREIGN_COLUMN'].ToString));
    end;
    TForeignKey(pConstraint).OnUpdate := TForeignOption(pCursor.Values['UPDATE_ACTION'].ToInt);
    TForeignKey(pConstraint).OnDelete := TForeignOption(pCursor.Values['DELETE_ACTION'].ToInt);
  end;
end;

function TCustomDDLExtracter.ExtractObjectColumnFromCursor(pCursor: ICursor): TCustomColumnMapper;
var
  lColumnType: TColumnType;
begin
  lColumnType := GetColumnTypeFromCursor(pCursor);
  case lColumnType of
    ctyBoolean:
      Result := TBooleanColumnMapper.Create;
    ctyEnumerator:
      Result := TEnumerationColumnMapper.Create;
    ctyBlob, ctyMemo:
      Result := TBlobColumnMapper.Create;
  else
    Result := TCustomColumnMapper.Create;
  end;
  Result.Name := pCursor.Values['COLUMN_NAME'].ToString;
  Result.ColumnType := lColumnType;
  if Result.ColumnType in [ctyString, ctyChar] then
  begin
    if (pCursor.Values['COLUMN_LENGTH'].ToInt > 0) then
    begin
      Result.Size := pCursor.Values['COLUMN_LENGTH'].ToInt;
    end;
  end
  else if Result.ColumnType in [ctyCurrency] then
  begin
    Result.Size := pCursor.Values['NUMERIC_PRECISION'].ToInt;
    Result.Scale := pCursor.Values['NUMERIC_SCALE'].ToInt;
  end;
  if pCursor.Values['IS_DOMAIN'].ToInt = 1 then
  begin
    Result.Domain := pCursor.Values['DOMAIN_NAME'].ToString;
  end;
  if pCursor.Values['IS_NULLABLE'].ToInt = 0 then
  begin
    Result.ColumnOptions := [Required];
  end;
  if pCursor.Values['IS_IDENTITY'].ToInt = 1 then
  begin
    Result.IdOptions.IsId := True;
    Result.IdOptions.IdOption := Identity;
  end;
end;

function TCustomDDLExtracter.ExtractObjectMapper(pName, pSchemaName: string; pDDLOptions: TDDLOptions): TObjectMapper;
var
  lSchemas: TList<string>;
begin
  FCurrentSchema := pSchemaName;
  FObjectMapper := nil;
  lSchemas := ExtractDDLSchemas;
  try
    if lSchemas.Contains(LowerCase(FCurrentSchema)) or (pSchemaName = '') then
    begin
      FObjectMapper := ExtractObjectTable(pName);
      if Assigned(FObjectMapper) then
      begin
        if (Fields in pDDLOptions) or (Sequences in pDDLOptions) then
        begin
          ExtractDDLFields;
        end;
        if (Sequences in pDDLOptions) and (Assigned(FDDLEntity)) then
        begin
          ExtractDDLSequence;
        end;
        if (Constraints in pDDLOptions) then
        begin
          ExtractDDLConstraints;
        end;
      end;
    end;
  finally
    lSchemas.Free;
  end;
  Result := FObjectMapper;
end;

function TCustomDDLExtracter.ExtractObjectTable(pTableName: string): TObjectMapper;
var
  lDBQuery: IDBStatement;
  lSelect: TSelectClause;
  lCursor: ICursor;
begin
  lDBQuery := GetDBPersistent.NewDBStatement;
  lSelect := GetSQLForTable(pTableName);
  try
    lDBQuery.SetCommand(lSelect, GetDBPersistent.GetSQLMapper);
    lCursor := lDBQuery.ExecuteQuery;
    Result := nil;
    try
      if not lCursor.IsEmpty then
      begin
        Result := TObjectMapper.Create;
        Result.Name := pTableName;
        if (GetCurrentSchema <> '') then
        begin
          Result.Schemas.AddSchema(GetCurrentSchema, True);
        end;
      end;
    finally
      lCursor := nil
    end;
  finally
    lDBQuery := nil;
    lSelect.Free;
  end;
end;

procedure TCustomDDLExtracter.AdjustIDColumnIfPrimaryConstraint(pConstraint: TCustomConstraint);
var
  lField: string;
  lDDLColumn: TDDLColumn;
  lColumn: TCustomColumnMapper;
begin
  if pConstraint.ConstraintType = TConstraintType.PrimaryKey then
  begin
    for lField in pConstraint.Fields do
    begin
      if (Assigned(FDDLEntity)) then
      begin
        lDDLColumn := GetDDLEntity.Columns.FindColumn(lField, ctyUnknow, [ctyDetail, ctyJoin]);
        if Assigned(lDDLColumn) then
        begin
          lDDLColumn.IdOptions.IsId := True;
          lDDLColumn.ColumnOptions := lDDLColumn.ColumnOptions + [Required];
        end;
      end
      else
      begin
        lColumn := FObjectMapper.Columns.FindColumn(lField);
        if Assigned(lColumn) then
        begin
          lColumn.IdOptions.IsId := True;
        end;
      end;
    end;
  end;
end;

function TCustomDDLExtracter.ExtractConstraintFromCursor(pCursor: ICursor): TCustomConstraint;
begin
  if (Assigned(FDDLEntity)) then
  begin
    Result := GetDDLEntity.Constraints.FindConstraint(Trim(pCursor.Values['CONSTRAINT_NAME'].ToString));
  end
  else
  begin
    Result := nil;
  end;
  if not Assigned(Result) then
  begin
    Result := GetConstraintClass(pCursor.Values['CONSTRAINT_TYPE'].ToInt).Create;
    Result.Name := Trim(pCursor.Values['CONSTRAINT_NAME'].ToString);
  end;
  ExtractConstraintFieldsFromCursor(Result, pCursor);
  ExtractForeignConstraintData(Result, pCursor);
end;

constructor TCustomDDLExtracter.Create(pPersistent: IDBPersistent);
begin
  FDBPersistent := pPersistent;
end;

destructor TCustomDDLExtracter.Destroy;
begin
  FDBPersistent := nil;
  FObjectMapper := nil;
  inherited;
end;

function TCustomDDLExtracter.DoVerifyDropField(const pTableName, pFieldName: String): TList<TCustomTableCommand>;
begin
  raise EInvalidMethodCallOnClass.Create('DoVerifyDropField', ClassName);
end;

procedure TCustomDDLExtracter.ExtractColumnFromCursor(pColumn: TDDLColumn; pCursor: ICursor);
begin
  pColumn.Name := pCursor.Values['COLUMN_NAME'].ToString;
  pColumn.ColumnType := GetColumnTypeFromCursor(pCursor);
  if pColumn.ColumnType in [ctyString, ctyChar] then
  begin
    pColumn.Size := pCursor.Values['COLUMN_LENGTH'].ToInt;
  end
  else if pColumn.ColumnType in [ctyCurrency] then
  begin
    pColumn.Size := pCursor.Values['NUMERIC_PRECISION'].ToInt;
    pColumn.Scale := pCursor.Values['NUMERIC_SCALE'].ToInt;
  end;
  if pCursor.Values['IS_DOMAIN'].ToInt = 1 then
  begin
    pColumn.Domain := pCursor.Values['DOMAIN_NAME'].ToString;
  end;
  if pCursor.Values['IS_NULLABLE'].ToInt = 0 then
  begin
    pColumn.ColumnOptions := [Required];
  end;
  if pCursor.Values['IS_IDENTITY'].ToInt = 1 then
  begin
    pColumn.IdOptions.IsId := True;
    pColumn.IdOptions.IdOption := Identity;
  end;
end;

function TCustomDDLExtracter.ExtractDDL(pObjectMapper: TObjectMapper; pSchemaName: string; pDDLOptions: TDDLOptions): TDDLEntity;
var
  lSchemas: TList<string>;
begin
  FObjectMapper := pObjectMapper;
  FCurrentSchema := pSchemaName;
  FDDLEntity := nil;
  lSchemas := ExtractDDLSchemas;
  try
    if lSchemas.Contains(LowerCase(FCurrentSchema)) or (pSchemaName = '') then
    begin
      FDDLEntity := ExtractDDLTable(pObjectMapper.Name);
      if Assigned(FDDLEntity) then
      begin
        if (Fields in pDDLOptions) or (Sequences in pDDLOptions) then
        begin
          ExtractDDLFields;
        end;
        if Sequences in pDDLOptions then
        begin
          ExtractDDLSequence;
        end;
        if Constraints in pDDLOptions then
        begin
          ExtractDDLConstraints;
        end;
      end;
    end;
  finally
    lSchemas.Free;
  end;
  Result := FDDLEntity;
end;

procedure TCustomDDLExtracter.ExtractDDLConstraints;
var
  lSelect: TSelectClause;
  lDBQuery: IDBStatement;
  lCursor: ICursor;
  lConstraint: TCustomConstraint;
begin
  if (Assigned(FDDLEntity)) then
  begin
    lSelect := GetSQLForConstraints(GetDDLEntity.Name);
  end
  else
  begin
    lSelect := GetSQLForConstraints(FObjectMapper.Name);
  end;
  lDBQuery := GetDBPersistent.NewDBStatement;
  try
    lDBQuery.SetCommand(lSelect, GetDBPersistent.GetSQLMapper);
    lCursor := lDBQuery.ExecuteQuery;
    try
      while not lCursor.Eof do
      begin
        lConstraint := ExtractConstraintFromCursor(lCursor);
        if (Assigned(FDDLEntity)) then
        begin
          if not FDDLEntity.Constraints.Contains(lConstraint) then
          begin
            FDDLEntity.Constraints.Add(lConstraint);
            AdjustIDColumnIfPrimaryConstraint(lConstraint);
          end;
        end
        else
        begin
          case lConstraint.ConstraintType of
            PrimaryKey:
              begin
                FObjectMapper.Primarys.Add(TPrimaryMapper.Create(lConstraint.Fields, nil));
                AdjustIDColumnIfPrimaryConstraint(lConstraint);
              end;
            UniqueKey:
              begin
                FObjectMapper.Uniques.Add(TUniqueMapper.Create(lConstraint.Fields, nil));
              end;
          end;
          lConstraint.Free;
        end;
        lCursor.Next;
      end;
    finally
      lCursor := nil;
    end;
  finally
    lDBQuery := nil;
    lSelect.Free;
  end;
end;

function TCustomDDLExtracter.ExtractDDLDomains(pSchemaName: string): TDDLDomains;
var
  lSelect: TSelectClause;
  lDBQuery: IDBStatement;
  lCursor: ICursor;
  lDomain: TDDLDomain;
begin
  FCurrentSchema := pSchemaName;
  Result := TDDLDomains.Create;
  lSelect := GetSQLForDomains;
  lDBQuery := GetDBPersistent.NewDBStatement;
  try
    lDBQuery.SetCommand(lSelect, GetDBPersistent.GetSQLMapper);
    lCursor := lDBQuery.ExecuteQuery;
    try
      while not lCursor.Eof do
      begin
        lDomain := TDDLDomain.Create(GetCurrentSchema);
        ExtractDomainFromCursor(lDomain, lCursor);
        Result.Add(lDomain);
        lCursor.Next;
      end;
    finally
      lCursor := nil;
    end;
  finally
    lDBQuery := nil;
    lSelect.Free;
  end;
end;

procedure TCustomDDLExtracter.ExtractDDLFields;
var
  lSelect: TSelectClause;
  lDBQuery: IDBStatement;
  lCursor: ICursor;
  lColumn: TDDLColumn;
  lObjectColumn: TCustomColumnMapper;
begin
  if (Assigned(FDDLEntity)) then
  begin
    lSelect := GetSQLForFields(GetDDLEntity.Name);
  end
  else
  begin
    lSelect := GetSQLForFields(FObjectMapper.Name);
  end;
  lDBQuery := GetDBPersistent.NewDBStatement;
  try
    lDBQuery.SetCommand(lSelect, GetDBPersistent.GetSQLMapper);
    lCursor := lDBQuery.ExecuteQuery;
    try
      while not lCursor.Eof do
      begin
        if (Assigned(FDDLEntity)) then
        begin
          lColumn := TDDLColumn.Create;
          ExtractColumnFromCursor(lColumn, lCursor);
          GetDDLEntity.Columns.Add(lColumn);
        end
        else
        begin
          lObjectColumn := ExtractObjectColumnFromCursor(lCursor);
          FObjectMapper.Columns.Add(lObjectColumn);
        end;
        lCursor.Next;
      end;
    finally
      lCursor := nil;
    end;
  finally
    lDBQuery := nil;
    lSelect.Free;
  end;
end;

function TCustomDDLExtracter.ExtractDDLSchemas: TList<string>;
var
  lDBQuery: IDBStatement;
  lSelect: TSelectClause;
  lCursor: ICursor;
begin
  lDBQuery := GetDBPersistent.NewDBStatement;
  lSelect := GetSQLForSchemas;
  Result := TList<string>.Create;
  try
    if Assigned(lSelect) then
    begin
      lDBQuery.SetCommand(lSelect, GetDBPersistent.GetSQLMapper);
      lCursor := lDBQuery.ExecuteQuery;
      try
        while not lCursor.Eof do
        begin
          Result.Add(LowerCase(lCursor.Values['SCHEMA_NAME'].ToString));
          lCursor.Next;
        end;
      finally
        lCursor := nil;
      end;
    end;
  finally
    lDBQuery := nil;
    FreeAndNil(lSelect);
  end;
end;

procedure TCustomDDLExtracter.ExtractDDLSequence;
var
  lObjectMapper: TObjectMapper;
  lColumn: TCustomColumnMapper;
  lDDLColumn: TDDLColumn;
  lDBQuery: IDBStatement;
  lSelect: TSelectClause;
  lCursor: ICursor;
begin
  lObjectMapper := GetObjectMapper;
  for lColumn in lObjectMapper.Columns do
  begin
    if lColumn.IdOptions.IsValidSequence(GetCurrentSchema) then
    begin
      lDDLColumn := GetDDLEntity.Columns.FindColumn(lColumn.Name, lColumn.ColumnType);
      if Assigned(lDDLColumn) then
      begin
        lSelect := GetSQLForSequence(lColumn.IdOptions.SequenceName);
        lDBQuery := GetDBPersistent.NewDBStatement;
        try
          lDBQuery.SetCommand(lSelect, GetDBPersistent.GetSQLMapper);
          lCursor := lDBQuery.ExecuteQuery;
          try
            if not lCursor.IsEmpty then
            begin
              lDDLColumn.IdOptions.SequenceName := lColumn.IdOptions.SequenceName;
              lDDLColumn.IdOptions.IdOption := TIdOption.Sequence;
              if GetCurrentSchema <> '' then
              begin
                lDDLColumn.IdOptions.Schemas.AddSchema(GetCurrentSchema, True);
              end;
            end;
          finally
            lCursor := nil;
          end;
        finally
          lDBQuery := nil;
          lSelect.Free;
        end;
      end;
    end;
  end;
end;

function TCustomDDLExtracter.ExtractDDLTable(pTableName: string): TDDLEntity;
var
  lDBQuery: IDBStatement;
  lSelect: TSelectClause;
  lCursor: ICursor;
begin
  lDBQuery := GetDBPersistent.NewDBStatement;
  lSelect := GetSQLForTable(pTableName);
  try
    lDBQuery.SetCommand(lSelect, GetDBPersistent.GetSQLMapper);
    lCursor := lDBQuery.ExecuteQuery;
    Result := nil;
    try
      if not lCursor.IsEmpty then
      begin
        Result := TDDLEntity.Create;
        Result.Name := pTableName;
        Result.Schema := GetCurrentSchema;
      end;
    finally
      lCursor := nil;
    end;
  finally
    lDBQuery := nil;
    lSelect.Free;
  end;
end;

procedure TCustomDDLExtracter.ExtractDomainFromCursor(pDomain: TDDLDomain; pCursor: ICursor);
begin
  pDomain.Name := pCursor.Values['DOMAIN_NAME'].ToString;
  pDomain.ColumnType := GetColumnTypeFromCursor(pCursor);
  if pDomain.ColumnType in [ctyString, ctyChar] then
  begin
    pDomain.Size := pCursor.Values['DOMAIN_LENGTH'].TryToInt;
  end
  else if pDomain.ColumnType in [ctyCurrency] then
  begin
    pDomain.Size := pCursor.Values['NUMERIC_PRECISION'].TryToInt;
    pDomain.Scale := pCursor.Values['NUMERIC_SCALE'].TryToInt;
  end;
  if pCursor.Values['IS_NULLABLE'].TryToInt = 0 then
  begin
    pDomain.ColumnOptions := [Required];
  end;
end;

function TCustomDDLExtracter.GetColumnTypeFromCursor(pCursor: ICursor): TColumnType;
begin
  raise EInvalidMethodCallOnClass.Create('GetColumnTypeFromCursor', ClassName);
end;

function TCustomDDLExtracter.GetConstraintClass(pConstraintType: Integer): TConstraintClass;
begin
  Result := nil;
  case TConstraintType(pConstraintType) of
    PrimaryKey:
      Result := TPrimaryKey;
    ForeignKey:
      Result := TForeignKey;
    UniqueKey:
      Result := TUniqueKey;
  end;
end;

function TCustomDDLExtracter.GetCurrentSchema: string;
begin
  Result := FCurrentSchema;
end;

function TCustomDDLExtracter.GetDBPersistent: IDBPersistent;
begin
  Result := FDBPersistent;
end;

function TCustomDDLExtracter.GetDDLEntity: TDDLEntity;
begin
  Result := FDDLEntity;
end;

function TCustomDDLExtracter.GetObjectMapper: TObjectMapper;
begin
  Result := FObjectMapper;
end;

function TCustomDDLExtracter.GetSelectForChecks(const pTableName, pFieldName: String): TSelectClause;
begin
  raise EInvalidMethodCallOnClass.Create('GetSelectForChecks', ClassName);
end;

function TCustomDDLExtracter.GetSelectForDefaults(const pTableName, pFieldName: String): TSelectClause;
begin
  raise EInvalidMethodCallOnClass.Create('GetSelectForDefaults', ClassName);
end;

function TCustomDDLExtracter.GetSQLForConstraints(const pTableName: string): TSelectClause;
begin
  raise EInvalidMethodCallOnClass.Create('GetSQLForConstraints', ClassName);
end;

function TCustomDDLExtracter.GetSQLForDomains: TSelectClause;
begin
  raise EInvalidMethodCallOnClass.Create('GetSQLForDomains', ClassName);
end;

function TCustomDDLExtracter.GetSQLForFields(const pTableName: string): TSelectClause;
begin
  raise EInvalidMethodCallOnClass.Create('GetSQLForFields', ClassName);
end;

function TCustomDDLExtracter.GetSQLForSchemas: TSelectClause;
begin
  raise EInvalidMethodCallOnClass.Create('GetSQLForSchemas', ClassName);
end;

function TCustomDDLExtracter.GetSQLForSequence(const pSequenceName: string): TSelectClause;
begin
  raise EInvalidMethodCallOnClass.Create('GetSQLForSequence', ClassName);
end;

function TCustomDDLExtracter.GetSQLForTable(const pName: string): TSelectClause;
begin
  raise EInvalidMethodCallOnClass.Create('GetSQLForTable', ClassName);
end;

function TCustomDDLExtracter.GetSQLFromObject(pObject: TObject): string;
begin
  Result := TMasterTextGenerator.ExtractText(pObject);
  pObject.Free;
end;

function TCustomDDLExtracter.NeedsVerifyDropField: Boolean;
begin
  Result := False;
end;

function TCustomDDLExtracter.VerifyDropField(pTableName, pFieldName: String): TList<TCustomTableCommand>;
begin
  Result := nil;
  if (NeedsVerifyDropField) then
  begin
    Result := DoVerifyDropField(pTableName, pFieldName);
  end;
end;

end.
