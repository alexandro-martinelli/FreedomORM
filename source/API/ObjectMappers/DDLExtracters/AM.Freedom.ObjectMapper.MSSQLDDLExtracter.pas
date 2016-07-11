unit AM.Freedom.ObjectMapper.MSSQLDDLExtracter;

interface

uses
  System.Generics.Collections,
  AM.Freedom.SQLMappers.SelectClause,
  AM.Freedom.SQLMappers.CustomDDLExtracter,
  AM.Freedom.ObjectMapper.DDLObjects,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ICursor,
  AM.Freedom.SQLCommands.CustomTableCommand;

type
  TMSSQLDDLExtracter = class(TCustomDDLExtracter)
  strict private
    procedure AddDropChecks(const pTableName, pFieldName: String; pCommands: TList<TCustomTableCommand>);
    procedure AddDropDefaults(const pTableName, pFieldName: String; pCommands: TList<TCustomTableCommand>);

  strict protected
    function GetSQLForTable(const pName: string): TSelectClause; override;
    function GetSQLForFields(const pTableName: string): TSelectClause; override;
    function GetSQLForConstraints(const pTableName: string): TSelectClause; override;
    function GetColumnTypeFromCursor(pCursor: ICursor): TColumnType; override;
    function GetSQLForSequence(const pSequenceName: string): TSelectClause; override;
    function GetSQLForDomains: TSelectClause; override;
    function GetSQLForSchemas: TSelectClause; override;
    function NeedsVerifyDropField: Boolean; override;
    function DoVerifyDropField(const pTableName, pFieldName: string): TList<TCustomTableCommand>; override;
    function GetSelectForChecks(const pTableName, pFieldName: string): TSelectClause; override;
    function GetSelectForDefaults(const pTableName, pFieldName: string): TSelectClause; override;
  end;

implementation

uses
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLMappers.Expressions,
  AM.Freedom.Helper.Variant,
  AM.Freedom.SQLCommands.TableFieldCommands,
  AM.Freedom.DBPersistent.IDBConnector;

{ TMSSQLDDLExtracter }

procedure TMSSQLDDLExtracter.AddDropChecks(const pTableName, pFieldName: String; pCommands: TList<TCustomTableCommand>);
var
  lDBQuery: IDBStatement;
  lSelect: TSelectClause;
  lCursor: ICursor;
  lName: String;
begin
  lDBQuery := GetDBPersistent.NewDBStatement;
  lSelect := GetSelectForChecks(pTableName, pFieldName);
  try
    lDBQuery.SetCommand(lSelect, GetDBPersistent.GetSQLMapper);
    lCursor := lDBQuery.ExecuteQuery;
    lCursor.First;
    while not lCursor.Eof do begin
      lName := lCursor.Values[0];
      if (lName <> '') then
      begin
        pCommands.Add(TDropConstraintCommand.Create(lName, GetCurrentSchema));
      end;
      lCursor.Next;
    end;
  finally
    lDBQuery := nil;
    lSelect.Free;
  end;
end;

procedure TMSSQLDDLExtracter.AddDropDefaults(const pTableName, pFieldName: String; pCommands: TList<TCustomTableCommand>);
var
  lDBQuery: IDBStatement;
  lSelect: TSelectClause;
  lCursor: ICursor;
  lName: String;
begin
  lDBQuery := GetDBPersistent.NewDBStatement;
  lSelect := GetSelectForDefaults(pTableName, pFieldName);
  try
    lDBQuery.SetCommand(lSelect, GetDBPersistent.GetSQLMapper);
    lCursor := lDBQuery.ExecuteQuery;
    lCursor.First;
    while not lCursor.Eof do begin
      lName := lCursor.Values[0];
      if (lName <> '') then
      begin
        pCommands.Add(TDropConstraintCommand.Create(lName, GetCurrentSchema));
      end;
      lCursor.Next;
    end;
  finally
    lDBQuery := nil;
    lSelect.Free;
  end;
end;

function TMSSQLDDLExtracter.DoVerifyDropField(const pTableName, pFieldName: string): TList<TCustomTableCommand>;
begin
  Result := TList<TCustomTableCommand>.Create;
  AddDropChecks(pTableName, pFieldName, Result);
  AddDropDefaults(pTableName, pFieldName, Result);
end;

function TMSSQLDDLExtracter.GetColumnTypeFromCursor(pCursor: ICursor): TColumnType;
begin
  Result := ctyUnknow;
  case pCursor.Values['DATA_TYPE'].ToInt of
    34, 165, 173:
      Result := ctyBlob;
    35, 99:
      Result := ctyMemo;
    36:
      Result := ctyGuid;
    40:
      Result := ctyDate;
    41:
      Result := ctyTime;
    42, 43, 58, 61, 189:
      Result := ctyDateTime;
    48:
      Result := ctyByte;
    52:
      Result := ctySmallint;
    56:
      Result := ctyInteger;
    59:
      Result := ctySingle;
    60, 122:
      Result := ctyExtended;
    106, 108:
      Result := ctyCurrency;
    62:
      Result := ctyDouble;
    98:
      Result := ctyString;
    104:
      Result := ctyBoolean;
    127, 240:
      Result := ctyInt64;
    167, 175, 231, 239:
      Result := ctyString;
    241:
      Result := ctyXML;
  end;
end;

function TMSSQLDDLExtracter.GetSelectForChecks(const pTableName, pFieldName: string): TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('INFORMATION_SCHEMA.TABLE_CONSTRAINTS', 'TC');
  Result.Field('CONSTRAINT_NAME', 'TC');
  Result.JoinTable('INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE', [
      TCriteria.CreateAsEqual(TFieldArgument.Create('TABLE_SCHEMA', 'CCU'), TFieldArgument.Create('TABLE_SCHEMA', 'TC')),
      TCriteria.CreateAsEqual(TFieldArgument.Create('TABLE_NAME', 'CCU'), TFieldArgument.Create('TABLE_NAME', 'TC')),
      TCriteria.CreateAsEqual(TFieldArgument.Create('TABLE_CATALOG', 'CCU'), TFieldArgument.Create('TABLE_CATALOG', 'TC'))], 'CCU');
  Result.WhereClause.AddCriteria([
      TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('CONSTRAINT_TYPE', 'TC')), TValueArgument.CreateAsString('CHECK')),
      TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('TABLE_NAME', 'TC')), TValueArgument.CreateAsString(pTableName)),
      TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('COLUMN_NAME', 'CCU')), TValueArgument.CreateAsString(pFieldName))]);
  if (GetCurrentSchema <> '') then
  begin
    Result.Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('TABLE_SCHEMA', 'TC')), TUpper.Create(TValueArgument.CreateAsString(GetCurrentSchema))));
  end;

end;

function TMSSQLDDLExtracter.GetSelectForDefaults(const pTableName, pFieldName: string): TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('SYSCONSTRAINTS', 'SC');
  Result.Field('NAME', 'DC', 'CONSTRAINT_NAME');
  Result.JoinTable('SYS.DEFAULT_CONSTRAINTS', [
      TCriteria.CreateAsEqual(TFieldArgument.Create('OBJECT_ID', 'DC'), TFieldArgument.Create('CONSTID', 'SC'))], 'DC');
  Result.JoinTable('SYS.TABLES', [
      TCriteria.CreateAsEqual(TFieldArgument.Create('OBJECT_ID', 'TAB'), TFieldArgument.Create('PARENT_OBJECT_ID', 'DC'))], 'TAB');
  Result.JoinTable('INFORMATION_SCHEMA.TABLES', [
      TCriteria.CreateAsEqual(TFieldArgument.Create('TABLE_NAME', 'ITAB'), TFieldArgument.Create('NAME', 'TAB'))], 'ITAB');
  Result.Where([TCriteria.CreateAsEqual(TFieldArgument.Create('ID', 'SC'), TLiteralArgument.Create('OBJECT_ID(''' + pTableName + ''')')),
      TCriteria.CreateAsEqual(TLiteralArgument.Create('COL_NAME(SC.ID, SC.COLID)'), TValueArgument.CreateAsString(pFieldName)),
      TCriteria.CreateAsEqual(TLiteralArgument.Create('OBJECTPROPERTY(SC.CONSTID, ''ISDEFAULTCNST'')'), TValueArgument.CreateAsInteger(1))]);
  if (GetCurrentSchema <> '') then
  begin
    Result.Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('TABLE_SCHEMA', 'ITAB')), TUpper.Create(TValueArgument.CreateAsString(GetCurrentSchema))));
  end;
end;

function TMSSQLDDLExtracter.GetSQLForConstraints(const pTableName: string): TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('INFORMATION_SCHEMA.TABLE_CONSTRAINTS', 'TABC');
  Result.Field('CASE TABC.CONSTRAINT_TYPE WHEN ''PRIMARY KEY'' THEN 0 WHEN ''FOREIGN KEY'' THEN 1' + ' WHEN ''UNIQUE'' THEN 2 END', '', 'CONSTRAINT_TYPE');
  Result.Field('CONSTRAINT_NAME', 'TABC');
  Result.Field('COLUMN_NAME', 'COLC', 'CONSTRAINT_COLUMN');
  Result.Field('TABLE_NAME', 'TABF', 'FOREIGN_TABLE');
  Result.Field('COLUMN_NAME', 'COLF', 'FOREIGN_COLUMN');
  Result.Field('case REFC.UPDATE_RULE when ''NO ACTION'' THEN 0 WHEN ''CASCADE'' THEN 1 WHEN ''SET NULL'' THEN 2' + ' WHEN ''SET DEFAULT'' THEN 3 end', '', 'UPDATE_ACTION');
  Result.Field('case REFC.DELETE_RULE when ''NO ACTION'' THEN 0 WHEN ''CASCADE'' THEN 1 WHEN ''SET NULL'' THEN 2' + ' WHEN ''SET DEFAULT'' THEN 3 end', '', 'DELETE_ACTION');
  Result.JoinTable('INFORMATION_SCHEMA.KEY_COLUMN_USAGE', [TCriteria.CreateAsEqual(TFieldArgument.Create('CONSTRAINT_NAME', 'COLC'), TFieldArgument.Create('CONSTRAINT_NAME', 'TABC')),
    TCriteria.CreateAsEqual(TFieldArgument.Create('CONSTRAINT_SCHEMA', 'COLC'), TFieldArgument.Create('TABLE_SCHEMA', 'TABC'))], 'COLC');
  Result.JoinTable('INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS', [TCriteria.CreateAsEqual(TFieldArgument.Create('CONSTRAINT_NAME', 'REFC'), TFieldArgument.Create('CONSTRAINT_NAME', 'TABC')),
    TCriteria.CreateAsEqual(TFieldArgument.Create('CONSTRAINT_SCHEMA', 'REFC'), TFieldArgument.Create('TABLE_SCHEMA', 'TABC'))], 'REFC', jkLeft);
  Result.JoinTable('INFORMATION_SCHEMA.TABLE_CONSTRAINTS', [TCriteria.CreateAsEqual(TFieldArgument.Create('CONSTRAINT_NAME', 'TABF'), TFieldArgument.Create('UNIQUE_CONSTRAINT_NAME', 'REFC')),
    TCriteria.CreateAsEqual(TFieldArgument.Create('CONSTRAINT_SCHEMA', 'TABF'), TFieldArgument.Create('UNIQUE_CONSTRAINT_SCHEMA', 'REFC'))], 'TABF', jkLeft);
  Result.JoinTable('INFORMATION_SCHEMA.KEY_COLUMN_USAGE', [TCriteria.CreateAsEqual(TFieldArgument.Create('CONSTRAINT_NAME', 'COLF'), TFieldArgument.Create('CONSTRAINT_NAME', 'TABF')),
    TCriteria.CreateAsEqual(TFieldArgument.Create('CONSTRAINT_SCHEMA', 'COLF'), TFieldArgument.Create('TABLE_SCHEMA', 'TABF'))], 'COLF', jkLeft);
  Result.Where(TCriteria.CreateAsIn(TFieldArgument.Create('CONSTRAINT_TYPE', 'TABC'), [TValueArgument.CreateAsString('UNIQUE'), TValueArgument.CreateAsString('PRIMARY KEY'), TValueArgument.CreateAsString('FOREIGN KEY')]));
  Result.Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('TABLE_NAME', 'TABC')), TUpper.Create(TValueArgument.CreateAsString(pTableName))));
  if GetCurrentSchema <> '' then
  begin
    Result.Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('TABLE_SCHEMA', 'TABC')), TUpper.Create(TValueArgument.CreateAsString(GetCurrentSchema))));
  end;
  Result.OrderBy('TABC.CONSTRAINT_NAME', 1);
  Result.OrderBy('COLC.ORDINAL_POSITION', 2);
  Result.OrderBy('COLF.ORDINAL_POSITION', 3);
end;

function TMSSQLDDLExtracter.GetSQLForDomains: TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('INFORMATION_SCHEMA.DOMAINS', 'DOM');
  Result.Field('DOMAIN_NAME', 'DOM');
  Result.Field('SYSTEM_TYPE_ID', 'TYP', 'DATA_TYPE');
  Result.Field('CHARACTER_MAXIMUM_LENGTH', 'DOM', 'DOMAIN_LENGTH');
  Result.Field('NUMERIC_PRECISION', 'DOM');
  Result.Field('NUMERIC_SCALE', 'DOM');
  Result.Field('CAST(TYP.IS_NULLABLE AS TINYINT)', '', 'IS_NULLABLE');
  Result.JoinTable('SYS.SCHEMAS', [TCriteria.CreateAsEqual(TFieldArgument.Create('NAME', 'SCH'), TFieldArgument.Create('DOMAIN_SCHEMA', 'DOM'))], 'SCH');
  Result.JoinTable('SYS.TYPES', [TCriteria.CreateAsEqual(TFieldArgument.Create('NAME', 'TYP'), TFieldArgument.Create('DOMAIN_NAME', 'DOM')), TCriteria.CreateAsEqual(TFieldArgument.Create('SCHEMA_ID', 'TYP'), TFieldArgument.Create('SCHEMA_ID', 'SCH')
    )], 'TYP');
  Result.Where(TCriteria.CreateAsEqual(TFieldArgument.Create('IS_USER_DEFINED', 'TYP'), TValueArgument.CreateAsInteger(1)));
// if GetCurrentSchema <> '' then
// begin
// Result.Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('DOMAIN_SCHEMA', 'DOM')),
// TUpper.Create(TValueArgument.CreateAsString(GetCurrentSchema))));
// end;
  Result.OrderBy('DOM.DOMAIN_NAME', 1);
end;

function TMSSQLDDLExtracter.GetSQLForFields(const pTableName: string): TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('SYS.COLUMNS', 'COL');
  Result.Field('NAME', 'COL', 'COLUMN_NAME');
  Result.Field('SYSTEM_TYPE_ID', 'COL', 'DATA_TYPE');
  Result.Field('MAX_LENGTH', 'COL', 'COLUMN_LENGTH');
  Result.Field('PRECISION', 'COL', 'NUMERIC_PRECISION');
  Result.Field('SCALE', 'COL', 'NUMERIC_SCALE');
  Result.Field('CAST(COL.IS_NULLABLE AS TINYINT)', '', 'IS_NULLABLE');
  Result.Field('CAST(COL.IS_IDENTITY AS TINYINT)', '', 'IS_IDENTITY');
  Result.Field('CAST(TYP.IS_USER_DEFINED AS TINYINT)', '', 'IS_DOMAIN');
  Result.Field('NAME', 'TYP', 'DOMAIN_NAME');
  Result.JoinTable('SYS.TABLES', [TCriteria.CreateAsEqual(TFieldArgument.Create('OBJECT_ID', 'TAB'), TFieldArgument.Create('OBJECT_ID', 'COL'))], 'TAB');
  Result.JoinTable('SYS.SCHEMAS', [TCriteria.CreateAsEqual(TFieldArgument.Create('SCHEMA_ID', 'SCH'), TFieldArgument.Create('SCHEMA_ID', 'TAB'))], 'SCH');
  Result.JoinTable('SYS.TYPES', [TCriteria.CreateAsEqual(TFieldArgument.Create('USER_TYPE_ID', 'TYP'), TFieldArgument.Create('USER_TYPE_ID', 'COL'))], 'TYP');
  Result.Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('NAME', 'TAB')), TUpper.Create(TValueArgument.CreateAsString(pTableName))));
  if GetCurrentSchema <> '' then
  begin
    Result.Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('NAME', 'SCH')), TUpper.Create(TValueArgument.CreateAsString(GetCurrentSchema))));
  end;
  Result.OrderBy('COL.COLUMN_ID', 1);
end;

function TMSSQLDDLExtracter.GetSQLForSchemas: TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('INFORMATION_SCHEMA.SCHEMATA', 'SCH');
  Result.Field('SCHEMA_NAME', 'SCH');
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TValueArgument.CreateAsString('GUEST')));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TValueArgument.CreateAsString('INFORMATION_SCHEMA')));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TValueArgument.CreateAsString('SYS')));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TValueArgument.CreateAsString('DB_OWNER')));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TValueArgument.CreateAsString('DB_ACCESSADMIN')));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TValueArgument.CreateAsString('DB_SECURITYADMIN')));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TValueArgument.CreateAsString('DB_DDLADMIN')));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TValueArgument.CreateAsString('DB_BACKUPOPERATOR')));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TValueArgument.CreateAsString('DB_DATAREADER')));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TValueArgument.CreateAsString('DB_DATAWRITER')));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TValueArgument.CreateAsString('DB_DENYDATAREADER')));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TValueArgument.CreateAsString('DB_DENYDATAWRITER')));
end;

function TMSSQLDDLExtracter.GetSQLForSequence(const pSequenceName: string): TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('INFORMATION_SCHEMA.SCHEMATA');
  Result.Field('CATALOG_NAME', '', 'SEQUENCE_NAME');
  Result.Where(TCriteria.CreateAsDifferent(TValueArgument.CreateAsInteger(1), TValueArgument.CreateAsInteger(1)));
  Result.WhereClause.LimitRows := 1;
end;

function TMSSQLDDLExtracter.GetSQLForTable(const pName: string): TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('INFORMATION_SCHEMA.TABLES', 'TAB');
  Result.Field('TABLE_NAME', 'TAB');
  Result.Where(TCriteria.CreateAsEqual(TFieldArgument.Create('TABLE_TYPE', 'TAB'), TValueArgument.CreateAsString('BASE TABLE')));
  Result.Where(TCriteria.CreateAsEqual(TFieldArgument.Create('TABLE_NAME', 'TAB'), TValueArgument.CreateAsString(pName)));
  if GetCurrentSchema <> '' then
  begin
    Result.Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('TABLE_SCHEMA', 'TAB')), TUpper.Create(TValueArgument.CreateAsString(GetCurrentSchema))));
  end;
end;

function TMSSQLDDLExtracter.NeedsVerifyDropField: Boolean;
begin
  Result := True;
end;

end.
