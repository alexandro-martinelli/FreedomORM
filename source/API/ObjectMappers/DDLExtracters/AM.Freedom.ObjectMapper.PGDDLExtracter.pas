unit AM.Freedom.ObjectMapper.PGDDLExtracter;

interface

uses
  AM.Freedom.SQLMappers.SelectClause,
  AM.Freedom.SQLMappers.CustomDDLExtracter,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ICursor;

type
  TPGDDLExtracter = class(TCustomDDLExtracter)
  strict protected
    function GetSQLForTable(const pName: string): TSelectClause; override;
    function GetSQLForFields(const pTableName: string): TSelectClause; override;
    function GetColumnTypeFromCursor(pCursor: ICursor): TColumnType; override;
    function GetSQLForConstraints(const pTableName: string): TSelectClause; override;
    function GetSQLForSequence(const pSequenceName: string): TSelectClause; override;
    function GetSQLForSchemas: TSelectClause; override;
    function GetSQLForDomains: TSelectClause; override;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLMappers.Expressions,
  AM.Freedom.Helper.Variant;
{ TPGDDLExtracter }

function TPGDDLExtracter.GetColumnTypeFromCursor(pCursor: ICursor): TColumnType;
  function IsSameText(pText: string; pStringArray: array of string): Boolean;
  var
    lCounter: Integer;
  begin
    Result := False;
    for lCounter := low(pStringArray) to high(pStringArray) do
    begin
      Result := SameText(pText, pStringArray[lCounter]);
      if Result then
      begin
        Break;
      end;
    end;
  end;

begin
  Result := ctyUnknow;
  if IsSameText(pCursor.Values['DATA_TYPE'].ToString, ['bigint', 'bigserial']) then
  begin
    Result := ctyInt64;
  end
  else if IsSameText(pCursor.Values['DATA_TYPE'].ToString, ['bit', 'bit varying', 'character varying', 'character']) then
  begin
    Result := ctyString;
  end
  else if SameText(pCursor.Values['DATA_TYPE'].ToString, 'date') then
  begin
    Result := ctyDate;
  end
  else if IsSameText(pCursor.Values['DATA_TYPE'].ToString, ['time', 'time without time zone']) then
  begin
    Result := ctyTime;
  end
  else if IsSameText(pCursor.Values['DATA_TYPE'].ToString, ['timestamp', 'timestamp without time zone']) then
  begin
    Result := ctyDateTime;
  end
  else if SameText(pCursor.Values['DATA_TYPE'].ToString, 'boolean') then
  begin
    Result := ctyBoolean;
  end
  else if IsSameText(pCursor.Values['DATA_TYPE'].ToString, ['smallint', 'smallserial']) then
  begin
    Result := ctySmallint;
  end
  else if IsSameText(pCursor.Values['DATA_TYPE'].ToString, ['serial', 'integer']) then
  begin
    Result := ctyInteger;
  end
  else if SameText(pCursor.Values['DATA_TYPE'].ToString, 'real') then
  begin
    Result := ctySingle;
  end
  else if SameText(pCursor.Values['DATA_TYPE'].ToString, 'double precision') then
  begin
    Result := ctyDouble;
  end
  else if IsSameText(pCursor.Values['DATA_TYPE'].ToString, ['numeric', 'decimal']) then
  begin
    Result := ctyExtended;
  end
  else if SameText(pCursor.Values['DATA_TYPE'].ToString, 'money') then
  begin
    Result := ctyCurrency;
  end
  else if SameText(pCursor.Values['DATA_TYPE'].ToString, 'text') then
  begin
    Result := ctyMemo;
  end
  else if SameText(pCursor.Values['DATA_TYPE'].ToString, 'uuid') then
  begin
    Result := ctyGuid
  end
  else if IsSameText(pCursor.Values['DATA_TYPE'].ToString, ['xml', 'json', 'bytea', 'tsquery', 'tsvector']) then
  begin
    Result := ctyBlob;
  end
  else if IsSameText(pCursor.Values['DATA_TYPE'].ToString, ['box', 'cidr', 'circle', 'inet', 'interval', 'line', 'lseg', 'macaddr', 'path', 'point', 'polygon', 'txid_snapshot']) then
  begin
    Result := ctyUnknow;
  end;
end;

function TPGDDLExtracter.GetSQLForConstraints(const pTableName: string): TSelectClause;
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

function TPGDDLExtracter.GetSQLForDomains: TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('INFORMATION_SCHEMA.DOMAINS', 'DOM');
  Result.Field('DOMAIN_NAME', 'DOM');
  Result.Field('DATA_TYPE', 'DOM');
  Result.Field('CHARACTER_MAXIMUM_LENGTH', 'DOM', 'DOMAIN_LENGTH');
  Result.Field('NUMERIC_PRECISION', 'DOM');
  Result.Field('NUMERIC_SCALE', 'DOM');
  Result.Field('CAST(1 AS SMALLINT)', '', 'IS_NULLABLE');
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('DOMAIN_SCHEMA', 'DOM')), TValueArgument.CreateAsString('INFORMATION_SCHEMA')));
  Result.OrderBy('DOM.DOMAIN_NAME', 1);
end;

function TPGDDLExtracter.GetSQLForFields(const pTableName: string): TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('INFORMATION_SCHEMA.COLUMNS', 'COL');
  Result.Field('COLUMN_NAME', 'COL', 'COLUMN_NAME');
  Result.Field('DATA_TYPE', 'COL', 'DATA_TYPE');
  Result.Field('CHARACTER_MAXIMUM_LENGTH', 'COL', 'COLUMN_LENGTH');
  Result.Field('NUMERIC_PRECISION', 'COL', 'NUMERIC_PRECISION');
  Result.Field('NUMERIC_SCALE', 'COL', 'NUMERIC_SCALE');
  Result.Field('case COL.IS_NULLABLE when ''NO'' then 0 when ''YES'' then 1 end', '', 'IS_NULLABLE');
  Result.Field('CASE WHEN COL.IS_IDENTITY = ''YES'' THEN 1 ELSE' + ' CASE WHEN UPPER(COL.COLUMN_DEFAULT) LIKE(''NEXTVAL%'') THEN 1' + ' ELSE 0 END END', '', 'IS_IDENTITY');
  Result.Field('case COL.IS_UPDATABLE when ''NO'' then 0 when ''YES'' then 1 end', '', 'IS_UPDATABLE');
  Result.Field('case when COL.DOMAIN_NAME <> '''' then 1 else 0 end', '', 'IS_DOMAIN');
  Result.Field('DOMAIN_NAME', 'COL');
  Result.Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('TABLE_NAME', 'COL')), TUpper.Create(TValueArgument.CreateAsString(pTableName))));
  if GetCurrentSchema <> '' then
  begin
    Result.Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('TABLE_SCHEMA', 'COL')), TUpper.Create(TValueArgument.CreateAsString(GetCurrentSchema))));
  end;
end;

function TPGDDLExtracter.GetSQLForSchemas: TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('INFORMATION_SCHEMA.SCHEMATA', 'SCH');
  Result.Field('SCHEMA_NAME', 'SCH');
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TUpper.Create(TValueArgument.CreateAsString('PG_TOAST'))));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TUpper.Create(TValueArgument.CreateAsString('PG_TEMP_1'))));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TUpper.Create(TValueArgument.CreateAsString('PG_TOAST_TEMP_1'))));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TUpper.Create(TValueArgument.CreateAsString('PG_CATALOG'))));
  Result.Where(TCriteria.CreateAsDifferent(TUpper.Create(TFieldArgument.Create('SCHEMA_NAME', 'SCH')), TUpper.Create(TValueArgument.CreateAsString('INFORMATION_SCHEMA'))));
end;

function TPGDDLExtracter.GetSQLForSequence(const pSequenceName: string): TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('INFORMATION_SCHEMA.SEQUENCES', 'SEQ');
  Result.Field('SEQUENCE_NAME', 'SEQ');
  Result.Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('SEQUENCE_NAME', 'SEQ')), TUpper.Create(TValueArgument.CreateAsString(pSequenceName))));
  if GetCurrentSchema <> '' then
  begin
    Result.Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('SEQUENCE_SCHEMA', 'SEQ')), TUpper.Create(TValueArgument.CreateAsString(GetCurrentSchema))));
  end;
end;

function TPGDDLExtracter.GetSQLForTable(const pName: string): TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('INFORMATION_SCHEMA.TABLES', 'TAB');
  Result.Field('TABLE_NAME', 'TAB');
  Result.Where(TCriteria.CreateAsEqual(TFieldArgument.Create('TABLE_TYPE', 'TAB'), TValueArgument.CreateAsString('BASE TABLE')));
  Result.Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('TABLE_NAME', 'TAB')), TUpper.Create(TValueArgument.CreateAsString(pName))));
  if GetCurrentSchema <> '' then
  begin
    Result.Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('TABLE_SCHEMA', 'TAB')), TUpper.Create(TValueArgument.CreateAsString(GetCurrentSchema))));
  end;
end;

end.
