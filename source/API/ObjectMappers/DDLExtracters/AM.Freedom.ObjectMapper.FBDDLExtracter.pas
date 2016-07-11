unit AM.Freedom.ObjectMapper.FBDDLExtracter;

interface

uses
  System.Classes,
  AM.Freedom.SQLMappers.CustomDDLExtracter,
  AM.Freedom.ObjectMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.DDLObjects,
  AM.Freedom.SQLMappers.SelectClause,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.GeneratorTextList,
  AM.Freedom.TextGenerator.MasterTextGenerator,
  AM.Freedom.SQLMappers.Expressions,
  AM.Freedom.SQLCommands.Fields,
  AM.Freedom.SQLCommands.Constraints,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.ICursor;

type
  TFBDDLExtracter = class(TCustomDDLExtracter)
  strict protected
    function GetSQLForTable(const pName: string): TSelectClause; override;
    function GetSQLForFields(const pTableName: string): TSelectClause; override;
    function GetSQLForConstraints(const pTableName: string): TSelectClause; override;
    function GetColumnTypeFromCursor(pCursor: ICursor): TColumnType; override;
    function GetSQLForSequence(const pSequenceName: string): TSelectClause; override;
    function GetSQLForDomains: TSelectClause; override;
    function GetSQLForSchemas: TSelectClause; override;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.SQLMappers.FBExpressions,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.DBPersistent.IDBConnector,
  AM.Freedom.Helper.Variant;

{ TFBDDLExtracter }

function TFBDDLExtracter.GetSQLForConstraints(const pTableName: string): TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('RDB$RELATION_CONSTRAINTS', 'RC');
  Result.Field('RDB$CONSTRAINT_NAME', 'RC', 'CONSTRAINT_NAME').Field('case RC.RDB$CONSTRAINT_TYPE when ''PRIMARY KEY'' then 0 when ''FOREIGN KEY'' then 1' + ' when ''UNIQUE'' then 2 end', '', 'CONSTRAINT_TYPE')
    .Field('RDB$FIELD_NAME', 'RIS', 'CONSTRAINT_COLUMN').Field('RDB$RELATION_NAME', 'RCF', 'FOREIGN_TABLE').Field('RDB$FIELD_NAME', 'RISF', 'FOREIGN_COLUMN')
    .Field('case RFC.RDB$UPDATE_RULE when ''RESTRICT'' THEN 0 WHEN ''CASCADE'' THEN 1 WHEN ''SET NULL'' THEN 2' + ' WHEN ''SET DEFAULT'' THEN 3 end', '', 'UPDATE_ACTION')
    .Field('case RFC.RDB$DELETE_RULE when ''RESTRICT'' THEN 0 WHEN ''CASCADE'' THEN 1 WHEN ''SET NULL'' THEN 2' + ' WHEN ''SET DEFAULT'' THEN 3 end', '', 'DELETE_ACTION')
    .JoinTable('RDB$INDICES', [TCriteria.CreateAsEqual(TFieldArgument.Create('RDB$INDEX_NAME', 'RI'), TFieldArgument.Create('RDB$INDEX_NAME', 'RC'))], 'RI')
    .JoinTable('RDB$INDEX_SEGMENTS', [TCriteria.CreateAsEqual(TFieldArgument.Create('RDB$INDEX_NAME', 'RIS'), TFieldArgument.Create('RDB$INDEX_NAME', 'RC'))], 'RIS')
    .JoinTable('RDB$INDEX_SEGMENTS', [TCriteria.CreateAsEqual(TFieldArgument.Create('RDB$INDEX_NAME', 'RISF'), TFieldArgument.Create('RDB$FOREIGN_KEY', 'RI'))], 'RISF', jkLeft)
    .JoinTable('RDB$REF_CONSTRAINTS', [TCriteria.CreateAsEqual(TFieldArgument.Create('RDB$CONSTRAINT_NAME', 'RFC'), TFieldArgument.Create('RDB$CONSTRAINT_NAME', 'RC'))], 'RFC', jkLeft)
    .JoinTable('RDB$RELATION_CONSTRAINTS', [TCriteria.CreateAsEqual(TFieldArgument.Create('RDB$CONSTRAINT_NAME', 'RCF'), TFieldArgument.Create('RDB$FOREIGN_KEY', 'RI'))], 'RCF', jkLeft)
    .Where(TCriteria.CreateAsIn(TFieldArgument.Create('RDB$CONSTRAINT_TYPE', 'RC'), [TValueArgument.CreateAsString('FOREIGN KEY'), TValueArgument.CreateAsString('PRIMARY KEY'), TValueArgument.CreateAsString('UNIQUE')]))
    .Where(TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('RDB$RELATION_NAME', 'RC')), TUpper.Create(TValueArgument.CreateAsString(pTableName)))).OrderBy('RC.RDB$CONSTRAINT_NAME', 1).OrderBy('RIS.RDB$FIELD_POSITION', 2)
    .OrderBy('RISF.RDB$FIELD_POSITION', 3);
end;

function TFBDDLExtracter.GetSQLForDomains: TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('RDB$FIELDS', 'DOM');
  Result.Field('RDB$FIELD_NAME', 'DOM', 'DOMAIN_NAME');
  Result.Field('RDB$FIELD_TYPE', 'DOM', 'DATA_TYPE');
  Result.Field('RDB$FIELD_SUB_TYPE', 'DOM', 'DATA_SUB_TYPE');
  Result.Field('COALESCE(DOM.RDB$CHARACTER_LENGTH, DOM.RDB$SEGMENT_LENGTH, 0)', '', 'DOMAIN_LENGTH');
  Result.Field('RDB$FIELD_PRECISION', 'DOM', 'NUMERIC_PRECISION');
  Result.Field('RDB$FIELD_SCALE', 'DOM', 'NUMERIC_SCALE');
  Result.Field('CASE COALESCE(DOM.RDB$NULL_FLAG, 0) WHEN 0 THEN 1 ELSE 0 END', '', 'IS_NULLABLE');
  Result.WhereClause.Policy := poAndNot;
  Result.Where(TCriteria.CreateAsEqual(TCoalesce.Create(TFieldArgument.Create('RDB$SYSTEM_FLAG', 'DOM'), [TValueArgument.CreateAsInteger(0)]), TValueArgument.CreateAsInteger(1)));
  Result.Where(TCriteria.CreateAsStartingWith(TFieldArgument.Create('RDB$FIELD_NAME', 'DOM'), TValueArgument.CreateAsString('RDB$')));
end;

function TFBDDLExtracter.GetSQLForFields(const pTableName: string): TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('RDB$RELATION_FIELDS', 'RF');
  Result.Field('RDB$FIELD_NAME', 'RF', 'COLUMN_NAME');
  Result.Field('RDB$FIELD_TYPE', 'RDF', 'DATA_TYPE');
  Result.Field('RDB$FIELD_SUB_TYPE', 'RDF', 'DATA_SUB_TYPE');
  Result.Field('RDB$CHARACTER_LENGTH', 'RDF', 'COLUMN_LENGTH');
  Result.Field('RDB$FIELD_PRECISION', 'RDF', 'NUMERIC_PRECISION');
  Result.Field('RDB$FIELD_SCALE', 'RDF', 'NUMERIC_SCALE');
  Result.Field('case coalesce(RDF.RDB$NULL_FLAG, RF.RDB$NULL_FLAG, 0) when 0 then 1 else 0 end', '', 'IS_NULLABLE');
  Result.Field('0', '', 'IS_IDENTITY');
  Result.Field('case when RDF.RDB$FIELD_NAME STARTING WITH (''RDB$'') then 0 ELSE 1 END', '', 'IS_DOMAIN');
  Result.Field('RDB$FIELD_NAME', 'RDF', 'DOMAIN_NAME');
  Result.JoinTable('RDB$FIELDS', [TCriteria.CreateAsEqual(TFieldArgument.Create('RDB$FIELD_NAME', 'RDF'), TFieldArgument.Create('RDB$FIELD_SOURCE', 'RF'))], 'RDF', jkLeft);
  Result.Where(TCriteria.CreateAsEqual(TFieldArgument.Create('RDB$RELATION_NAME', 'RF'), TValueArgument.CreateAsString(pTableName)));
  Result.OrderBy('RF.RDB$FIELD_POSITION', 1);
end;

function TFBDDLExtracter.GetSQLForSchemas: TSelectClause;
begin
  Result := nil;
end;

function TFBDDLExtracter.GetSQLForSequence(const pSequenceName: string): TSelectClause;
begin
  Result := TSelectClause.CreateFromTable('RDB$GENERATORS');
  Result.Field('RDB$GENERATOR_NAME', '', 'SEQUENCE_NAME');
  Result.Where(TCriteria.CreateAsEqual(TCoalesce.Create(TFieldArgument.Create('RDB$SYSTEM_FLAG'), [TValueArgument.CreateAsInteger(0)]), TValueArgument.CreateAsInteger(0)));
  if pSequenceName <> '' then
  begin
    Result.Where(TCriteria.CreateAsEqual(TFieldArgument.Create('RDB$GENERATOR_NAME'), TValueArgument.CreateAsString(pSequenceName)));
  end;
end;

function TFBDDLExtracter.GetSQLForTable(const pName: string): TSelectClause;
const
  cTypeTable = 0;
  cFalse = 0;
begin
  Result := TSelectClause.CreateFromTable('RDB$RELATIONS');
  Result.Field('RDB$RELATION_NAME').Where(TCriteria.CreateAsEqual(TFieldArgument.Create('RDB$RELATION_TYPE'), TValueArgument.CreateAsInteger(cTypeTable)))
    .Where(TCriteria.CreateAsEqual(TFieldArgument.Create('RDB$SYSTEM_FLAG'), TValueArgument.CreateAsInteger(cFalse)));
  if pName <> '' then
  begin
    Result.Where(TCriteria.CreateAsEqual(TFieldArgument.Create('RDB$RELATION_NAME'), TValueArgument.CreateAsString(pName)));
  end;
end;

function TFBDDLExtracter.GetColumnTypeFromCursor(pCursor: ICursor): TColumnType;
begin
  Result := ctyUnknow;
  case pCursor.Values['DATA_TYPE'].ToInt of
    7:
      Result := ctySmallint;
    8:
      Result := ctyInteger;
    16:
      case pCursor.Values['DATA_SUB_TYPE'].ToInt of
        0:
          Result := ctyInt64;
      else
        Result := ctyCurrency;
      end;
    10:
      Result := ctyDouble;
    27:
      Result := ctyExtended;
    37:
      Result := ctyString;
    12:
      Result := ctyDate;
    13:
      Result := ctyTime;
    35:
      Result := ctyDateTime;
    261:
      case pCursor.Values['DATA_SUB_TYPE'].ToInt of
        0:
          Result := ctyBlob;
      else
        Result := ctyMemo;
      end;
    14:
      Result := ctyChar;
  end;
end;

end.
