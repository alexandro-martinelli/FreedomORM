unit AM.Freedom.SQLMappers.SelectClause;

interface

uses
  AM.Freedom.SQLMappers.CustomSelect,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLMappers.SQLFieldList,
  AM.Freedom.SQLMappers.FromClause,
  AM.Freedom.SQLMappers.JoinClause,
  AM.Freedom.SQLMappers.WhereClause,
  AM.Freedom.SQLMappers.GroupByClause,
  AM.Freedom.SQLMappers.HavingClause,
  AM.Freedom.SQLMappers.OrderByClause,
  AM.Freedom.ObjectMapper;

type
  TSelectClause = class(TCustomSelect)
  public
    class function CreateFrom<T: class>(pAddAllFields: Boolean = False; pAddAllFieldMode: TAddAllFieldsMode = FieldByField): TSelectClause;
    class function CreateFromClass(pClass: TClass; pAddAllFields: Boolean = False; pAddAllFieldMode: TAddAllFieldsMode = FieldByField): TSelectClause;
    class function CreateFromTable(pName: String; pAlias: String = ''; pSchemaName: String = ''): TSelectClause;
    class function CreateFromSelect(pSelect: TSelectClause): TSelectClause;
    class function CreateFromMapper(pMapper: TObjectMapper): TSelectClause;
    function Field(const pName: string; pTableAlias: string = ''; pAlias: String = ''): TSelectClause; overload;
    function Field(const pFieldArgument: TFieldArgument): TSelectClause; overload;
    function Field(const pArgument: TCustomArgument; pAlias: String = ''): TSelectClause; overload;
    function Join(const pArgument: TCustomArgument; pJoinOn: Array of TCriteria; const pJoinKind: TJoinKind = jkJoin): TSelectClause; overload;
    function Join(const pJoin: TJoin): TSelectClause; overload;
    function Join<T: class>(pJoinOn: Array of TCriteria; const pJoinKind: TJoinKind = jkJoin): TSelectClause; overload;
    function JoinTable(pName: String; pJoinOn: Array of TCriteria; pAlias: String = ''; const pJoinKind: TJoinKind = jkJoin): TSelectClause; overload;
    function Where(pWhere: TCriteria): TSelectClause; overload;
    function Where(pWheres: Array of TCriteria): TSelectClause; overload;
    function GroupBy(pField: TFieldArgument): TSelectClause; overload;
    function GroupBy(pFieldName: String; pTableAlias: String): TSelectClause; overload;
    function Having(pHaving: TCriteria): TSelectClause; overload;
    function OrderBy(const pFieldNameOrIndex: string; const pIndex: Byte; const pTableAlias: String = ''; const pDirective: String = ''; const pOrder: TOrderType = TOrderType.Asc): TSelectClause; overload;
  end;

implementation

uses
  AM.Freedom.ObjectMapper.MapperToSelectClause;

{ TSelect }

class function TSelectClause.CreateFrom<T>(pAddAllFields: Boolean; pAddAllFieldMode: TAddAllFieldsMode): TSelectClause;
begin
  Result := TSelectClause.Create;
  Result.AddFrom<T>(pAddAllFields, pAddAllFieldMode);
end;

class function TSelectClause.CreateFromMapper(pMapper: TObjectMapper): TSelectClause;
begin
  Result := CreateFromTable(pMapper.Name, pMapper.Alias, pMapper.CurrentSchema);
  Result.DoAddJoins(pMapper);
  Result.AddAllFieldsFromMapper(pMapper);
end;

class function TSelectClause.CreateFromSelect(pSelect: TSelectClause): TSelectClause;
begin
  Result := TSelectClause.Create;
  Result.FromClause.Argument := pSelect;
end;

class function TSelectClause.CreateFromTable(pName, pAlias, pSchemaName: String): TSelectClause;
begin
  Result := TSelectClause.Create;
  Result.AddFromTable(pName, pAlias, pSchemaName);
end;

function TSelectClause.Field(const pName: string; pTableAlias, pAlias: String): TSelectClause;
begin
  AddField(pName, pTableAlias, pAlias);
  Result := Self;
end;

function TSelectClause.Field(const pFieldArgument: TFieldArgument): TSelectClause;
begin
  Result := Self;
  AddField(pFieldArgument);
end;

function TSelectClause.GroupBy(pField: TFieldArgument): TSelectClause;
begin
  Result := Self;
  AddGroupBy(pField);
end;

function TSelectClause.Having(pHaving: TCriteria): TSelectClause;
begin
  Result := Self;
  AddHaving(pHaving);
end;

function TSelectClause.Join(const pArgument: TCustomArgument; pJoinOn: array of TCriteria; const pJoinKind: TJoinKind): TSelectClause;
begin
  Result := Self;
  Addjoin(pArgument, pJoinOn, pJoinKind);
end;

function TSelectClause.OrderBy(const pFieldNameOrIndex: String; const pIndex: Byte; const pTableAlias, pDirective: String; const pOrder: TOrderType): TSelectClause;
begin
  Result := Self;
  AddOrderBy(pFieldNameOrIndex, pIndex, pTableAlias, pDirective, pOrder);
end;

function TSelectClause.Where(pWheres: array of TCriteria): TSelectClause;
begin
  Result := Self;
  WhereClause.AddCriteria(pWheres);
end;

function TSelectClause.Where(pWhere: TCriteria): TSelectClause;
begin
  Result := Self;
  AddWhere(pWhere);
end;

function TSelectClause.JoinTable(pName: String; pJoinOn: array of TCriteria; pAlias: String; const pJoinKind: TJoinKind): TSelectClause;
begin
  Result := Self;
  Addjoin(TTableArgument.Create(pName, pAlias), pJoinOn, pJoinKind);
end;

function TSelectClause.Join(const pJoin: TJoin): TSelectClause;
begin
  Result := Self;
  Addjoin(pJoin);
end;

function TSelectClause.Join<T>(pJoinOn: array of TCriteria; const pJoinKind: TJoinKind = jkJoin): TSelectClause;
begin
  Result := Self;
  Addjoin<T>(pJoinOn, pJoinKind);
end;

function TSelectClause.Field(const pArgument: TCustomArgument; pAlias: String): TSelectClause;
begin
  Result := Self;
  AddField(pArgument, pAlias);
end;

function TSelectClause.GroupBy(pFieldName, pTableAlias: String): TSelectClause;
begin
  Result := Self;
  AddGroupBy(TFieldArgument.Create(pFieldName, pTableAlias));
end;

class function TSelectClause.CreateFromClass(pClass: TClass; pAddAllFields: Boolean;
  pAddAllFieldMode: TAddAllFieldsMode): TSelectClause;
begin
  Result := TSelectClause.Create;
  Result.AddFromClass(pClass, pAddAllFields, pAddAllFieldMode);
end;

end.
