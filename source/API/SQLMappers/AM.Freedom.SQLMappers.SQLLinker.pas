unit AM.Freedom.SQLMappers.SQLLinker;

interface

uses
  AM.Freedom.SQLMappers.CustomSelect,
  AM.Freedom.IDBPersistent,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.FreedomObjectList,
  AM.Freedom.SQLMappers.JoinClause,
  AM.Freedom.FreedomObject,
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.ObjectMapper,
  AM.Freedom.ObjectMapper.ColumnMappers;

type
  TSQLLinker<T: TFreedomObject, constructor> = class(TCustomSelect)
  strict private
    FDBPersistent: IDBPersistent;
  public
    constructor Create(pDBPersistent: IDBPersistent; pAddAllFields: Boolean = False; pAddAllFieldMode: TAddAllFieldsMode = FieldByField); reintroduce;
    destructor Destroy; override;
    function Join(const pArgument: TCustomArgument; pJoinOn: Array of TCriteria; const pJoinKind: TJoinKind = jkJoin): TSQLLinker<T>; overload;
    function Join(const pJoin: TJoin): TSQLLinker<T>; overload;
    function Join<C: class>(const pJoinKind: TJoinKind = jkJoin): TSQLLinker<T>; overload;
    function Join<C: class>(pJoinOn: Array of TCriteria; const pJoinKind: TJoinKind = jkJoin): TSQLLinker<T>; overload;
    function JoinTable(pName: String; pJoinOn: Array of TCriteria; pAlias: String = ''; const pJoinKind: TJoinKind = jkJoin): TSQLLinker<T>; overload;
    function Where(pWhere: TCriteria): TSQLLinker<T>; overload;
    function GroupBy(pField: TFieldArgument): TSQLLinker<T>; overload;
    function Having(pHaving: TCriteria): TSQLLinker<T>; overload;
    function OrderBy(const pFieldNameOrIndex: string; const pIndex: Byte; const pTableAlias: String = ''; const pDirective: String = ''; const pOrder: TOrderType = TOrderType.Asc): TSQLLinker<T>; overload;
    function List: TFreedomObjectList<T>;
    function First: T;
    function Last: T;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.Exceptions,
  AM.Freedom.ObjectMapper.CustomColumnMapper;

function TSQLLinker<T>.GroupBy(pField: TFieldArgument): TSQLLinker<T>;
begin
  Result := Self;
  AddGroupBy(pField);
end;

function TSQLLinker<T>.Having(pHaving: TCriteria): TSQLLinker<T>;
begin
  Result := Self;
  AddHaving(pHaving);
end;

function TSQLLinker<T>.Join(const pArgument: TCustomArgument; pJoinOn: array of TCriteria; const pJoinKind: TJoinKind): TSQLLinker<T>;
begin
  Result := Self;
  Addjoin(pArgument, pJoinOn, pJoinKind);
end;

function TSQLLinker<T>.OrderBy(const pFieldNameOrIndex: String; const pIndex: Byte; const pTableAlias, pDirective: String; const pOrder: TOrderType): TSQLLinker<T>;
begin
  Result := Self;
  AddOrderBy(pFieldNameOrIndex, pIndex, pTableAlias, pDirective, pOrder);
end;

function TSQLLinker<T>.Where(pWhere: TCriteria): TSQLLinker<T>;
begin
  Result := Self;
  AddWhere(pWhere);
end;

function TSQLLinker<T>.JoinTable(pName: String; pJoinOn: array of TCriteria; pAlias: String; const pJoinKind: TJoinKind): TSQLLinker<T>;
begin
  Result := Self;
  Addjoin(TTableArgument.Create(pName, pAlias), pJoinOn, pJoinKind);
end;

function TSQLLinker<T>.Last: T;
var
  lList: TFreedomObjectList<T>;
  lOldLimitRows: Cardinal;
begin
  if (OrderByClause.Count > 0) then
  begin
    OrderByClause.InvertAllOrderType;
    lOldLimitRows := WhereClause.LimitRows;
    WhereClause.LimitRows := 1;
  end;
  lList := List;
  Result := nil;
  try
    if (lList.Count > 0) then
    begin
      Result := lList.Extract(lList.Last);
    end;
  finally
    lList.Free;
    if (OrderByClause.Count > 0) then
    begin
      OrderByClause.InvertAllOrderType;
      WhereClause.LimitRows := lOldLimitRows;
    end;
  end;
end;

function TSQLLinker<T>.List: TFreedomObjectList<T>;
var
  lFreedomObjectList: IFreedomObjectList;
begin
  Result := TFreedomObjectList<T>.Create(FDBPersistent);
  try
    Supports(Result, IFreedomObjectList, lFreedomObjectList);
    lFreedomObjectList.DoSearchWithSelect(Self);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSQLLinker<T>.Join(const pJoin: TJoin): TSQLLinker<T>;
begin
  Result := Self;
  Addjoin(pJoin);
end;

function TSQLLinker<T>.Join<C>(pJoinOn: array of TCriteria; const pJoinKind: TJoinKind): TSQLLinker<T>;
begin
  Result := Self;
  Addjoin<C>(pJoinOn, pJoinKind);
end;

function TSQLLinker<T>.Join<C>(const pJoinKind: TJoinKind = jkJoin): TSQLLinker<T>;
var
  lParams: TObjectToMapperParams;
  lFromMapper: TObjectMapper;
  lJoinMapper: TObjectMapper;
  lJoinOn: Array of TCriteria;
  lColumn: TCustomColumnMapper;
begin
  Result := Self;
  lParams := TObjectToMapperParams.Create;
  lParams.MetaClassType := T;
  lParams.Options := [];
  lFromMapper := TObjectToMapper.ObjectToMapper(lParams);
  lParams := TObjectToMapperParams.Create;
  lParams.MetaClassType := C;
  lParams.Options := [];
  lJoinMapper := TObjectToMapper.ObjectToMapper(lParams);
  try
    SetLength(lJoinOn, 0);
    if (lFromMapper.Columns.FindColumn(lJoinMapper.Columns.IDColumn.Name)) <> nil then
    begin
      SetLength(lJoinOn, 1);
      lJoinOn[0] := TCriteria.CreateAsEqual(TFieldArgument.Create(lJoinMapper.Columns.IDColumn.Name, lJoinMapper.Alias),
          TFieldArgument.Create(lJoinMapper.Columns.IDColumn.Name, lFromMapper.Alias));
      Addjoin<C>(lJoinOn, pJoinKind);
    end
    else
    begin
      for lColumn in lFromMapper.Columns do
      begin
        if (lColumn.ColumnType in [ctyJoin, ctyDetail]) and lCOlumn.LazyOptions.IsLazy and
           (lColumn.LazyOptions.LazyClassType = TClass(C)) then
        begin
          SetLength(lJoinOn, 1);
          lJoinOn[0] := TCriteria.CreateAsEqual(TFieldArgument.Create(lJoinMapper.Columns.IDColumn.Name, lJoinMapper.Alias),
              TFieldArgument.Create(lColumn.Name, lFromMapper.Alias));
          Addjoin<C>(lJoinOn, pJoinKind);
        end;
      end;
    end;
    if (Length(lJoinOn) = 0) then
    begin
      raise EInvalidColumnsForJoin.Create(C.ClassName, T.ClassName);
    end;
  finally
    TObjectToMapper.UnlockMapper(lFromMapper.GetHashCode);
    TObjectToMapper.UnlockMapper(lJoinMapper.GetHashCode);
  end;
end;

constructor TSQLLinker<T>.Create(pDBPersistent: IDBPersistent; pAddAllFields: Boolean; pAddAllFieldMode: TAddAllFieldsMode);
begin
  inherited Create;
  FDBPersistent := pDBPersistent;
  AddFrom<T>(pAddAllFields, pAddAllFieldMode);
end;

destructor TSQLLinker<T>.Destroy;
begin
  FDBPersistent := nil;
  inherited;
end;

function TSQLLinker<T>.First: T;
var
  lList: TFreedomObjectList<T>;
begin
  WhereClause.LimitRows := 1;
  Result := nil;
  lList := List;
  try
    if (lList.Count > 0) then
    begin
      Result := lList.Extract(lList.First);
    end;
  finally
    lList.Free;
  end;
end;

end.
