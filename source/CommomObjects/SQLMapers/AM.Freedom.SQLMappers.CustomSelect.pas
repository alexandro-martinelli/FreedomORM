unit AM.Freedom.SQLMappers.CustomSelect;

interface

uses
  AM.Freedom.SQLMappers.SQLFieldList,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLMappers.ISelect,
  AM.Freedom.SQLMappers.FromClause,
  AM.Freedom.SQLMappers.JoinClause,
  AM.Freedom.SQLMappers.WhereClause,
  AM.Freedom.SQLMappers.OrderByClause,
  AM.Freedom.SQLMappers.GroupByClause,
  AM.Freedom.SQLMappers.HavingClause,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Exceptions,
  AM.Freedom.ObjectMapper,
  AM.Freedom.ObjectMapper.ColumnMappers, AM.Freedom.GroupCriteria;

type
  TAddAllFieldsMode = AM.Freedom.EnumerationTypes.TAddAllFieldsMode;

  TCustomSelect = class(TCustomCommand, ISelect)
  strict private
    FFieldList: TSQLFieldList;
    FFromClause: TFromClause;
    FJoinClause: TJoinClause;
    FWhereClause: TWhereClause;
    FGroupBy: TGroupByClause;
    FHaving: THavingClause;
    FOrderBy: TOrderByClause;
    FOwnsWhere: Boolean;
    procedure CreateAllFields;
    procedure AddJoinColumn(pColumn: TJoinedColumnMapper; pAlias: String);
    function JoinExists(pJoin: TJoin): Boolean;
    function GetLeftArgumentName(pJoin: TJoin): String;
    function GetRigthArgumentName(pJoin: TJoin): String;
  strict protected const
    cWordSeparator: Array [0..6] of String = ('.', ' ', ',', ';', '-', '(', ')');
  strict protected
    function IsSingleWord(pWord: String): Boolean;
    procedure DoAddJoins(pMapper: TObjectMapper);
    function GetFieldList: TSQLFieldList;
    function CreateJoinForColumn(pJoinColumn: TJoinedColumnMapper; pTableAlias: String): TJoin;
    procedure SetFromClause(const pFrom: TFromClause); virtual;
    procedure SetGroupBy(const pGroupBy: TGroupByClause); virtual;
    procedure SetHaving(const pHaving: THavingClause); virtual;
    procedure SetJoinClause(const pJoin: TJoinClause); virtual;
    procedure SetOrderBy(const pOrderBy: TOrderByClause); virtual;
    procedure SetWhereClause(const pWhere: TWhereClause); virtual;
    function GetCommandType: TCommandType; override;
    procedure AddFrom<T: class>(pAddAllFields: Boolean = False; pAddAllFieldMode: TAddAllFieldsMode = FieldByField; pSchemaName: String = '');
    procedure AddAllFieldsFrom<T: class>(pAddAllFieldMode: TAddAllFieldsMode = FieldByField; pSchemaName: String = '');

    procedure AddFromClass(pClass: TClass; pAddAllFields: Boolean = False; pAddAllFieldMode: TAddAllFieldsMode = FieldByField; pSchemaName: String = '');
    procedure AddAllFieldsFromClass(pClass: TClass; pAddAllFieldMode: TAddAllFieldsMode = FieldByField; pSchemaName: String = '');

    procedure DoAddFieldByFieldsFrom(pMapper: TObjectMapper);
    procedure AddFromTable(pName: String; pAlias: String = ''; pSchemaName: String = '');
    procedure AddField(const pName: string; pTableAlias: string = ''; pAlias: String = ''); overload;
    procedure AddField(const pFieldArgument: TFieldArgument); overload;
    procedure AddField(const pArgument: TCustomArgument; pAlias: String = ''); overload;
    procedure AddJoin(const pArgument: TCustomArgument; pJoinOn: Array of TCriteria; const pJoinKind: TJoinKind = jkJoin); overload;
    procedure AddJoin(const pJoin: TJoin); overload;
    procedure AddJoin<T: class>(pJoinOn: Array of TCriteria; const pJoinKind: TJoinKind = jkJoin; pSchemaName: string = ''); overload;
    procedure AddJoinTable(pName: String; pJoinOn: Array of TCriteria; pAlias: String = ''; const pJoinKind: TJoinKind = jkJoin); overload;
    procedure AddWhere(pWhere: TCriteria); overload;
    procedure AddGroupBy(pField: TFieldArgument); overload;
    procedure AddHaving(pHaving: TCriteria); overload;
    procedure AddOrderBy(const pFieldNameOrIndex: string; const pIndex: Byte; const pTableAlias: String = ''; const pDirective: String = ''; const pOrder: TOrderType = TOrderType.Asc); overload;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure AddAllFieldsFromMapper(pMapper: TObjectMapper);
    procedure AssignWhere(pGroupCriteria: TGroupCriteria);

    property FieldList: TSQLFieldList read GetFieldList;
    property FromClause: TFromClause read FFromClause write SetFromClause;
    property JoinClause: TJoinClause read FJoinClause write SetJoinClause;
    property WhereClause: TWhereClause read FWhereClause write SetWhereClause;
    property GroupByClause: TGroupByClause read FGroupBy write SetGroupBy;
    property HavingClause: THavingClause read FHaving write SetHaving;
    property OrderByClause: TOrderByClause read FOrderBy write SetOrderBy;
    property Alias;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.ObjectMapper.MapperToSelectClause,
  AM.Freedom.ObjectMapper.ObjectToMapper;
{ TSelect }

procedure TCustomSelect.AddAllFieldsFrom<T>(pAddAllFieldMode: TAddAllFieldsMode; pSchemaName: String);
begin
  AddAllFieldsFromClass(T, pAddAllFieldMode, pSchemaName);
end;

procedure TCustomSelect.AddFrom<T>(pAddAllFields: Boolean; pAddAllFieldMode: TAddAllFieldsMode; pSchemaName: String);
begin
  AddFromClass(T, pAddAllFields, pAddAllFieldMode, pSchemaName);
end;

procedure TCustomSelect.AddFromTable(pName, pAlias, pSchemaName: String);
begin
  FFromClause.Argument := TTableArgument.Create(pName, pAlias, pSchemaName);
end;

constructor TCustomSelect.Create;
begin
  inherited Create('');
  CreateAllFields;
  FOwnsWhere := True;
end;

procedure TCustomSelect.CreateAllFields;
begin
  FFieldList := TSQLFieldList.Create;
  FFromClause := TFromClause.Create;
  FJoinClause := TJoinClause.Create;
  FWhereClause := TWhereClause.Create;
  FGroupBy := TGroupByClause.Create;
  FHaving := THavingClause.Create;
  FOrderBy := TOrderByClause.Create;
end;

function TCustomSelect.CreateJoinForColumn(pJoinColumn: TJoinedColumnMapper; pTableAlias: String): TJoin;
begin
  Result := nil;
  if (JoinClause.FindJoin(pJoinColumn.RefObjectName, pJoinColumn.RefColumnName, pJoinColumn.Name) = nil) then
  begin
    Result := TJoin.Create;
    Result.Kind := pJoinColumn.JoinKind;
    if (Result.Kind = jkNone) then
    begin
      Result.Kind := jkLeft;
    end;
    Result.JoinTable(pJoinColumn.RefObjectName, pJoinColumn.RefObjectAlias);
    Result.JoinOn.AddCriteria(TCriteria.CreateAsEqual(TFieldArgument.Create(pJoinColumn.RefColumnName, pJoinColumn.RefObjectAlias), TFieldArgument.Create(pJoinColumn.Name, pTableAlias)));
  end;
end;

destructor TCustomSelect.Destroy;
begin
  FreeAndNil(FFieldList);
  FreeAndNil(FFromClause);
  FreeAndNil(FJoinClause);
  if FOwnsWhere then
  begin
    FreeAndNil(FWhereClause);
  end;
  FreeAndNil(FGroupBy);
  FreeAndNil(FHaving);
  FreeAndNil(FOrderBy);
  inherited;
end;

procedure TCustomSelect.DoAddFieldByFieldsFrom(pMapper: TObjectMapper);
var
  lColumn: TCustomColumnMapper;
begin
  for lColumn in pMapper.Columns do
  begin
    if (not lColumn.IsExtension) and ((lColumn.Schemas.FindSchema(pMapper.CurrentSchema) <> nil) or (pMapper.CurrentSchema = '')) then
    begin
      if lColumn.ColumnType = ctyJoin then
      begin
        AddJoinColumn(TJoinedColumnMapper(lColumn), pMapper.Alias);
      end
      else if FieldList.FindField(lColumn.Name) = nil then
      begin
        AddField(lColumn.Name, pMapper.Alias, lColumn.Alias);
      end;
      if lColumn.OrderOptions.Index > 0 then
      begin
        AddOrderBy(lColumn.Name, lColumn.OrderOptions.Index, pMapper.Alias, lColumn.OrderOptions.Directive, lColumn.OrderOptions.OrderType);
      end;
    end;
  end;
  OrderByClause.Sort;
end;

procedure TCustomSelect.DoAddJoins(pMapper: TObjectMapper);
var
  lColumn: TCustomColumnMapper;
  lJoin: TJoin;
begin
  for lColumn in pMapper.Columns.JoinColumns do
  begin
    if ((lColumn.Schemas.FindSchema(pMapper.CurrentSchema) <> nil) or (pMapper.CurrentSchema = '')) then
    begin
      lJoin := CreateJoinForColumn(TJoinedColumnMapper(lColumn), pMapper.Alias);
      if Assigned(lJoin) then
      begin
        JoinClause.Add(lJoin);
      end;
    end;
  end;
end;

procedure TCustomSelect.AddField(const pName: string; pTableAlias, pAlias: String);
var
  lArgument: TCustomArgument;
  lFieldFound: Boolean;
begin
  lFieldFound := False;
  for lArgument in FFieldList do
  begin
    if lArgument.InheritsFrom(TFieldArgument) then
    begin
      if SameText(TFieldArgument(lArgument).Name, pName) and SameText(TFieldArgument(lArgument).TableAlias, pTableAlias) then
      begin
        lFieldFound := True;
        Break;
      end;
    end;
  end;
  if not lFieldFound then
  begin
    if (not IsSingleWord(pName.Trim)) then
    begin
      pTableAlias := '';
    end;
    FFieldList.Add(TFieldArgument.Create(pName, pTableAlias, pAlias));
  end;
end;

procedure TCustomSelect.AddField(const pFieldArgument: TFieldArgument);
begin
  FFieldList.Add(pFieldArgument);
end;

function TCustomSelect.GetCommandType: TCommandType;
begin
  Result := TCommandType.Select;
end;

function TCustomSelect.GetFieldList: TSQLFieldList;
begin
  Result := FFieldList;
end;

function TCustomSelect.JoinExists(pJoin: TJoin): Boolean;
var
  lRefObjectName: String;
  lRefColumnName: String;
  lJoinColumnName: String;
begin
  Result := False;
  if pJoin.Argument.ClassType = TTableArgument then
  begin
    lRefObjectName :=  TTableArgument(pJoin.Argument).Name;
    lRefColumnName := GetLeftArgumentName(pJoin);
    lJoinColumnName := GetRigthArgumentName(pJoin);
    JoinClause.FindJoin(lRefObjectName, lRefColumnName, lJoinColumnName);
  end;
end;

function TCustomSelect.GetLeftArgumentName(pJoin: TJoin): String;
var
  lCriteria: TCriteria;
begin
  Result := '';
  lCriteria := pJoin.JoinOn.ListCriterias.First;
  if Assigned(lCriteria) then
  begin
    if lCriteria.LeftArgument.ClassType = TFieldArgument then
    begin
      Result := TFieldArgument(lCriteria.LeftArgument).Name;
    end;
  end;
end;

function TCustomSelect.GetRigthArgumentName(pJoin: TJoin): String;
var
  lCriteria: TCriteria;
begin
  Result := '';
  lCriteria := pJoin.JoinOn.ListCriterias.First;
  if Assigned(lCriteria) then
  begin
    if lCriteria.RigthArgument.ClassType = TFieldArgument then
    begin
      Result := TFieldArgument(lCriteria.RigthArgument).Name;
    end;
  end;
end;

procedure TCustomSelect.AddGroupBy(pField: TFieldArgument);
begin
  FGroupBy.Add(pField);
end;

procedure TCustomSelect.AddHaving(pHaving: TCriteria);
begin
  FHaving.ListCriterias.Add(pHaving);
end;

procedure TCustomSelect.AddJoin(const pArgument: TCustomArgument; pJoinOn: array of TCriteria; const pJoinKind: TJoinKind);
begin
  AddJoin(TJoin.Create(pArgument, pJoinOn, pJoinKind));
end;

procedure TCustomSelect.AddOrderBy(const pFieldNameOrIndex: String; const pIndex: Byte; const pTableAlias, pDirective: String; const pOrder: TOrderType);
begin
  FOrderBy.Add(TOrderBy.Create(pFieldNameOrIndex, pIndex, pTableAlias, pDirective, pOrder));
end;

procedure TCustomSelect.SetFromClause(const pFrom: TFromClause);
begin
  if FFromClause <> pFrom then
  begin
    FreeAndNil(FFromClause);
  end;
  FFromClause := pFrom;
end;

procedure TCustomSelect.SetGroupBy(const pGroupBy: TGroupByClause);
begin
  if FGroupBy <> pGroupBy then
  begin
    FreeAndNil(FGroupBy);
  end;
  FGroupBy := pGroupBy;
end;

procedure TCustomSelect.SetHaving(const pHaving: THavingClause);
begin
  if FHaving <> pHaving then
  begin
    FreeAndNil(FHaving);
  end;
  FHaving := pHaving;
end;

procedure TCustomSelect.SetJoinClause(const pJoin: TJoinClause);
begin
  if FJoinClause <> pJoin then
  begin
    FreeAndNil(FJoinClause);
  end;
  FJoinClause := pJoin;
end;

procedure TCustomSelect.SetOrderBy(const pOrderBy: TOrderByClause);
begin
  if FOrderBy <> pOrderBy then
  begin
    FreeAndNil(FOrderBy);
  end;
  FOrderBy := pOrderBy;
end;

procedure TCustomSelect.SetWhereClause(const pWhere: TWhereClause);
begin
  if FWhereClause <> pWhere then
  begin
    FreeAndNil(FWhereClause);
  end;
  FWhereClause := pWhere;
  FOwnsWhere := False;
end;

procedure TCustomSelect.AddWhere(pWhere: TCriteria);
begin
  FWhereClause.ListCriterias.Add(pWhere);
end;

procedure TCustomSelect.AssignWhere(pGroupCriteria: TGroupCriteria);
begin
  SetWhereClause(TWhereClause(pGroupCriteria));
end;

procedure TCustomSelect.AddJoinTable(pName: String; pJoinOn: array of TCriteria; pAlias: String; const pJoinKind: TJoinKind);
begin
  AddJoin(TJoin.Create(TTableArgument.Create(pName, pAlias), pJoinOn, pJoinKind));
end;

procedure TCustomSelect.AddJoin(const pJoin: TJoin);
begin
  if (not JoinExists(pJoin)) then
  begin
    FJoinClause.Add(pJoin);
  end
  else
  begin
    pJoin.Free;
  end;
end;

procedure TCustomSelect.AddJoin<T>(pJoinOn: array of TCriteria; const pJoinKind: TJoinKind; pSchemaName: string);
var
  lMapper: TObjectMapper;
  lParams: TObjectToMapperParams;
begin
  lParams := TObjectToMapperParams.Create;
  lParams.MetaClassType := T;
  lParams.Options := [];
  lMapper := TObjectToMapper.ObjectToMapper(lParams);
  try
    AddJoin(TJoin.Create(TTableArgument.Create(lMapper.Name, lMapper.Alias, pSchemaName), pJoinOn, pJoinKind));
  finally
    TObjectToMapper.UnlockMapper(lMapper.GetHashCode);
  end;
end;

procedure TCustomSelect.AddJoinColumn(pColumn: TJoinedColumnMapper; pAlias: string);
var
  lColumnName: string;
  lAlias: String;
  lJoin: TJoin;
begin
  lAlias := pColumn.RefObjectAlias;
  lColumnName := pColumn.RefResultColumnName;
  if (lColumnName = '') then
  begin
    lColumnName := pColumn.Name;
    lAlias := pAlias;
  end
  else
  begin
    lJoin := JoinClause.FindJoin(pColumn.RefObjectName, pColumn.RefColumnName, pColumn.Name);
    if (Assigned(lJoin)) then
    begin
      lAlias := lJoin.Alias;
    end;
  end;
  if (FieldList.FieldNotExists(lColumnName, lAlias, pColumn.Alias)) then
  begin
    AddField(lColumnName, lAlias, pColumn.Alias);
  end;
end;

procedure TCustomSelect.AddAllFieldsFromMapper(pMapper: TObjectMapper);
begin
  DoAddFieldByFieldsFrom(pMapper);
end;

procedure TCustomSelect.AddField(const pArgument: TCustomArgument; pAlias: String);
begin
  FFieldList.Add(pArgument);
end;

procedure TCustomSelect.AddFromClass(pClass: TClass; pAddAllFields: Boolean; pAddAllFieldMode: TAddAllFieldsMode;
  pSchemaName: String);
var
  lMapper: TObjectMapper;
  lParams: TObjectToMapperParams;
begin
  lParams := TObjectToMapperParams.Create;
  lParams.MetaClassType := pClass;
  if pSchemaName = '' then
  begin
    pSchemaName := TObjectToMapper.ExtractDefaultSchema(pClass.ClassInfo);
  end;
  lMapper := TObjectToMapper.ObjectToMapper(lParams);
  lMapper.CurrentSchema := pSchemaName;
  try
    FFromClause.Argument := TTableArgument.Create(lMapper.Name, lMapper.Alias, pSchemaName);
    if pAddAllFields then
    begin
      AddAllFieldsFromClass(pClass, pAddAllFieldMode, pSchemaName)
    end;
  finally
    TObjectToMapper.UnlockMapper(lMapper.GetHashCode);
  end;
end;

procedure TCustomSelect.AddAllFieldsFromClass(pClass: TClass; pAddAllFieldMode: TAddAllFieldsMode; pSchemaName: String);
var
  lMapper: TObjectMapper;
  lParams: TObjectToMapperParams;
begin
  lParams := TObjectToMapperParams.Create;
  lParams.MetaClassType := pClass;
  lParams.Options := [SubLevels];
  lMapper := TObjectToMapper.ObjectToMapper(lParams);
  try
    lMapper.CurrentSchema := pSchemaName;
    DoAddJoins(lMapper);
    if pAddAllFieldMode = Asterisk then
    begin
      AddField('*', lMapper.Alias);
    end
    else
    begin
      DoAddFieldByFieldsFrom(lMapper);
    end;
  finally
    TObjectToMapper.UnlockMapper(lMapper.GetHashCode);
  end;
end;

function TCustomSelect.IsSingleWord(pWord: String): Boolean;
var
  lIndex: Integer;
begin
  for lIndex := Low(cWordSeparator) to High(cWordSeparator) do
  begin
    Result := not ContainsText(pWord, cWordSeparator[lIndex]);
    if (not Result) then
    begin
      Break;
    end;
  end;
end;

end.
