unit uTestSQLLinker;

interface

uses
  TestFramework,
  AM.Freedom.FreedomObjectList,
  AM.Freedom.SQLMappers.JoinClause,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.ObjectMapper,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.FreedomObject,
  AM.Freedom.SQLMappers.SQLLinker,
  AM.Freedom.SQLMappers.CustomSelect,
  AM.Freedom.SQLMappers.Arguments,
  uObjectsTests,
  AM.Freedom.CustomDBPersistent;

type
  TestTSQLLinker = class(TTestCase)
  strict private
    FSQLLinker: TSQLLinker<TProduto>;
    function GetConnection: TCustomDBPersistent;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestJoin;
//    procedure TestJoinTable;
//    procedure TestWhere;
//    procedure TestGroupBy;
//    procedure TestHaving;
//    procedure TestOrderBy;
  end;

implementation

uses
  AM.Freedom.DefaultsClassRegister;

function TestTSQLLinker.GetConnection: TCustomDBPersistent;
begin
  Result := TCustomDBPersistent(TDefaultsClassRegister.DefaultPersistent);
end;

procedure TestTSQLLinker.SetUp;
begin
  FSQLLinker := GetConnection.CreateLinker<TProduto>;
end;

procedure TestTSQLLinker.TearDown;
begin
  FSQLLinker.Free;
  FSQLLinker := nil;
end;

procedure TestTSQLLinker.TestJoin;
begin
  FSQLLinker.Join<TUnidade>;
  CheckTrue(FSQLLinker.JoinClause.Items[0].Argument.ClassType = TTableArgument);
  CheckEqualsString(TTableArgument(FSQLLinker.JoinClause.Items[0].Argument).Name, 'UNIDADES');
  CheckEqualsString(TTableArgument(FSQLLinker.JoinClause.Items[0].Argument).Alias, 'UN');
  CheckTrue(FSQLLinker.JoinClause.Items[0].JoinOn.ListCriterias.Items[0].LeftArgument.ClassType = TFieldArgument);
  CheckEqualsString(TFieldArgument(FSQLLinker.JoinClause.Items[0].JoinOn.ListCriterias.Items[0].LeftArgument).Name, 'CODUNIDADE');
  CheckEqualsString(TFieldArgument(FSQLLinker.JoinClause.Items[0].JoinOn.ListCriterias.Items[0].LeftArgument).TableAlias, 'UN');

  CheckTrue(FSQLLinker.JoinClause.Items[0].JoinOn.ListCriterias.Items[0].RigthArgument.ClassType = TFieldArgument);
  CheckEqualsString(TFieldArgument(FSQLLinker.JoinClause.Items[0].JoinOn.ListCriterias.Items[0].RigthArgument).Name, 'CODUNIDADE');
  CheckEqualsString(TFieldArgument(FSQLLinker.JoinClause.Items[0].JoinOn.ListCriterias.Items[0].RigthArgument).TableAlias, 'P');
end;

initialization
  RegisterTest(TestTSQLLinker.Suite);

//procedure TestTSQLLinker.TestJoinTable;
//begin
////  FSQLLinker.JoinTable(pName, pJoinOn, pAlias, pJoinKind);
//end;
//
//procedure TestTSQLLinker.TestWhere;
//begin
//  //FSQLLinker.Where(pWhere);
//end;
//
//procedure TestTSQLLinker.TestGroupBy;
//begin
//  //FSQLLinker.GroupBy(pField);
//end;
//
//procedure TestTSQLLinker.TestHaving;
//begin
//  //FSQLLinker.Having(pHaving);
//end;
//
//procedure TestTSQLLinker.TestOrderBy;
//begin
//  //FSQLLinker.OrderBy(pFieldNameOrIndex, pIndex, pTableAlias, pDirective, pOrder);
//end;

end.
