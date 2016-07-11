unit uTestSearchCriteria;

{$I FreedomORM.inc}

interface

uses
  TestFramework, AM.Freedom.SearchCriteria,
  AM.Freedom.EnumerationTypes;

type
  TestTSearchCriteria = class(TTestCase)
  strict private
    FSearchCriteria: TSearchCriteria;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

implementation

uses
  AM.Freedom.SearchCriteria.Criteria,
  AM.Freedom.SQLMappers.Arguments, AM.Freedom.SQLMappers.Expressions;

procedure TestTSearchCriteria.SetUp;
begin
  FSearchCriteria := TSearchCriteria.Create;
end;

procedure TestTSearchCriteria.TearDown;
begin
  FSearchCriteria.Free;
  FSearchCriteria := nil;
end;

procedure TestTSearchCriteria.TestProperties;
begin
  CheckEquals(0, FSearchCriteria.LimitRows);
  Check(FSearchCriteria.Policy = poAnd);

  FSearchCriteria.LimitRows := 50;
  FSearchCriteria.Policy := poOrNot;
  CheckEquals(50, FSearchCriteria.LimitRows);
  Check(FSearchCriteria.Policy = poOrNot);

  FSearchCriteria.Add(TSearchCriteria.Create);
  CheckEquals(1, FSearchCriteria.Count);
  FSearchCriteria.ListCriterias.Add(TCriteria.CreateAsDifferent(
    TFieldArgument.Create('NOME', 'CLI', ''),
    TValueArgument.CreateAsString('Alexandro')));
  CheckEquals(1, FSearchCriteria.ListCriterias.Count);
  CheckEqualsString('NOME', TFieldArgument(FSearchCriteria.ListCriterias.Items[0].LeftArgument).Name);
end;

initialization
  RegisterTest(TestTSearchCriteria.Suite);

end.

