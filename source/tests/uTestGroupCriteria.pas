unit uTestGroupCriteria;

{$I FreedomORM.inc}

interface

uses
  TestFramework, AM.Freedom.GroupCriteria,
  AM.Freedom.EnumerationTypes;

type
  TestTGroupCriteria = class(TTestCase)
  strict private
    FGroupCriteria: TGroupCriteria;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

implementation

uses
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMappers.Arguments, AM.Freedom.SQLMappers.Expressions;

procedure TestTGroupCriteria.SetUp;
begin
  FGroupCriteria := TGroupCriteria.Create;
end;

procedure TestTGroupCriteria.TearDown;
begin
  FGroupCriteria.Free;
  FGroupCriteria := nil;
end;

procedure TestTGroupCriteria.TestProperties;
begin
  CheckEquals(0, FGroupCriteria.LimitRows);
  Check(FGroupCriteria.Policy = poAnd);

  FGroupCriteria.LimitRows := 50;
  FGroupCriteria.Policy := poOrNot;
  CheckEquals(50, FGroupCriteria.LimitRows);
  Check(FGroupCriteria.Policy = poOrNot);

  FGroupCriteria.Add(TGroupCriteria.Create);
  CheckEquals(1, FGroupCriteria.Count);
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateAsDifferent(
    TFieldArgument.Create('NOME', 'CLI', ''),
    TValueArgument.CreateAsString('Alexandro')));
  CheckEquals(1, FGroupCriteria.ListCriterias.Count);
  CheckEqualsString('NOME', TFieldArgument(FGroupCriteria.ListCriterias.Items[0].LeftArgument).Name);
end;

initialization
  RegisterTest(TestTGroupCriteria.Suite);

end.

