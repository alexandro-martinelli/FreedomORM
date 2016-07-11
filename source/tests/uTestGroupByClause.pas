unit uTestGroupByClause;

{$I FreedomORM.inc}

interface

uses
  TestFramework, AM.Freedom.SQLMappers.Arguments, System.Generics.Collections,
  AM.Freedom.SQLMappers.GroupByClause;

type
  TestTGroupByClause = class(TTestCase)
  strict private
    FGroupByClause: TGroupByClause;
  public
    procedure TearDown; override;
  published
    procedure TestProperties;

  end;

implementation

procedure TestTGroupByClause.TearDown;
begin
  FGroupByClause.Free;
  FGroupByClause := nil;
end;

procedure TestTGroupByClause.TestProperties;
begin
  FGroupByClause := TGroupByClause.Create([TFieldArgument.Create('NOME', 'CLI'),
      TFieldArgument.Create('SOBRENOME', 'CLI')]);
  CheckEquals(2, FGroupByClause.Count);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTGroupByClause.Suite);
end.

