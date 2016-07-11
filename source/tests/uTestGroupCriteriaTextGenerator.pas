unit uTestGroupCriteriaTextGenerator;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  AM.Freedom.TextGenerator.GroupCriteriaTextGenerator,
  AM.Freedom.GroupCriteria;

type
  TestTGroupCriteriaTextGenerator = class(TTestCase)
  strict private
    FGroupCriteriaTextGenerator: TGroupCriteriaTextGenerator;
    FGroupCriteria: TGroupCriteria;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestOneToOne;
    procedure TestOneToTwo;
    procedure TestTwoToOne;
    procedure TestTwoToTwo;
    procedure TestOneWithTwo;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLMappers.Arguments;

procedure TestTGroupCriteriaTextGenerator.SetUp;
begin
  FGroupCriteriaTextGenerator := TGroupCriteriaTextGenerator.Create;
  FGroupCriteria := TGroupCriteria.Create;
end;

procedure TestTGroupCriteriaTextGenerator.TearDown;
begin
  FreeAndnil(FGroupCriteriaTextGenerator);
  FreeAndnil(FGroupCriteria);
end;

procedure TestTGroupCriteriaTextGenerator.TestOneToOne;
var
  xReturnValue: string;
begin
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro')));
  xReturnValue := FGroupCriteriaTextGenerator.GenerateText(FGroupCriteria);
  CheckEqualsString('(NOME = ''Alexandro'')', xReturnValue);
end;

procedure TestTGroupCriteriaTextGenerator.TestOneToTwo;
var
  xReturnValue: string;
begin
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro')));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));

  xReturnValue := FGroupCriteriaTextGenerator.GenerateText(FGroupCriteria);
  CheckEqualsString('((NOME = ''Alexandro'') and (ID = 10))', xReturnValue);
end;

procedure TestTGroupCriteriaTextGenerator.TestOneWithTwo;
var
  xReturnValue: string;
  aSearch: TGroupCriteria;
begin
  aSearch := FGroupCriteria.AddGroupCriteria;
  aSearch.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro')));
  aSearch.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('SOBRENOME'), TValueArgument.CreateAsString('Martinelli')));
  aSearch := FGroupCriteria.AddGroupCriteria(poOr);
  aSearch.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  aSearch.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(20)));
  xReturnValue := FGroupCriteriaTextGenerator.GenerateText(FGroupCriteria);
  CheckEqualsString('(((NOME = ''Alexandro'') and (SOBRENOME = ''Martinelli'')) and ((ID = 10) or (ID = 20)))', xReturnValue);
end;

procedure TestTGroupCriteriaTextGenerator.TestTwoToOne;
var
  xReturnValue: string;
  aSearch: TGroupCriteria;
begin
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro')));
  aSearch := TGroupCriteria.Create;
  aSearch.Policy := poOr;
  aSearch.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  FGroupCriteria.Add(aSearch);
  xReturnValue := FGroupCriteriaTextGenerator.GenerateText(FGroupCriteria);
  CheckEqualsString('((NOME = ''Alexandro'') and (ID = 10))', xReturnValue);

end;

procedure TestTGroupCriteriaTextGenerator.TestTwoToTwo;
var
  xReturnValue: string;
  aSearch: TGroupCriteria;
begin
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro')));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('SOBRENOME'), TValueArgument.CreateAsString('Martinelli')));
  aSearch := TGroupCriteria.Create;
  aSearch.Policy := poOr;
  aSearch.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  aSearch.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(20)));
  FGroupCriteria.Add(aSearch);
  xReturnValue := FGroupCriteriaTextGenerator.GenerateText(FGroupCriteria);
  CheckEqualsString('(((NOME = ''Alexandro'') and (SOBRENOME = ''Martinelli'')) and ((ID = 10) or (ID = 20)))', xReturnValue);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTGroupCriteriaTextGenerator.Suite);
end.

