unit uTestSearchCriteriaTextGenerator;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  AM.Freedom.TextGenerator.SearchCriteriaTextGenerator,
  AM.Freedom.SearchCriteria;

type
  TestTSearchCriteriaTextGenerator = class(TTestCase)
  strict private
    FSearchCriteriaTextGenerator: TSearchCriteriaTextGenerator;
    FSearchCriteria: TSearchCriteria;
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
  AM.Freedom.SearchCriteria.Criteria,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLMappers.Arguments;

procedure TestTSearchCriteriaTextGenerator.SetUp;
begin
  FSearchCriteriaTextGenerator := TSearchCriteriaTextGenerator.Create;
  FSearchCriteria := TSearchCriteria.Create;
end;

procedure TestTSearchCriteriaTextGenerator.TearDown;
begin
  FreeAndnil(FSearchCriteriaTextGenerator);
  FreeAndnil(FSearchCriteria);
end;

procedure TestTSearchCriteriaTextGenerator.TestOneToOne;
var
  xReturnValue: string;
begin
  FSearchCriteria.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro')));
  xReturnValue := FSearchCriteriaTextGenerator.GenerateText(FSearchCriteria);
  CheckEqualsString('(NOME = ''Alexandro'')', xReturnValue);
end;

procedure TestTSearchCriteriaTextGenerator.TestOneToTwo;
var
  xReturnValue: string;
begin
  FSearchCriteria.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro')));
  FSearchCriteria.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));

  xReturnValue := FSearchCriteriaTextGenerator.GenerateText(FSearchCriteria);
  CheckEqualsString('((NOME = ''Alexandro'') and (ID = 10))', xReturnValue);
end;

procedure TestTSearchCriteriaTextGenerator.TestOneWithTwo;
var
  xReturnValue: string;
  aSearch: TSearchCriteria;
begin
  aSearch := FSearchCriteria.AddSearchCriteria;
  aSearch.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro')));
  aSearch.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('SOBRENOME'), TValueArgument.CreateAsString('Martinelli')));
  aSearch := FSearchCriteria.AddSearchCriteria(poOr);
  aSearch.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  aSearch.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(20)));
  xReturnValue := FSearchCriteriaTextGenerator.GenerateText(FSearchCriteria);
  CheckEqualsString('(((NOME = ''Alexandro'') and (SOBRENOME = ''Martinelli'')) and ((ID = 10) or (ID = 20)))', xReturnValue);
end;

procedure TestTSearchCriteriaTextGenerator.TestTwoToOne;
var
  xReturnValue: string;
  aSearch: TSearchCriteria;
begin
  FSearchCriteria.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro')));
  aSearch := TSearchCriteria.Create;
  aSearch.Policy := poOr;
  aSearch.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  FSearchCriteria.Add(aSearch);
  xReturnValue := FSearchCriteriaTextGenerator.GenerateText(FSearchCriteria);
  CheckEqualsString('((NOME = ''Alexandro'') and (ID = 10))', xReturnValue);

end;

procedure TestTSearchCriteriaTextGenerator.TestTwoToTwo;
var
  xReturnValue: string;
  aSearch: TSearchCriteria;
begin
  FSearchCriteria.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro')));
  FSearchCriteria.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('SOBRENOME'), TValueArgument.CreateAsString('Martinelli')));
  aSearch := TSearchCriteria.Create;
  aSearch.Policy := poOr;
  aSearch.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  aSearch.ListCriterias.Add(TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(20)));
  FSearchCriteria.Add(aSearch);
  xReturnValue := FSearchCriteriaTextGenerator.GenerateText(FSearchCriteria);
  CheckEqualsString('(((NOME = ''Alexandro'') and (SOBRENOME = ''Martinelli'')) and ((ID = 10) or (ID = 20)))', xReturnValue);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTSearchCriteriaTextGenerator.Suite);
end.

