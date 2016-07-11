unit uTestSimpleSQLGenerator;

{$I FreedomORM.inc}

interface

uses
  TestFramework, AM.Freedom.GroupCriteria,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLGenerator.SimpleSQLGenerator;

type
  TestTSimpleSQLGenerator = class(TTestCase)
  strict private
    FSQLGenerator: TSimpleSQLGenerator;
    FGroupCriteria: TGroupCriteria;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateSQLTextNoWhere;
    procedure TestGenerateSQLTextWithWhere;

    procedure TestGenerateSQLTextNoWhereGroup;
    procedure TestGenerateSQLTextWithWhereGroup;

    procedure TestGenerateSQLTextNoWhereOrder;
    procedure TestGenerateSQLTextWithWhereOrder;

    procedure TestGenerateSQLTextNoWhereGroupOrder;
    procedure TestGenerateSQLTextWithWhereGroupOrder;

    procedure TestGenerateSQLTextWithExpressions;

    procedure TestLimitRows;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.SQLMappers.MSSQLMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria.Criteria, AM.Freedom.SQLMappers.Expressions;

procedure TestTSimpleSQLGenerator.SetUp;
begin
  FGroupCriteria := TGroupCriteria.Create;
end;

procedure TestTSimpleSQLGenerator.TearDown;
begin
  FreeAndnil(FSQLGenerator);
  FreeAndNil(FGroupCriteria);
end;

procedure TestTSimpleSQLGenerator.TestGenerateSQLTextNoWhere;
var
  lReturnValue: string;
  lSQL: string;
begin
  lSQL := 'SELECT c.* FROM CLIENTES C';
  FSQLGenerator := TSimpleSQLGenerator.Create(lSQL);
  FGroupCriteria.Policy := poOr;
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(20)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(30)));
  lReturnValue := FSQLGenerator.Search(FGroupCriteria);
  CheckEqualsString('select C.* from CLIENTES C where ((ID = 10) or (ID = 20) or (ID = 30))', lReturnValue);
end;

procedure TestTSimpleSQLGenerator.TestGenerateSQLTextNoWhereGroup;
var
  lReturnValue: string;
  lSQL: string;
begin
  lSQL := 'SELECT COUNT(*) FROM CLIENTES GROUP BY 1';
  FSQLGenerator := TSimpleSQLGenerator.Create(lSQL);
  FGroupCriteria.Policy := poOr;
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(20)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(30)));
  lReturnValue := FSQLGenerator.Search(FGroupCriteria);
  CheckEqualsString('select count(*) from CLIENTES where ((ID = 10) or (ID = 20) or (ID = 30)) group by 1', lReturnValue);
end;

procedure TestTSimpleSQLGenerator.TestGenerateSQLTextNoWhereGroupOrder;
var
  lReturnValue: string;
  lSQL: string;
begin
  lSQL := 'SELECT COUNT(*) FROM CLIENTES GROUP BY 1 ORDER BY 1';
  FSQLGenerator := TSimpleSQLGenerator.Create(lSQL);
  FGroupCriteria.Policy := poOr;
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(20)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(30)));
  lReturnValue := FSQLGenerator.Search(FGroupCriteria);
  CheckEqualsString('select count(*) from CLIENTES where ((ID = 10) or (ID = 20) or (ID = 30)) group by 1 order by 1', lReturnValue);
end;

procedure TestTSimpleSQLGenerator.TestGenerateSQLTextNoWhereOrder;
var
  lReturnValue: string;
  lSQL: string;
begin
  lSQL := 'SELECT COUNT(*) FROM CLIENTES ORDER BY 1';
  FSQLGenerator := TSimpleSQLGenerator.Create(lSQL);
  FGroupCriteria.Policy := poOr;
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(20)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(30)));
  lReturnValue := FSQLGenerator.Search(FGroupCriteria);
  CheckEqualsString('select count(*) from CLIENTES where ((ID = 10) or (ID = 20) or (ID = 30)) order by 1', lReturnValue);
end;

procedure TestTSimpleSQLGenerator.TestGenerateSQLTextWithExpressions;
var
  lReturnValue: string;
  lSQL: string;
begin
  lSQL := 'SELECT * FROM CLIENTES WHERE ID IS NOT NULL';
  FSQLGenerator := TSimpleSQLGenerator.Create(lSQL);
  FGroupCriteria.Policy := poAnd;
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(
      TCoalesce.Create(TFieldArgument.Create('NOME'), [TFieldArgument.Create('APELIDO'), TNullArgument.Create]), TValueArgument.CreateAsString('Alexandro Martinelli')));
  lReturnValue := FSQLGenerator.Search(FGroupCriteria);
  CheckEqualsString('select * from CLIENTES where ID is not null and (coalesce(NOME, APELIDO, null) = ''Alexandro Martinelli'')', lReturnValue);
  FGroupCriteria.ListCriterias.Clear;
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(
      TLower.Create(TFieldArgument.Create('NOME')),
      TUpper.Create(TValueArgument.CreateAsString('Alexandro Martinelli'))));
  lReturnValue := FSQLGenerator.Search(FGroupCriteria);
  CheckEqualsString('select * from CLIENTES where ID is not null and (lower(NOME) = upper(''Alexandro Martinelli''))', lReturnValue);
end;

procedure TestTSimpleSQLGenerator.TestGenerateSQLTextWithWhere;
var
  lReturnValue: string;
  lSQL: string;
begin
  lSQL := 'SELECT * FROM CLIENTES WHERE ID IS NOT NULL';
  FSQLGenerator := TSimpleSQLGenerator.Create(lSQL);
  FGroupCriteria.Policy := poOr;
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(20)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(30)));
  lReturnValue := FSQLGenerator.Search(FGroupCriteria);
  CheckEqualsString('select * from CLIENTES where ID is not null and ((ID = 10) or (ID = 20) or (ID = 30))', lReturnValue);
end;

procedure TestTSimpleSQLGenerator.TestGenerateSQLTextWithWhereGroup;
var
  lReturnValue: string;
  lSQL: string;
begin
  lSQL := 'SELECT COUNT(*) FROM CLIENTES WHERE ID IS NOT NULL GROUP BY 1';
  FSQLGenerator := TSimpleSQLGenerator.Create(lSQL);
  FGroupCriteria.Policy := poOr;
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(20)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(30)));
  lReturnValue := FSQLGenerator.Search(FGroupCriteria);
  CheckEqualsString('select count(*) from CLIENTES where ID is not null and ((ID = 10) or (ID = 20) or (ID = 30)) group by 1', lReturnValue);
end;

procedure TestTSimpleSQLGenerator.TestGenerateSQLTextWithWhereGroupOrder;
var
  lReturnValue: string;
  lSQL: string;
begin
  lSQL := 'SELECT COUNT(*) FROM CLIENTES WHERE ID IS NOT NULL GROUP BY 1 ORDER BY 1';
  FSQLGenerator := TSimpleSQLGenerator.Create(lSQL);
  FGroupCriteria.Policy := poOr;
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(20)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(30)));
  lReturnValue := FSQLGenerator.Search(FGroupCriteria);
  CheckEqualsString('select count(*) from CLIENTES where ID is not null and ((ID = 10) or (ID = 20) or (ID = 30)) group by 1 order by 1', lReturnValue);
end;

procedure TestTSimpleSQLGenerator.TestGenerateSQLTextWithWhereOrder;
var
  lReturnValue: string;
  lSQL: string;
begin
  lSQL := 'SELECT COUNT(*) FROM CLIENTES WHERE ID IS NOT NULL ORDER BY 1';
  FSQLGenerator := TSimpleSQLGenerator.Create(lSQL);
  FGroupCriteria.Policy := poOr;
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(20)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(30)));
  lReturnValue := FSQLGenerator.Search(FGroupCriteria);
  CheckEqualsString('select count(*) from CLIENTES where ID is not null and ((ID = 10) or (ID = 20) or (ID = 30)) order by 1', lReturnValue);
end;

procedure TestTSimpleSQLGenerator.TestLimitRows;
var
  lReturnValue: string;
  lSQL: string;
begin
  lSQL := 'SELECT COUNT(*) FROM CLIENTES WHERE ID IS NOT NULL ORDER BY 1';
  FSQLGenerator := TSimpleSQLGenerator.Create(lSQL);
  FGroupCriteria.Policy := poOr;
  FGroupCriteria.LimitRows := 100;
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(10)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(20)));
  FGroupCriteria.ListCriterias.Add(TCriteria.CreateasEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(30)));
  lReturnValue := FSQLGenerator.Search(FGroupCriteria);
  CheckEqualsString('select' + {$IFDEF MSSQL} ' top 100' + {$ELSEIF Defined(FIREBIRD)} ' first 100' + {$IFEND}
      ' count(*) from CLIENTES where ID is not null and ((ID = 10) or (ID = 20) or (ID = 30))' +
      ' order by 1' {$IFDEF POSTGRE} + ' limit 100' {$ENDIF}, lReturnValue);
end;

initialization

// Register any test cases with the test runner
RegisterTest(TestTSimpleSQLGenerator.Suite);

end.
