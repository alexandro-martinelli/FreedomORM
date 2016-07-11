unit uTestSQLFieldList;

{$I FreedomORM.inc}

interface

uses
  TestFramework, System.Generics.Collections, AM.Freedom.Helper.Variant,
  AM.Freedom.SQLMapper.CustomArgument, AM.Freedom.SQLMappers.Expressions,
  AM.Freedom.SQLMappers.SQLFieldList, AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLMappers.Arguments;

type
  // Test methods for class TSQLFieldList

  TestTSQLFieldList = class(TTestCase)
  strict private
    FSQLFieldList: TSQLFieldList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddField;
    procedure TestAddLiteral;
    procedure TestAddValue;
    procedure TestAddSum;
    procedure TestAddMin;
    procedure TestAddMax;
    procedure TestAddAvg;
    procedure TestAddCount;
    procedure TestAddCoalesce;
    procedure TestAddCast;
    procedure TestCreateWithParams;

  end;

implementation

uses
  System.SysUtils, AM.Freedom.SQLCommands.Fields;

procedure TestTSQLFieldList.SetUp;
begin
  FSQLFieldList := TSQLFieldList.Create;
end;

procedure TestTSQLFieldList.TearDown;
begin
  FSQLFieldList.Free;
  FSQLFieldList := nil;
end;

procedure TestTSQLFieldList.TestAddField;
var
  pAlias: string;
  pTableAlias: string;
  pName: string;
begin
  FSQLFieldList.AddField(pName, pTableAlias, pAlias);
  CheckEquals(1, FSQLFieldList.Count);
  CheckEquals(TFieldArgument, FSQLFieldList.Items[0].ClassType);
end;

procedure TestTSQLFieldList.TestAddLiteral;
var
  pLiteralValue: string;
begin
  FSQLFieldList.AddLiteral(pLiteralValue);
  CheckEquals(1, FSQLFieldList.Count);
  CheckEquals(TLiteralArgument, FSQLFieldList.Items[0].ClassType);
end;

procedure TestTSQLFieldList.TestAddValue;
begin
  FSQLFieldList.AddValue<String>('Alexandro');
  CheckEquals(1, FSQLFieldList.Count);
  CheckEquals(TValueArgument, FSQLFieldList.Items[0].ClassType);
end;

procedure TestTSQLFieldList.TestCreateWithParams;
begin
  FreeAndNil(FSQLFieldList);
  FSQLFieldList := TSQLFieldList.Create([TFieldArgument.Create('NOME', 'CLI'), TValueArgument.CreateAs<Integer>(10)]);
  CheckEquals(2, FSQLFieldList.Count);
  CheckEquals(TFieldArgument, FSQLFieldList.Items[0].ClassType);
  CheckEquals(TValueArgument, FSQLFieldList.Items[1].ClassType);
end;

procedure TestTSQLFieldList.TestAddSum;
begin
  FSQLFieldList.AddSum(TFieldArgument.Create('NOME', 'CLI'));
  CheckEquals(1, FSQLFieldList.Count);
  CheckEquals(TSum, FSQLFieldList.Items[0].ClassType);
end;

procedure TestTSQLFieldList.TestAddMin;
begin
  FSQLFieldList.AddMin(TFieldArgument.Create('NOME', 'CLI'));
  CheckEquals(1, FSQLFieldList.Count);
  CheckEquals(TMin, FSQLFieldList.Items[0].ClassType);
end;

procedure TestTSQLFieldList.TestAddMax;
begin
  FSQLFieldList.AddMax(TFieldArgument.Create('NOME', 'CLI'));
  CheckEquals(1, FSQLFieldList.Count);
  CheckEquals(TMax, FSQLFieldList.Items[0].ClassType);
end;

procedure TestTSQLFieldList.TestAddAvg;
begin
  FSQLFieldList.AddAvg(TFieldArgument.Create('NOME', 'CLI'));
  CheckEquals(1, FSQLFieldList.Count);
  CheckEquals(TAvg, FSQLFieldList.Items[0].ClassType);
end;

procedure TestTSQLFieldList.TestAddCount;
begin
  FSQLFieldList.AddCount(TFieldArgument.Create('NOME', 'CLI'));
  CheckEquals(1, FSQLFieldList.Count);
  CheckEquals(TCount, FSQLFieldList.Items[0].ClassType);
end;

procedure TestTSQLFieldList.TestAddCoalesce;
begin
  FSQLFieldList.AddCoalesce(TFieldArgument.Create('NOME', 'CLI'), [TFieldArgument.Create('SOBRENOME', 'CLI')]);
  CheckEquals(1, FSQLFieldList.Count);
  CheckEquals(TCoalesce, FSQLFieldList.Items[0].ClassType);
end;

procedure TestTSQLFieldList.TestAddCast;
begin
  FSQLFieldList.AddCast(TFieldArgument.Create('NOME', 'CLI'),  TNumericFieldCommand.Create('', 15, 4));
  CheckEquals(1, FSQLFieldList.Count);
  CheckEquals(TCast, FSQLFieldList.Items[0].ClassType);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTSQLFieldList.Suite);
end.

