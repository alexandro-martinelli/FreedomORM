unit uTestExpressionsTextGenerator;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  AM.Freedom.TextGenerator.CustomExpressionTextGenerator,
  AM.Freedom.Helper.CalcExpressionType,
  AM.Freedom.TextGenerator.ExpressionsTextGenerator,
  AM.Freedom.SQLMappers.CustomExpression,
  AM.Freedom.SQLMappers.MSSQLMapper,
  AM.Freedom.SQLMappers.Expressions,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLCommands.Fields,
  AM.Freedom.TextGenerator.FBExpressionsTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.TextGenerator.MSSQLExpressionsTextGenerator,
  AM.Freedom.SQLMappers.MSSQLExpressions;

type
  TCustomTestExpressionTextGenerator = class(TTestCase)
  strict private
    FTextGenerator: TCustomExpressionTextGenerator;
    FExpression: TCustomExpression;
  strict protected
    function CreateFieldArgument: TFieldArgument;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; virtual; abstract;
    function GetExpressionClass: TExpressionClass; virtual; abstract;
    function CreateExpression: TCustomExpression; virtual;
    function GetExpected: String; virtual; abstract;
    function GetTextParams: TGenerateTextParams; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSimpleArgument; virtual;
  end;

  TestTSumTextGenerator = class(TCustomTestExpressionTextGenerator)
  strict protected
    function GetExpected: string; override;
    function GetExpressionClass: TExpressionClass; override;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; override;
  end;

  TestTMinTextGenerator = class(TCustomTestExpressionTextGenerator)
  strict protected
    function GetExpected: string; override;
    function GetExpressionClass: TExpressionClass; override;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; override;
  end;

  TestTMaxTextGenerator = class(TCustomTestExpressionTextGenerator)
  strict protected
    function GetExpected: string; override;
    function GetExpressionClass: TExpressionClass; override;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; override;
  end;

  TestTAvgTextGenerator = class(TCustomTestExpressionTextGenerator)
  strict protected
    function GetExpected: string; override;
    function GetExpressionClass: TExpressionClass; override;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; override;
  end;

  TestTCountTextGenerator = class(TCustomTestExpressionTextGenerator)
  strict protected
    function GetExpected: string; override;
    function GetExpressionClass: TExpressionClass; override;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; override;
  end;

  TestTUpperTextGenerator = class(TCustomTestExpressionTextGenerator)
  strict protected
    function GetExpected: string; override;
    function GetExpressionClass: TExpressionClass; override;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; override;
  end;

  TestTLowerTextGenerator = class(TCustomTestExpressionTextGenerator)
  strict protected
    function GetExpected: string; override;
    function GetExpressionClass: TExpressionClass; override;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; override;
  end;

  TestTCoalesceTextGenerator = class(TCustomTestExpressionTextGenerator)
  strict protected
    function GetExpected: string; override;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; override;
    function CreateExpression: TCustomExpression; override;
  end;

  TestTCastTextGenerator = class(TCustomTestExpressionTextGenerator)
  strict protected
    function GetExpected: string; override;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; override;
    function CreateExpression: TCustomExpression; override;
  end;

  TestTCalcTextGenerator = class(TCustomTestExpressionTextGenerator)
  strict protected
    function GetExpected: string; override;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; override;
    function CreateExpression: TCustomExpression; override;
  end;

  TestTCaseWhenTextGenerator = class(TCustomTestExpressionTextGenerator)
  strict protected
    function GetExpected: string; override;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; override;
    function CreateExpression: TCustomExpression; override;
  end;

  TestFBListTextGenerator = class(TCustomTestExpressionTextGenerator)
  strict protected
    function GetExpected: string; override;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; override;
    function CreateExpression: TCustomExpression; override;
    function GetTextParams: TGenerateTextParams; override;
  published
    procedure TestSimpleArgument; override;
  end;

  TestMSSQLDateAddTextGenerator = class(TCustomTestExpressionTextGenerator)
  strict protected
    function GetExpected: string; override;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; override;
    function CreateExpression: TCustomExpression; override;
    function GetTextParams: TGenerateTextParams; override;
  published
    procedure TestSimpleArgument; override;
  end;

  TestMSSQLGetDateTextGenerator = class(TCustomTestExpressionTextGenerator)
  strict protected
    function GetExpected: string; override;
    function GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass; override;
    function CreateExpression: TCustomExpression; override;
    function GetTextParams: TGenerateTextParams; override;
  published
    procedure TestSimpleArgument; override;
  end;



implementation

uses
  System.SysUtils, AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria.Criteria, AM.Freedom.SQLMappers.FBExpressions;

{ TCustomTextExpressionTextGenerator }

function TCustomTestExpressionTextGenerator.CreateExpression: TCustomExpression;
begin
  Result := GetExpressionClass.Create(CreateFieldArgument);
end;

function TCustomTestExpressionTextGenerator.CreateFieldArgument: TFieldArgument;
begin
  Result := TFieldArgument.Create('NOME', 'CLI');
end;

function TCustomTestExpressionTextGenerator.GetTextParams: TGenerateTextParams;
begin
  Result := nil;
end;

procedure TCustomTestExpressionTextGenerator.SetUp;
begin
  inherited;
  FTextGenerator := GetExpressionTextGeneratorClass.Create;
  FExpression := CreateExpression;
end;

procedure TCustomTestExpressionTextGenerator.TearDown;
begin
  inherited;
  FreeAndNil(FTextGenerator);
  FreeAndNil(FExpression);
end;

procedure TCustomTestExpressionTextGenerator.TestSimpleArgument;
begin
  CheckEqualsString(GetExpected, FTextGenerator.GenerateText(FExpression, GetTextParams));
end;

{ TestTSumTextGenerator }

function TestTSumTextGenerator.GetExpected: string;
begin
  Result := 'sum(CLI.NOME)';
end;

function TestTSumTextGenerator.GetExpressionClass: TExpressionClass;
begin
  Result := TSum;
end;

function TestTSumTextGenerator.GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass;
begin
  Result := TSumTextGenerator;
end;

{ TestTMinTextGenerator }

function TestTMinTextGenerator.GetExpected: string;
begin
  Result := 'min(CLI.NOME)';
end;

function TestTMinTextGenerator.GetExpressionClass: TExpressionClass;
begin
  Result := TMin;
end;

function TestTMinTextGenerator.GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass;
begin
  Result := TMinTextGenerator;
end;

{ TestTMaxTextGenerator }

function TestTMaxTextGenerator.GetExpected: string;
begin
  Result := 'max(CLI.NOME)';
end;

function TestTMaxTextGenerator.GetExpressionClass: TExpressionClass;
begin
  Result := TMax;
end;

function TestTMaxTextGenerator.GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass;
begin
  Result := TMaxTextGenerator;
end;

{ TestTAvgTextGenerator }

function TestTAvgTextGenerator.GetExpected: string;
begin
  Result := 'avg(CLI.NOME)';
end;

function TestTAvgTextGenerator.GetExpressionClass: TExpressionClass;
begin
  Result := TAvg;
end;

function TestTAvgTextGenerator.GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass;
begin
  Result := TAvgTextGenerator;
end;

{ TestTCountTextGenerator }

function TestTCountTextGenerator.GetExpected: string;
begin
  Result := 'count(CLI.NOME)';
end;

function TestTCountTextGenerator.GetExpressionClass: TExpressionClass;
begin
  Result := TCount;
end;

function TestTCountTextGenerator.GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass;
begin
  Result := TCountTextGenerator;
end;

{ TestTUpperTextGenerator }

function TestTUpperTextGenerator.GetExpected: string;
begin
  Result := 'upper(CLI.NOME)';
end;

function TestTUpperTextGenerator.GetExpressionClass: TExpressionClass;
begin
  Result := TUpper;
end;

function TestTUpperTextGenerator.GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass;
begin
  Result := TUpperTextGenerator;
end;

{ TestTLowerTextGenerator }

function TestTLowerTextGenerator.GetExpected: string;
begin
  Result := 'lower(CLI.NOME)';
end;

function TestTLowerTextGenerator.GetExpressionClass: TExpressionClass;
begin
  Result := TLower;
end;

function TestTLowerTextGenerator.GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass;
begin
  Result := TLowerTextGenerator;
end;

{ TestTCoalesceTextGenerator }

function TestTCoalesceTextGenerator.CreateExpression: TCustomExpression;
begin
  Result := TCoalesce.Create(CreateFieldArgument, [TValueArgument.CreateAs<String>('Alexandro')]);
end;

function TestTCoalesceTextGenerator.GetExpected: string;
begin
  Result := 'coalesce(CLI.NOME, ''Alexandro'')';
end;

function TestTCoalesceTextGenerator.GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass;
begin
  Result := TCoalesceTextGenerator;
end;

{ TestTCastTextGenerator }

function TestTCastTextGenerator.CreateExpression: TCustomExpression;
begin
  Result := TCast.Create(CreateFieldArgument, TVarcharFieldCommand.Create('', 150));
end;

function TestTCastTextGenerator.GetExpected: string;
begin
  Result := 'cast(CLI.NOME as varchar(150))';
end;

function TestTCastTextGenerator.GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass;
begin
  Result := TCastTextGenerator;
end;

{ TestTCalcTextGenerator }

function TestTCalcTextGenerator.CreateExpression: TCustomExpression;
begin
  Result := TCalc.Create(CreateFieldArgument, TCalcExpressionType.Multiply, CreateFieldArgument, []);
end;

function TestTCalcTextGenerator.GetExpected: string;
begin
  Result := '(CLI.NOME * CLI.NOME)';
end;

function TestTCalcTextGenerator.GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass;
begin
  Result := TCalcTextGenerator
end;

{ TestTCaseWhenTextGenerator }

function TestTCaseWhenTextGenerator.CreateExpression: TCustomExpression;
begin
  Result := TCaseWhen.Create([TCriteria.CreateAsNull(TFieldArgument.Create('NOME', 'CLI'))],
      TFieldArgument.Create('APELIDO', 'CLI'), TFieldArgument.Create('NOME', 'CLI'));
end;

function TestTCaseWhenTextGenerator.GetExpected: string;
begin
  Result := '(case when (CLI.NOME is null) then CLI.APELIDO else CLI.NOME end)';
end;

function TestTCaseWhenTextGenerator.GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass;
begin
  Result := TCaseWhenTextGenerator;
end;

{ TestFBListTextGenerator }

function TestFBListTextGenerator.CreateExpression: TCustomExpression;
begin
  Result := TFBList.Create(TFieldArgument.Create('ID', 'CLI'), ',', 'IDS');
end;

function TestFBListTextGenerator.GetExpected: string;
begin
  Result := 'list(CLI.ID, '','') as IDS';
end;

function TestFBListTextGenerator.GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass;
begin
  Result := TFBListTextGenerator;
end;

function TestFBListTextGenerator.GetTextParams: TGenerateTextParams;
begin
  Result := TGenerateTextParams.Create(True, False);
end;

procedure TestFBListTextGenerator.TestSimpleArgument;
begin
  {$IFDEF FIREBID}
    inherited;
  {$ENDIF}
end;

{ TestMSSQLDateAddTextGenerator }

function TestMSSQLDateAddTextGenerator.CreateExpression: TCustomExpression;
begin
  Result := TMSSQLDateAdd.Create(Minute, 30, TMSSQLGetDate.Create, 'DATA_HORA');
end;

function TestMSSQLDateAddTextGenerator.GetExpected: string;
begin
  Result := 'dateadd(minute, 30, getdate()) as DATA_HORA';
end;

function TestMSSQLDateAddTextGenerator.GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass;
begin
  Result := TMSSQLDateAddTextGenerator;
end;

function TestMSSQLDateAddTextGenerator.GetTextParams: TGenerateTextParams;
begin
  Result := TGenerateTextParams.Create(True, False);
end;

procedure TestMSSQLDateAddTextGenerator.TestSimpleArgument;
begin
  {$IFDEF MSSQL}
  inherited;
  {$ENDIF}
end;

{ TestMSSQLGetDateTextGenerator }

function TestMSSQLGetDateTextGenerator.CreateExpression: TCustomExpression;
begin
  Result := TMSSQLGetDate.Create('DATA');
end;

function TestMSSQLGetDateTextGenerator.GetExpected: string;
begin
  Result := 'getdate() as DATA';
end;

function TestMSSQLGetDateTextGenerator.GetExpressionTextGeneratorClass: TExpressionTextGeneratorClass;
begin
  Result := TMSSQLGetDateTextGenerator;
end;

function TestMSSQLGetDateTextGenerator.GetTextParams: TGenerateTextParams;
begin
  Result:= TGenerateTextParams.Create(True, False);
end;

procedure TestMSSQLGetDateTextGenerator.TestSimpleArgument;
begin
  {$IFDEF MSSQL}
  inherited;
  {$ENDIF}
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTSumTextGenerator.Suite);
  RegisterTest(TestTMinTextGenerator.Suite);
  RegisterTest(TestTMaxTextGenerator.Suite);
  RegisterTest(TestTAvgTextGenerator.Suite);
  RegisterTest(TestTCountTextGenerator.Suite);
  RegisterTest(TestTUpperTextGenerator.Suite);
  RegisterTest(TestTLowerTextGenerator.Suite);
  RegisterTest(TestTCoalesceTextGenerator.Suite);
  RegisterTest(TestTCastTextGenerator.Suite);
  RegisterTest(TestTCalcTextGenerator.Suite);
  RegisterTest(TestTCaseWhenTextGenerator.Suite);
  RegisterTest(TestFBListTextGenerator.Suite);
  RegisterTest(TestMSSQLDateAddTextGenerator.Suite);
  RegisterTest(TestMSSQLGetDateTextGenerator.Suite);

end.

