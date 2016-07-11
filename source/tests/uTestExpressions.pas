unit uTestExpressions;
{$I FreedomORM.inc}

interface

uses
  TestFramework,
  System.Generics.Collections,
  AM.Freedom.Helper.Variant,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLMappers.Expressions,
  AM.Freedom.SQLMappers.CustomExpression,
  AM.Freedom.GroupCriteria,
  AM.Freedom.SQLCommands.Fields,
  AM.Freedom.SQLMappers.FBExpressions,
  AM.Freedom.SQLMappers.MSSQLExpressions;

type
  TCustomTestExpression = class abstract(TTestCase)
  strict private
    FExpression: TCustomExpression;
  strict protected
    function GetExpressionClass: TExpressionClass; virtual; abstract;
    function GetArgumentForTest: TCustomArgument; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties; virtual;
  end;

  TTestSum = class(TCustomTestExpression)
  strict protected
    function GetExpressionClass: TExpressionClass; override;
  end;

  TTestMin = class(TCustomTestExpression)
  strict protected
    function GetExpressionClass: TExpressionClass; override;
  end;

  TTestMax = class(TCustomTestExpression)
  strict protected
    function GetExpressionClass: TExpressionClass; override;
  end;

  TTestAvg = class(TCustomTestExpression)
  strict protected
    function GetExpressionClass: TExpressionClass; override;
  end;

  TTestCount = class(TCustomTestExpression)
  strict protected
    function GetExpressionClass: TExpressionClass; override;
  end;

  TTestUpper = class(TCustomTestExpression)
  strict protected
    function GetExpressionClass: TExpressionClass; override;
  end;

  TTestLower = class(TCustomTestExpression)
  strict protected
    function GetExpressionClass: TExpressionClass; override;
  end;

  TestTCoalesce = class(TTestCase)
  strict private
    FCoalesce: TCoalesce;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTCast = class(TTestCase)
  strict private
    FCast: TCast;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTCalcItem = class(TTestCase)
  strict private
    FCalcItem: TCalcItem;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTCalc = class(TTestCase)
  strict private
    FCalc: TCalc;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTCaseWhen = class(TTestCase)
  strict private
    FCase: TCaseWhen;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTFBList = class(TTestCase)
  strict private
    FFBList: TFBList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTMSSQLDateAdd = class(TTestCase)
  strict private
    FMSSQLDateAdd: TMSSQLDateAdd;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTMSSQLGetDate = class(TTestCase)
  strict private
    FMSSQLGetDate: TMSSQLGetDate;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria.Criteria;

procedure TestTCoalesce.SetUp;
begin
  FCoalesce := TCoalesce.Create(TFieldArgument.Create('NOME', 'CLI'), [TFieldArgument.Create('APELIDO', 'CLI')]);
end;

procedure TestTCoalesce.TearDown;
begin
  FCoalesce.Free;
  FCoalesce := nil;
end;

procedure TestTCoalesce.TestProperties;
begin
  Check(Assigned(FCoalesce));
  CheckEquals(TCoalesce, FCoalesce.ClassType);
  Check(Assigned(FCoalesce.Argument));
  CheckEquals(1, FCoalesce.AnotherArguments.Count);
end;

procedure TestTCast.SetUp;
begin
  FCast := TCast.Create(TFieldArgument.Create('NOME', 'CLI'), TNumericFieldCommand.Create('', 15, 2));
end;

procedure TestTCast.TearDown;
begin
  FCast.Free;
  FCast := nil;
end;

procedure TestTCast.TestProperties;
begin
  Check(Assigned(FCast));
  CheckEquals(TCast, FCast.ClassType);
  Check(Assigned(FCast.Argument));
  CheckEquals(TNumericFieldCommand, FCast.CastAs.ClassType);
end;
{ TCustomTestExpression }

function TCustomTestExpression.GetArgumentForTest: TCustomArgument;
begin
  Result := TFieldArgument.Create('NOME', 'CLI');
end;

procedure TCustomTestExpression.SetUp;
begin
  inherited;
  FExpression := GetExpressionClass.Create(GetArgumentForTest);
end;

procedure TCustomTestExpression.TearDown;
begin
  inherited;
  FreeAndnil(FExpression);
end;

procedure TCustomTestExpression.TestProperties;
begin
  Check(Assigned(FExpression));
  CheckEquals(GetExpressionClass, FExpression.ClassType);
  Check(Assigned(FExpression.Argument));
end;
{ TTestSum }

function TTestSum.GetExpressionClass: TExpressionClass;
begin
  Result := TSum;
end;
{ TTestMin }

function TTestMin.GetExpressionClass: TExpressionClass;
begin
  Result := TMin;
end;
{ TTestMax }

function TTestMax.GetExpressionClass: TExpressionClass;
begin
  Result := TMax;
end;
{ TTestAvg }

function TTestAvg.GetExpressionClass: TExpressionClass;
begin
  Result := TAvg;
end;
{ TTestCount }

function TTestCount.GetExpressionClass: TExpressionClass;
begin
  Result := TCount;
end;
{ TTestUpper }

function TTestUpper.GetExpressionClass: TExpressionClass;
begin
  Result := TUpper;
end;
{ TTestLower }

function TTestLower.GetExpressionClass: TExpressionClass;
begin
  Result := TLower;
end;
{ TestTCalcItem }

procedure TestTCalcItem.SetUp;
begin
  FCalcItem := TCalcItem.Create(TFieldArgument.Create('NOME', 'CLI'));
end;

procedure TestTCalcItem.TearDown;
begin
  FreeAndnil(FCalcItem);
  inherited;
end;

procedure TestTCalcItem.TestProperties;
begin
  Check(Assigned(FCalcItem));
  CheckEquals(TFieldArgument, FCalcItem.Argument.ClassType);
  Check(TCalcExpressionType.Addition = FCalcItem.CalcType);
  FCalcItem.CalcType := TCalcExpressionType.Subtract;
  Check(TCalcExpressionType.Subtract = FCalcItem.CalcType);
end;
{ TestTCalc }

procedure TestTCalc.SetUp;
begin
  inherited;
  FCalc := TCalc.Create;
end;

procedure TestTCalc.TearDown;
begin
  inherited;
  FreeAndnil(FCalc);
end;

procedure TestTCalc.TestProperties;
begin
  FCalc.CalcItems.Add(TCalcItem.Create(TFieldArgument.Create('QTDE', 'PED')));
  FCalc.CalcItems.Add(TCalcItem.Create(TFieldArgument.Create('VALOR_UNITARIO', 'PED'), Multiply));
  CheckEquals(2, FCalc.CalcItems.Count);
  CheckEqualsString('QTDE', TFieldArgument(FCalc.CalcItems.Items[0].Argument).Name);
  CheckEqualsString('VALOR_UNITARIO', TFieldArgument(FCalc.CalcItems.Items[1].Argument).Name);
end;
{ TestTCaseWhen }

procedure TestTCaseWhen.SetUp;
begin
  inherited;
  FCase := TCaseWhen.Create([TCriteria.CreateAsNull(TFieldArgument.Create('NOME', 'CLI'))], TFieldArgument.Create('NOME', 'CLI'), TFieldArgument.Create('APELIDO', 'CLI'));
end;

procedure TestTCaseWhen.TearDown;
begin
  inherited;
  FreeAndnil(FCase);
end;

procedure TestTCaseWhen.TestProperties;
begin
  Check(Assigned(FCase));
  CheckEquals(1, FCase.CaseWhen.ListCriterias.Count);
  CheckEquals(TFieldArgument, FCase.CaseThen.ClassType);
  CheckEquals(TFieldArgument, FCase.CaseElse.ClassType);
end;
{ TestTFBList }

procedure TestTFBList.SetUp;
begin
  inherited;
  FFBList := TFBList.Create(TFieldArgument.Create('NOME', 'CLI'), ',', 'NOMES');
end;

procedure TestTFBList.TearDown;
begin
  inherited;
  FFBList.Free;
end;

procedure TestTFBList.TestProperties;
begin
  CheckTrue(Assigned(FFBList.Argument));
  CheckEquals(TFieldArgument, FFBList.Argument.ClassType);
  CheckEqualsString(',', FFBList.Delimiter);
  CheckEqualsString('NOMES', FFBList.Alias);
end;

{ TestTMSSQLDateAdd }

procedure TestTMSSQLDateAdd.SetUp;
begin
  inherited;
  FMSSQLDateAdd := TMSSQLDateAdd.Create(Minute, 30, TMSSQLGetDate.Create, 'DATA_HORA');
end;

procedure TestTMSSQLDateAdd.TearDown;
begin
  FMSSQLDateAdd.Free;
  inherited;
end;

procedure TestTMSSQLDateAdd.TestProperties;
begin
  Check(Assigned(FMSSQLDateAdd));
  Check(FMSSQLDateAdd.AddType = Minute);
  CheckEquals(30, FMSSQLDateAdd.Increment);
  Check(FMSSQLDateAdd.Argument.ClassType = TMSSQLGetDate);
  CheckEqualsString('DATA_HORA', FMSSQLDateAdd.Alias);
end;

{ TestTMSSQLGetDate }

procedure TestTMSSQLGetDate.SetUp;
begin
  inherited;
  FMSSQLGetDate := TMSSQLGetDate.Create('DATA');
end;

procedure TestTMSSQLGetDate.TearDown;
begin
  inherited;
  FMSSQLGetDate.Free;
end;

procedure TestTMSSQLGetDate.TestProperties;
begin
  Check(Assigned(FMSSQLGetDate));
  CheckEqualsString('DATA', FMSSQLGetDate.Alias);
end;

initialization
  RegisterTest(TTestSum.Suite);
  RegisterTest(TTestMin.Suite);
  RegisterTest(TTestMax.Suite);
  RegisterTest(TTestAvg.Suite);
  RegisterTest(TTestCount.Suite);
  RegisterTest(TTestUpper.Suite);
  RegisterTest(TTestLower.Suite);
  RegisterTest(TestTCoalesce.Suite);
  RegisterTest(TestTCast.Suite);
  RegisterTest(TestTCalcItem.Suite);
  RegisterTest(TestTCalc.Suite);
  RegisterTest(TestTCaseWhen.Suite);
  RegisterTest(TestTFBList.Suite);
  RegisterTest(TestTMSSQLDateAdd.Suite);
  RegisterTest(TestTMSSQLGetDate.Suite);

end.
