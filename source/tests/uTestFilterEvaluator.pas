unit uTestFilterEvaluator;


interface

uses
  TestFramework,
  uObjectsTests,
  AM.Freedom.FilterEvaluator,
  AM.Freedom.ObjectMapper;

type
  TestTFreedomFilterEvaluator = class(TTestCase)
  private
    FObjectMapper: TObjectMapper;
    FProduto: TProduto;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEvaluateExpression;
  end;

implementation

uses
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.EnumerationTypes;

procedure TestTFreedomFilterEvaluator.SetUp;
var
  lParams: TObjectToMapperParams;
begin
  inherited;
  FProduto := TProduto.Create(1);
  lParams := TObjectToMapperParams.Create;
  lParams.ObjectInstance := FProduto;
  lParams.Options := [Properties];
  FObjectMapper := TObjectToMapper.ObjectToMapper(lParams);
end;

procedure TestTFreedomFilterEvaluator.TearDown;
begin
  inherited;
  TObjectToMapper.UnLockMapper(FObjectMapper.GetHashCode);
  FProduto.Free;
end;

procedure TestTFreedomFilterEvaluator.TestEvaluateExpression;
var
  lExpression: string;
  lReturnValue: Boolean;
begin
  lExpression := 'Ativo = True and Descricao like (''%Arr%'')';
  lReturnValue := TFreedomFilterEvaluator.EvaluateExpression(lExpression, FObjectMapper);
  CheckTrue(lReturnValue);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTFreedomFilterEvaluator.Suite);
end.

