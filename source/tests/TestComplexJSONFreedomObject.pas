unit TestComplexJSONFreedomObject;

interface

uses
  TestFramework,
  System.SysUtils,
  AM.Freedom.JSONFreedomObject,
  System.JSON,
  AM.Freedom.InterfacedObjects,
  AM.Freedom.IJSONObject,
  Data.DBXJSONReflect;

type
  TComplexJSONFreedomObject = class(TJSONFreedomObject)
  private
    FId: Integer;
    FNome: String;
    FFantasia: String;
    FValorVenda: Extended;
    FMetaVenda: Variant;
  public
    property Id: Integer read FId write FId;
    property Nome: String read FNome write FNome;
    property Fantasia: String read FFantasia write FFantasia;
    property ValorVenda: Extended read FValorVenda write FValorVenda;
    property MetaVenda: Variant read FMetaVenda write FMetaVenda;
  end;

  TestTComplexJSONFreedomObject = class(TTestCase)
  strict private
    FJSONFreedomObject: TComplexJSONFreedomObject;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestToJSON;
    procedure TestFromJSON;
  end;

implementation

uses
  System.Variants,
  CodeSiteLogging;

procedure TestTComplexJSONFreedomObject.SetUp;
begin
  FJSONFreedomObject := TComplexJSONFreedomObject.Create;
end;

procedure TestTComplexJSONFreedomObject.TearDown;
begin
  FJSONFreedomObject.Free;
end;

procedure TestTComplexJSONFreedomObject.TestToJSON;
var
  lJSONValue: TJSONValue;
  lJSONResult: string;
begin
  FJSONFreedomObject.Id := 10;
  FJSONFreedomObject.Nome := 'Alexandro Martinelli';
  FJSONFreedomObject.Fantasia := 'Alex';
  FJSONFreedomObject.ValorVenda := 10000;
  FJSONFreedomObject.MetaVenda := 1000000;
  lJSONValue := FJSONFreedomObject.ToJSON;
  try
    lJSONResult := '{"type":"TestComplexJSONFreedomObject.TComplexJSONFreedomObject","id":1,"fields":{"FId":10,"FNome":"Alexandro Martinelli","FFantasia":"Alex","FValorVenda":10000,"FMetaVenda":["LongWord","1000000"],"FNotifiers":null}}';
    CodeSite.Send(lJSONValue.ToString);
    CheckEqualsString(lJSONResult, lJSONValue.ToString);
  finally
    lJSONValue.Free;
  end;
end;

procedure TestTComplexJSONFreedomObject.TestFromJSON;
var
  lJSONValue: TJSONValue;
  lJSONFreedomObject: TComplexJSONFreedomObject;
begin
  FJSONFreedomObject.Id := 10;
  FJSONFreedomObject.Nome := 'Alexandro Martinelli';
  FJSONFreedomObject.Fantasia := 'Alex';
  FJSONFreedomObject.ValorVenda := 10000;
  FJSONFreedomObject.MetaVenda := 1000000;
  lJSONValue := FJSONFreedomObject.ToJSON;
  lJSONFreedomObject := FJSONFreedomObject.FromJSON<TComplexJSONFreedomObject>(lJSONValue);
  try
    CheckEquals(FJSONFreedomObject.Id, lJSONFreedomObject.Id);
    CheckEqualsString(FJSONFreedomObject.Nome, lJSONFreedomObject.Nome);
    CheckEqualsString(FJSONFreedomObject.Fantasia, lJSONFreedomObject.Fantasia);
    CheckEquals(FJSONFreedomObject.ValorVenda, lJSONFreedomObject.ValorVenda);
    CheckTrue(VarSameValue(FJSONFreedomObject.MetaVenda, lJSONFreedomObject.MetaVenda));
  finally
    lJSONValue.Free;
    lJSONFreedomObject.Free;
  end;
end;

initialization
  // Register any test cases with the test runner
//  RegisterTest(TestTComplexJSONFreedomObject.Suite);
end.

