unit TestJSONFreedomObject;

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
  TTestJsonObject = class(TJSONFreedomObject)
  private
    FId: Integer;
    FNome: String;
    FFantasia: String;
  public
    property Id: Integer read FId write FId;
    property Nome: String read FNome write FNome;
    property Fantasia: String read FFantasia write FFantasia;
  end;


  TestTJSONFreedomObject = class(TTestCase)
  strict private
    FTestJsonObject: TTestJsonObject;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestToJSON;
    procedure TestFromJSON;
  end;

implementation

uses
  CodeSiteLogging;

procedure TestTJSONFreedomObject.SetUp;
begin
  FTestJsonObject := TTestJsonObject.Create;
end;

procedure TestTJSONFreedomObject.TearDown;
begin
  FTestJsonObject.Free;
end;

procedure TestTJSONFreedomObject.TestToJSON;
var
  lJSONValue: TJSONValue;
  lJSONResult: string;
begin
  FTestJsonObject.Id := 10;
  FTestJsonObject.Nome := 'Alexandro Martinelli';
  FTestJsonObject.Fantasia := 'Alex';
  lJSONValue := FTestJsonObject.ToJSON;
  try
    lJSONResult := '{"type":"TestJSONFreedomObject.TTestJsonObject","id":1,"fields":{"FId":10,"FNome":"Alexandro Martinelli","FFantasia":"Alex","FNotifiers":null}}';
    CodeSite.Send(lJSONValue.ToString);
    CheckEqualsString(lJSONResult, lJSONValue.ToString);
  finally
    lJSONValue.Free;
  end;
end;

procedure TestTJSONFreedomObject.TestFromJSON;
var
  lJSONValue: TJSONValue;
  lTestJsonObject: TTestJsonObject;
begin
  FTestJsonObject.Id := 10;
  FTestJsonObject.Nome := 'Alexandro Martinelli';
  FTestJsonObject.Fantasia := 'Alex';
  lJSONValue := FTestJsonObject.ToJSON;
  lTestJsonObject := FTestJsonObject.FromJSON<TTestJsonObject>(lJSONValue);
  try
    CheckEquals(FTestJsonObject.Id, lTestJsonObject.Id);
    CheckEqualsString(FTestJsonObject.Nome, lTestJsonObject.Nome);
    CheckEqualsString(FTestJsonObject.Fantasia, lTestJsonObject.Fantasia);
  finally
    lJSONValue.Free;
    lTestJsonObject.Free;
  end;
end;

initialization
  // Register any test cases with the test runner
//  RegisterTest(TestTJSONFreedomObject.Suite);
end.

