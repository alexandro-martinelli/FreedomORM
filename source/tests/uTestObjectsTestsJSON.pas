unit uTestObjectsTestsJSON;

interface

uses
  TestFramework,
  uObjectsTestsJSON,
  System.SysUtils,
  uObjectsTests;

type
  TestTObjectTestJSON = class(TTestCase)
  strict private
    FObjectTestJSON: TObjectTestJSON;
    FProduto: TProduto;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConvert;
    procedure TestConvertFreedomObject;
  end;

implementation

uses
  CodeSiteLogging,
  System.Variants,
  AM.Freedom.JSONConverter;

procedure TestTObjectTestJSON.SetUp;
begin
  FObjectTestJSON := TObjectTestJSON.Create;
  FObjectTestJSON.PropInt := 100000;
  FObjectTestJSON.PropSmallInt := 30000;
  FObjectTestJSON.PropByte := 250;
  FObjectTestJSON.PropString := 'Alexandro Martinelli dá Silva Çetilha';
  FObjectTestJSON.PropChar := #10;
  FObjectTestJSON.PropExtended := 10.00;
  FObjectTestJSON.PropSingle := 1.35;
  FObjectTestJSON.PropCurrency := 10.002;
  FObjectTestJSON.PropDouble := 10.98;
  FObjectTestJSON.PropInt64 := 8898761879;
  FObjectTestJSON.PropVariant := Null;
  FObjectTestJSON.PropDate := Date;
  FObjectTestJSON.PropTime := Time;
  FObjectTestJSON.PropDateTime := Now;
  FObjectTestJSON.PropBoolean := False;
  FObjectTestJSON.PropEnum := etDividir;
  FObjectTestJSON.PropSet := [etDividir, etSingle];
  FObjectTestJSON.PropList.Add('etDividir');
  FObjectTestJSON.PropList.Add('etSingle');
end;

procedure TestTObjectTestJSON.TearDown;
begin
  FreeAndNil(FObjectTestJSON);
end;

procedure TestTObjectTestJSON.TestConvert;
var
  lJSON: String;
begin
  lJSON := TJSONConverter.MakeJSON(FObjectTestJSON);
  CodeSite.Send(lJSON);
end;

procedure TestTObjectTestJSON.TestConvertFreedomObject;
var
  lJSON: String;
begin
  FProduto := TProduto.Create(1);
  lJSON :=TJSONConverter.MakeJSON(FProduto);
  CodeSite.Send(lJSON);
end;

initialization
  //RegisterTest(TestTObjectTestJSON.Suite);

end.
