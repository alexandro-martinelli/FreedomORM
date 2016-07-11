unit uTestStringsJSONFreedomObject;

interface

uses
  TestFramework,
  System.SysUtils,
  System.Classes,
  AM.Freedom.JSONFreedomObject,
  System.JSON,
  AM.Freedom.InterfacedObjects,
  AM.Freedom.IJSONObject,
  Data.DBXJSONReflect;

type
  TStringsJSONFreedomObject = class(TJSONFreedomObject)
  private
    FId: Integer;
    FNome: String;
    FFantasia: String;
    FValorVenda: Extended;
    FMetaVenda: Variant;
    FStrings: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    property Id: Integer read FId write FId;
    property Nome: String read FNome write FNome;
    property Fantasia: String read FFantasia write FFantasia;
    property ValorVenda: Extended read FValorVenda write FValorVenda;
    property MetaVenda: Variant read FMetaVenda write FMetaVenda;
    property Strings: TStrings read FStrings;
  end;

  TestTStringsJSONFreedomObject = class(TTestCase)
  strict private
    FJSONFreedomObject: TStringsJSONFreedomObject;
  public
    procedure SetUp; override;
    procedure TearDown; override;
    function ToByteArray(pStream: TStream): TBytes;
  published
    procedure TestToJSON;
    procedure TestFromJSON;
  end;

implementation

uses
  System.Variants,
  CodeSiteLogging;

procedure TestTStringsJSONFreedomObject.SetUp;
begin
  FJSONFreedomObject := TStringsJSONFreedomObject.Create;
end;

procedure TestTStringsJSONFreedomObject.TearDown;
begin
  FJSONFreedomObject.Free;
end;

procedure TestTStringsJSONFreedomObject.TestToJSON;
var
  lJSONValue: TJSONValue;
begin
  FJSONFreedomObject.Id := 10;
  FJSONFreedomObject.Nome := 'Alexandro Martinelli';
  FJSONFreedomObject.Fantasia := 'Alex';
  FJSONFreedomObject.ValorVenda := 10000;
  FJSONFreedomObject.MetaVenda := 1000000;
  FJSONFreedomObject.Strings.Text := 'the best seller of the world!';
  lJSONValue := FJSONFreedomObject.ToJSON;
  try
    CodeSite.Send(lJSONValue.ToString);
  finally
    lJSONValue.Free;
  end;
end;

function TestTStringsJSONFreedomObject.ToByteArray(pStream: TStream): TBytes;
begin
  pStream.Position := 0;
  SetLength(Result, pStream.Size);
  pStream.Read(Result, pStream.size);
end;

procedure TestTStringsJSONFreedomObject.TestFromJSON;
var
  lJSONValue: TJSONValue;
  lJSONFreedomObject: TStringsJSONFreedomObject;
begin
  FJSONFreedomObject.Id := 10;
  FJSONFreedomObject.Nome := 'Alexandro Martinelli';
  FJSONFreedomObject.Fantasia := 'Alex';
  FJSONFreedomObject.ValorVenda := 10000;
  FJSONFreedomObject.MetaVenda := 1000000;
  FJSONFreedomObject.Strings.Text := 'the best seller of the world!';

  lJSONValue := FJSONFreedomObject.ToJSON;
  lJSONFreedomObject := FJSONFreedomObject.FromJSON<TStringsJSONFreedomObject>(lJSONValue);
  try
    CheckEquals(FJSONFreedomObject.Id, lJSONFreedomObject.Id);
    CheckEqualsString(FJSONFreedomObject.Nome, lJSONFreedomObject.Nome);
    CheckEqualsString(FJSONFreedomObject.Fantasia, lJSONFreedomObject.Fantasia);
    CheckEquals(FJSONFreedomObject.ValorVenda, lJSONFreedomObject.ValorVenda);
    CheckTrue(VarSameValue(FJSONFreedomObject.MetaVenda, lJSONFreedomObject.MetaVenda));
    CheckEqualsString('the best seller of the world!', Trim(lJSONFreedomObject.Strings.Text));
  finally
    lJSONValue.Free;
    lJSONFreedomObject.Free;
  end;
end;

{ TStringsJSONFreedomObject }

constructor TStringsJSONFreedomObject.Create;
begin
  FStrings := TStringList.Create;
end;

destructor TStringsJSONFreedomObject.Destroy;
begin
  FStrings.Free;
  inherited;
end;

initialization
//  RegisterTest(TestTStringsJSONFreedomObject.Suite);

end.
