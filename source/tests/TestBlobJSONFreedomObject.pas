unit TestBlobJSONFreedomObject;

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
  TBlobJSONFreedomObject = class(TJSONFreedomObject)
  private
    FId: Integer;
    FNome: string;
    FFantasia: string;
    FValorVenda: Extended;
    FMetaVenda: Variant;
    FObservacao: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    property Id: Integer read FId write FId;
    property Nome: string read FNome write FNome;
    property Fantasia: string read FFantasia write FFantasia;
    property ValorVenda: Extended read FValorVenda write FValorVenda;
    property MetaVenda: Variant read FMetaVenda write FMetaVenda;
    property Observacao: TStrings read FObservacao write FObservacao;
  end;

  TestTBlobJSONFreedomObject = class(TTestCase)
  strict private
    FJSONFreedomObject: TBlobJSONFreedomObject;
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

procedure TestTBlobJSONFreedomObject.SetUp;
begin
  FJSONFreedomObject := TBlobJSONFreedomObject.Create;
end;

procedure TestTBlobJSONFreedomObject.TearDown;
begin
  FJSONFreedomObject.Free;
end;

procedure TestTBlobJSONFreedomObject.TestToJSON;
var
  lJSONValue: TJSONValue;
begin
  FJSONFreedomObject.Id := 10;
  FJSONFreedomObject.Nome := 'Alexandro Martinelli';
  FJSONFreedomObject.Fantasia := 'Alex';
  FJSONFreedomObject.ValorVenda := 10000;
  FJSONFreedomObject.MetaVenda := 1000000;
  FJSONFreedomObject.Observacao.Text := 'The best seller of the world!';
  lJSONValue := FJSONFreedomObject.ToJSON;
  try
    CodeSite.Send(lJSONValue.ToString);
  finally
    lJSONValue.Free;
  end;
end;

procedure TestTBlobJSONFreedomObject.TestFromJSON;
var
  lJSONValue: TJSONValue;
  lJSONFreedomObject: TBlobJSONFreedomObject;
begin
  FJSONFreedomObject.Id := 10;
  FJSONFreedomObject.Nome := 'Alexandro Martinelli';
  FJSONFreedomObject.Fantasia := 'Alex';
  FJSONFreedomObject.ValorVenda := 10000;
  FJSONFreedomObject.MetaVenda := 1000000;
  FJSONFreedomObject.Observacao.Text := 'The best seller of the world!';
  lJSONValue := FJSONFreedomObject.ToJSON;
  lJSONFreedomObject := FJSONFreedomObject.FromJSON<TBlobJSONFreedomObject>(lJSONValue);
  try
    CheckEquals(FJSONFreedomObject.Id, lJSONFreedomObject.Id);
    CheckEqualsString(FJSONFreedomObject.Nome, lJSONFreedomObject.Nome);
    CheckEqualsString(FJSONFreedomObject.Fantasia, lJSONFreedomObject.Fantasia);
    CheckEquals(FJSONFreedomObject.ValorVenda, lJSONFreedomObject.ValorVenda);
    CheckTrue(VarSameValue(FJSONFreedomObject.MetaVenda, lJSONFreedomObject.MetaVenda));
    CheckEqualsString(FJSONFreedomObject.Observacao.Text, lJSONFreedomObject.Observacao.Text);
  finally
    lJSONFreedomObject.Free;
    lJSONValue.Free;
  end;
end;

{ TBlobJSONFreedomObject }

constructor TBlobJSONFreedomObject.Create;
begin
  FObservacao := TStringList.Create;
end;

destructor TBlobJSONFreedomObject.Destroy;
begin
  FObservacao.Free;
  inherited;
end;

initialization
//  RegisterTest(TestTBlobJSONFreedomObject.Suite);

end.
