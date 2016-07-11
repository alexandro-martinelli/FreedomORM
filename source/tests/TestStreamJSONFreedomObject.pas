unit TestStreamJSONFreedomObject;

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
  TStreamJSONFreedomObject = class(TJSONFreedomObject)
  private
    FId: Integer;
    FNome: String;
    FFantasia: String;
    FValorVenda: Extended;
    FMetaVenda: Variant;
    FStream: TStream;
  public
    constructor Create;
    destructor Destroy; override;
    property Id: Integer read FId write FId;
    property Nome: String read FNome write FNome;
    property Fantasia: String read FFantasia write FFantasia;
    property ValorVenda: Extended read FValorVenda write FValorVenda;
    property MetaVenda: Variant read FMetaVenda write FMetaVenda;
    property Stream: TStream read FStream;
  end;

  TestTStreamJSONFreedomObject = class(TTestCase)
  strict private
    FJSONFreedomObject: TStreamJSONFreedomObject;
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

procedure TestTStreamJSONFreedomObject.SetUp;
begin
  FJSONFreedomObject := TStreamJSONFreedomObject.Create;
end;

procedure TestTStreamJSONFreedomObject.TearDown;
begin
  FJSONFreedomObject.Free;
end;

procedure TestTStreamJSONFreedomObject.TestToJSON;
var
  lJSONValue: TJSONValue;
  lTexto: TStrings;
begin
  FJSONFreedomObject.Id := 10;
  FJSONFreedomObject.Nome := 'Alexandro Martinelli';
  FJSONFreedomObject.Fantasia := 'Alex';
  FJSONFreedomObject.ValorVenda := 10000;
  FJSONFreedomObject.MetaVenda := 1000000;
  lTexto := TStringList.Create;
  lTexto.Text := 'the best seller of the world!';
  lTexto.SaveToStream(FJSONFreedomObject.Stream);
  lJSONValue := FJSONFreedomObject.ToJSON;
  try
    CodeSite.Send(lJSONValue.ToString);
  finally
    lJSONValue.Free;
    lTexto.Free;
  end;
end;

function TestTStreamJSONFreedomObject.ToByteArray(pStream: TStream): TBytes;
begin
  pStream.Position := 0;
  SetLength(Result, pStream.Size);
  pStream.Read(Result, pStream.size);
end;

procedure TestTStreamJSONFreedomObject.TestFromJSON;
var
  lJSONValue: TJSONValue;
  lJSONFreedomObject: TStreamJSONFreedomObject;
  lTexto: TStrings;
begin
  FJSONFreedomObject.Id := 10;
  FJSONFreedomObject.Nome := 'Alexandro Martinelli';
  FJSONFreedomObject.Fantasia := 'Alex';
  FJSONFreedomObject.ValorVenda := 10000;
  FJSONFreedomObject.MetaVenda := 1000000;
  lTexto := TStringList.Create;
  lTexto.Text := 'the best seller of the world!';
  lTexto.SaveToStream(FJSONFreedomObject.Stream);
  FJSONFreedomObject.Stream.Position := 0;
  lJSONValue := FJSONFreedomObject.ToJSON;
  lJSONFreedomObject := FJSONFreedomObject.FromJSON<TStreamJSONFreedomObject>(lJSONValue);
  try
    CheckEquals(FJSONFreedomObject.Id, lJSONFreedomObject.Id);
    CheckEqualsString(FJSONFreedomObject.Nome, lJSONFreedomObject.Nome);
    CheckEqualsString(FJSONFreedomObject.Fantasia, lJSONFreedomObject.Fantasia);
    CheckEquals(FJSONFreedomObject.ValorVenda, lJSONFreedomObject.ValorVenda);
    CheckTrue(VarSameValue(FJSONFreedomObject.MetaVenda, lJSONFreedomObject.MetaVenda));
    lTexto.LoadFromStream(lJSONFreedomObject.Stream);
    CheckEqualsString('the best seller of the world!', Trim(lTexto.Text));
  finally
    lJSONValue.Free;
    lJSONFreedomObject.Free;
    lTexto.Free;
  end;

end;

{ TStreamJSONFreedomObject }

constructor TStreamJSONFreedomObject.Create;
begin
  FStream := TStringStream.Create;
end;

destructor TStreamJSONFreedomObject.Destroy;
begin
  FStream.Free;
  inherited;
end;

initialization
//  RegisterTest(TestTStreamJSONFreedomObject.Suite);

end.
