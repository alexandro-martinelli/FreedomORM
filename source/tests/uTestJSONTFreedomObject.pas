unit uTestJSONTFreedomObject;

interface

uses
  TestFramework,
  System.SysUtils,
  System.Classes,
  AM.Freedom.JSONFreedomObject,
  System.JSON,
  AM.Freedom.InterfacedObjects,
  AM.Freedom.IJSONObject,
  Data.DBXJSONReflect,
  uObjectsTests;

type
  TestJSONTFreedomObject = class(TTestCase)
  strict private
    FProduto: TProduto;
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

procedure TestJSONTFreedomObject.SetUp;
begin
  FProduto := TProduto.Create(1);
end;

procedure TestJSONTFreedomObject.TearDown;
begin
  FProduto.Free;
end;

procedure TestJSONTFreedomObject.TestToJSON;
var
  lJSONValue: TJSONValue;
begin
  lJSONValue := FProduto.ToJSON;
  try
    CodeSite.Send(lJSONValue.ToString);
  finally
    lJSONValue.Free;
  end;
end;

procedure TestJSONTFreedomObject.TestFromJSON;
var
  lJSONValue: TJSONValue;
  lProduto: TProduto;
begin
  FProduto.MateriasPrimas.Sort('Id');
  lJSONValue := FProduto.ToJSON;
  lProduto := FProduto.FromJSON<TProduto>(lJSONValue);
  try
    CheckEquals(FProduto.Codigo, lProduto.Codigo);
    CheckEqualsString(FProduto.Descricao, lProduto.Descricao);
    CheckEquals(FProduto.PrecoVenda, lProduto.PrecoVenda);
    CheckEquals(FProduto.DataCadastro, lProduto.DataCadastro);
    CheckEquals(FProduto.DataHoraCadastro, lProduto.DataHoraCadastro);
    CheckEquals(FProduto.Ativo, lProduto.Ativo);
    CheckTrue(FProduto.Tipo = lProduto.Tipo);
    CheckEqualsString(FProduto.Observacao.Text, lProduto.Observacao.Text);
    CheckEquals(FProduto.IdUnidade, lProduto.IdUnidade);
    CheckEquals(FProduto.UnidadeMedida, lProduto.UnidadeMedida);
    CheckEquals(FProduto.AtivoStr, lProduto.AtivoStr);
    CheckTrue(FProduto.ProdutoStr = lProduto.ProdutoStr);
    CheckEquals(FProduto.PrecoVendaNullable.Value, lProduto.PrecoVendaNullable.Value);
    CheckEquals(FProduto.DataCadastroNullable.Value, lProduto.DataCadastroNullable.Value);
    CheckEquals(FProduto.AtivoNullable.Value, lProduto.AtivoNullable.Value);
    CheckEquals(FProduto.AtivoStrNullable.Value, lProduto.AtivoStrNullable.Value);
    CheckEquals(FProduto.IdUnidadeNullable.Value, lProduto.IdUnidadeNullable.Value);
    CheckEqualsString(FProduto.DescricaoUnidade, lProduto.DescricaoUnidade);
    CheckEquals(FProduto.MateriasPrimas.Count, lProduto.MateriasPrimas.Count);
  finally
    lJSONValue.Free;
    lProduto.Free;
  end;
end;

initialization
  //RegisterTest(TestJSONTFreedomObject.Suite);

end.
