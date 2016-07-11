unit uTestFreedomObject;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  uObjectsTests;

type
  TestFreedomObject = class(TTestCase)
  private
    FProduto: TProduto;
    FProdutoVenda: TProdutoVenda;
    FCodigoForDelete: Integer;
    function DoCopyFrom(pIgnoreFields: Array of String; pDetails: Boolean = False): TProduto;
  protected
    procedure TearDown; override;
  public
    constructor Create(MethodName: string; RunCount: Int64); override;
  published
    procedure TestCreateProdutoWithID;
    procedure TestCreateProdutoWithID2;
    procedure TestCreateProdutoWithIDInAuditoria;
    procedure TestCopyFrom;
    procedure TestCopyFromWithIgnoreFields;
    procedure TestCopyFromWithDetails;
    procedure TestCopyFromWithIgnoreFieldsAndDetails;
    procedure TestAlterProduto;
    procedure TestInsertProduto;

    procedure TestInsertPessoa;
    procedure TestAlterPessoa;

    procedure TestInsertMovimentoHistorico;

    procedure TestLoadDetails;
    procedure TestPersistDetails;
    procedure TestDeleteWithDetails;
    procedure TestLiveRefresh;
    procedure TestAlterProdutoWithLiveRefresh;
    procedure TestReload;
    procedure TestReloadWithCurrentID;
    procedure TestLazyInExtension;
    procedure TestNoLazyJoinedColumn;
    procedure TestRecreateClass;
    procedure TestLoadCursoredObjectById;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  AM.Freedom.GroupCriteria,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMappers.Arguments;

{ TestTFBPersistent }

constructor TestFreedomObject.Create(MethodName: string; RunCount: Int64);
begin
  inherited;
  FCodigoForDelete := 0;
end;

function TestFreedomObject.DoCopyFrom(pIgnoreFields: array of String; pDetails: Boolean): TProduto;
begin
  FProduto := TProduto.Create(1);
  Result := TProduto.Create;
  try
    Result.CopyFrom(FProduto, pIgnoreFields, pDetails);
    CheckEquals(1, Result.Codigo);
    CheckTrue(Result.Ativo);
    CheckTrue(Result.Tipo = TTipoProduto.tpProduto);
    CheckEquals(5, Result.Unidade.Id);
    CheckTrue(Result.AtivoStr);
    CheckTrue(Result.ProdutoStr = TTipoProduto.tpProduto);
    CheckEqualsString('MM', Result.UnidadeMedida);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TestFreedomObject.TearDown;
begin
  FreeAndNil(FProduto);
  FreeAndNil(FProdutoVenda);
  inherited;
end;

procedure TestFreedomObject.TestAlterPessoa;
var
  lPessoa: TPessoa;
  lIdPessoa: Integer;
  lNome: String;
begin
  lPessoa := TPessoa.Create;
  try
    lNome := 'Alexandro ' + DateTimeToStr(Now);
    lPessoa.Nome := lNome;
    lPessoa.Cidade.Id := 1;
    lPessoa.Insert;
    lIdPessoa := lPessoa.Id;
  finally
    FreeAndNil(lPessoa);
  end;
  lPessoa := TPessoa.Create(lIdPessoa);
  try
    lPessoa.Update;
  finally
    FreeAndNil(lPessoa);
  end;
  lPessoa := TPessoa.Create(lIdPessoa);
  try
    CheckEqualsString(lNome, lPessoa.Nome);
    CheckEquals(lIdPessoa, lPessoa.ID);
    CheckEquals(1, lPessoa.Cidade.Id);
  finally
    FreeAndNil(lPessoa);
  end;
  Sleep(1000);
  lPessoa := TPessoa.Create(lIdPessoa);
  try
    lNome := 'Alexandro ' + DateTimeToStr(Now);
    lPessoa.Nome := lNome;
    lPessoa.Update;
  finally
    FreeAndNil(lPessoa);
  end;
  lPessoa := TPessoa.Create(lIdPessoa);
  try
    CheckEqualsString(lNome, lPessoa.Nome);
    CheckEquals(lIdPessoa, lPessoa.ID);
    CheckEquals(1, lPessoa.Cidade.Id);
  finally
    FreeAndNil(lPessoa);
  end;
end;

procedure TestFreedomObject.TestAlterProduto;
var
  lNow: TDate;
begin
  FProduto := TProduto.Create(1);
  FProduto.Descricao := 'Arroz';
  lNow := Trunc(Now);
  FProduto.DataCadastro := lNow;
  FProduto.Tipo := TTipoProduto.tpProduto;
  FProduto.Ativo := True;
  FProduto.Unidade := TUnidade.Create(5);
  FProduto.Observacao.Insert(0, '- Alterado por FreedomORM na versão teste em ' + DateTimeToStr(Now));
  FProduto.PrecoVendaNullable.Value := 250;
  FProduto.AtivoNullable.Value := True;
  FProduto.AtivoStrNullable.Value := True;
  FProduto.Enderecos.EnderecoCobranca.IdBairro := 2;
  FProduto.Enderecos.EnderecoCobranca.IdCidade := 3;
  FProduto.Enderecos.EnderecoCobranca.Numero := 24;
  FProduto.Enderecos.EnderecoCobranca.Endereco := 'Rua 24 de Novembro, Fundos';
  FProduto.Update;
  FProduto.Free;
  FProduto := TProduto.Create(1);
  CheckEqualsString('Arroz', FProduto.Descricao);
  CheckTrue(lNow = FProduto.DataCadastro);
  CheckTrue(TTipoProduto.tpProduto = FProduto.Tipo);
  CheckTrue(FProduto.Ativo);
  CheckEquals(5, FProduto.IdUnidade);
  CheckEquals(5, FProduto.Unidade.Id);
  CheckEquals(250, FProduto.PrecoVendaNullable.Value);
  CheckFalse(FProduto.PrecoVendaNullable.IsNull);
  CheckTrue(FProduto.AtivoNullable.Value);
  CheckFalse(FProduto.AtivoNullable.IsNull);
  CheckTrue(FProduto.AtivoStrNullable.Value);
  CheckFalse(FProduto.AtivoStrNullable.IsNull);
  CheckEquals(2, FProduto.Enderecos.EnderecoCobranca.IdBairro);
  CheckEquals(3, FProduto.Enderecos.EnderecoCobranca.IdCidade);
  CheckEquals(24, FProduto.Enderecos.EnderecoCobranca.Numero);
  CheckEqualsString('Rua 24 de Novembro, Fundos', FProduto.Enderecos.EnderecoCobranca.Endereco);

  FProduto.PrecoVendaNullable.Clear;
  FProduto.AtivoNullable.Clear;
  FProduto.AtivoStrNullable.Clear;

  FProduto.Enderecos.EnderecoCobranca.IdBairro := 0;
  FProduto.Enderecos.EnderecoCobranca.IdCidade := 0;
  FProduto.Enderecos.EnderecoCobranca.Numero := 0;
  FProduto.Enderecos.EnderecoCobranca.Endereco := '';
  FProduto.Update;
  FProduto.Free;
  FProduto := TProduto.Create(1);
  CheckEquals(0, FProduto.PrecoVendaNullable.Value);
  CheckTrue(FProduto.PrecoVendaNullable.IsNull);
  CheckFalse(FProduto.AtivoNullable.Value);
  CheckTrue(FProduto.AtivoNullable.IsNull);
  CheckEquals(0, FProduto.Enderecos.EnderecoCobranca.IdBairro);
  CheckEquals(0, FProduto.Enderecos.EnderecoCobranca.IdCidade);
  CheckEquals(0, FProduto.Enderecos.EnderecoCobranca.Numero);
  CheckEqualsString('', FProduto.Enderecos.EnderecoCobranca.Endereco);
end;

procedure TestFreedomObject.TestAlterProdutoWithLiveRefresh;
begin
//  FProduto := TProduto.Create(1);
//  FProduto.IdUnidade := 2;
//  CheckEqualsString('Peças', FProduto.Unidade.Descricao);
//  CheckEqualsString('Peças', FProduto.Unidade.Descricao);
//  FProduto.Update;
//  FreeAndNil(FProduto);
//  FProduto := TProduto.Create(1);
//  CheckEqualsString('Peças', FProduto.Unidade.Descricao);
//  FProduto.IdUnidade := 5;
//  CheckEqualsString('Milímetros', FProduto.Unidade.Descricao);
//  CheckEqualsString('Milímetros', FProduto.Unidade.Descricao);
//  FProduto.Update;
end;

procedure TestFreedomObject.TestCopyFrom;
var
  lProduto: TProduto;
begin
  try
    try
      lProduto := DoCopyFrom([]);
      CheckEqualsString('Arroz', lProduto.Descricao);
      CheckEquals(0, lProduto.MateriasPrimas.Count);
    except
      lProduto := nil;
      raise;
    end;
  finally
    FreeAndNil(lProduto);
  end;
end;

procedure TestFreedomObject.TestCopyFromWithDetails;
var
  lProduto: TProduto;
begin
  lProduto := nil;
  try
    lProduto := DoCopyFrom([], True);
    CheckEqualsString('Arroz', lProduto.Descricao);
    CheckEquals(3, lProduto.MateriasPrimas.Count);
  finally
    FreeAndNil(lProduto);
  end;
end;

procedure TestFreedomObject.TestCopyFromWithIgnoreFields;
var
  lProduto: TProduto;
begin
  lProduto := nil;
  try
    lProduto := DoCopyFrom(['DESCRICAO']);
    CheckEqualsString('', lProduto.Descricao);
    CheckEquals(0, lProduto.MateriasPrimas.Count);
  finally
    FreeAndNil(lProduto);
  end;
end;

procedure TestFreedomObject.TestCopyFromWithIgnoreFieldsAndDetails;
var
  lProduto: TProduto;
begin
  lProduto := nil;
  try
    lProduto := DoCopyFrom(['DESCRICAO'], True);
    CheckEqualsString('', lProduto.Descricao);
    CheckEquals(3, lProduto.MateriasPrimas.Count);
  finally
    FreeAndNil(lProduto);
  end;
end;

procedure TestFreedomObject.TestCreateProdutoWithID;
begin
  FProduto := TProduto.Create(1);
  CheckEqualsString('Arroz', FProduto.Descricao);
  CheckEquals(1, FProduto.Codigo);
  CheckTrue(FProduto.Ativo);
  CheckTrue(FProduto.Tipo = TTipoProduto.tpProduto);
  CheckEquals(5, FProduto.Unidade.Id);
  CheckTrue(FProduto.AtivoStr);
  CheckTrue(FProduto.ProdutoStr = TTipoProduto.tpProduto);
  CheckEqualsString('MM', FProduto.UnidadeMedida);
  CheckEqualsString('Milímetros', FProduto.DescricaoUnidade);
end;

procedure TestFreedomObject.TestCreateProdutoWithID2;
begin
  FProduto := TProduto.Create(5);
  CheckEqualsString('Cebola', FProduto.Descricao);
  CheckEquals(5, FProduto.Codigo);
end;


procedure TestFreedomObject.TestCreateProdutoWithIDInAuditoria;
  function CreateGroupCriteriaForAuditoria: TGroupCriteria;
  begin
    Result := TGroupCriteria.Create;
    Result.SchemaName := 'auditoria';
    Result.AddCriteria(TCriteria.CreateAsEqual(TFieldArgument.Create('CODPRODUTO'), TValueArgument.CreateAsInteger(1)));
  end;
begin
  FProduto := TProduto.Create(CreateGroupCriteriaForAuditoria);
end;

procedure TestFreedomObject.TestLoadCursoredObjectById;
begin
  FProdutoVenda := TProdutoVenda.Create(1);
  CheckEquals(1, FProdutoVenda.Codigo);
  CheckEqualsString('Arroz', FProdutoVenda.Descricao);
  CheckEquals(1, FProdutoVenda.PrecoVenda);
  CheckEqualsString('MM', FProdutoVenda.Unidade);
end;

procedure TestFreedomObject.TestDeleteWithDetails;
var
  lCodigo: Integer;
begin
  FProduto := TProduto.Create(1);
  FProduto.MateriasPrimas.FindObjectByID(5);
  FProduto.Descricao := FProduto.Descricao + ' ' + DatetimeToStr(Now);
  FProduto.Insert;
  lCodigo := FProduto.Codigo;
  FProduto.Free;
  FProduto := TProduto.Create(lCodigo);
  FProduto.Delete;
  FProduto.Free;
  FProduto := TProduto.Create(lCodigo);
  CheckEquals(0, FProduto.Codigo);
end;

procedure TestFreedomObject.TestInsertMovimentoHistorico;
var
  lHistorico: TDBHistoricoMovimento;
begin
  lHistorico := TDBHistoricoMovimento.Create;
  try
    lHistorico.IdUsuario := 1;
    lHistorico.DataHora := Now;
    lHistorico.ArquivoXML.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Scripts\PreDefSymbols.xml');
    lHistorico.Insert;
    lHistorico.ArquivoXML.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Scripts\XmlComment.xml');
    lHistorico.Update;
  finally
    lHistorico.Delete;
    lHistorico.Free;
  end;
end;

procedure TestFreedomObject.TestInsertPessoa;
var
  lPessoa: TPessoa;
  lIdPessoa: Integer;
  lNome: String;
begin
  lPessoa := TPessoa.Create;
  try
    lNome := 'Alexandro ' + DateTimeToStr(Now);
    lPessoa.Nome := lNome;
    lPessoa.Cidade.Id := 1;
    lPessoa.Insert;
    lIdPessoa := lPessoa.Id;
  finally
    FreeAndNil(lPessoa);
  end;
  lPessoa := TPessoa.Create(lIdPessoa);
  try
    CheckEqualsString(lNome, lPessoa.Nome);
    CheckEquals(lIdPessoa, lPessoa.ID);
    CheckEquals(1, lPessoa.Cidade.Id);
  finally
    FreeAndNil(lPessoa);
  end;
end;

procedure TestFreedomObject.TestInsertProduto;
var
  lNow: TDate;
  lDescricao: string;
  lObservacao: string;
begin
  FProduto := TProduto.Create(1);
  lDescricao := 'Arroz Integral ' + FormatDateTime('dd/MM/yyyy HH:nn:ss:zzz', Now);
  FProduto.Descricao := lDescricao;
  lNow := Trunc(Now);
  FProduto.DataCadastro := lNow;
  FProduto.Tipo := TTipoProduto.tpProduto;
  FProduto.Ativo := True;
  FProduto.PrecoVenda := 12563.21;
  FProduto.Unidade := TUnidade.Create(5);
  lObservacao := 'Adicionado por FreedomORM em ' + DateTimeToStr(Now);
  FProduto.Observacao.Text := lObservacao;
  FProduto.PrecoVendaNullable.Value := 3500;
  FProduto.AtivoNullable.Value := True;
  FProduto.AtivoStrNullable.Value := False;
  FProduto.Enderecos.EnderecoCobranca.IdBairro := 2;
  FProduto.Enderecos.EnderecoCobranca.IdCidade := 3;
  FProduto.Enderecos.EnderecoCobranca.Numero := 24;
  FProduto.Enderecos.EnderecoCobranca.Endereco := 'Rua 24 de Novembro, Fundos';

  FProduto.Insert;
  FCodigoForDelete := FProduto.Codigo;
  FProduto.Free;
  FProduto := TProduto.Create(FCodigoForDelete);
  CheckEquals(FCodigoForDelete, FProduto.Codigo);
  CheckEqualsString(lDescricao, FProduto.Descricao);
  CheckEquals(lNow, FProduto.DataCadastro);
  CheckTrue(FProduto.Tipo = TTipoProduto.tpProduto);
  CheckTrue(FProduto.Ativo);
  CheckEquals(12563.21, FProduto.PrecoVenda, 0.01, 'Preço de Venda não confere');
  CheckEqualsString(lObservacao, Trim(FProduto.Observacao.Text), 'Observação não confere');
  CheckEquals(5, FProduto.Unidade.Id, 'Unidade não confere');

  CheckEquals(3500, FProduto.PrecoVendaNullable.Value);
  CheckTrue(FProduto.AtivoNullable.Value);
  CheckEquals(1, FProduto.AtivoNullable.InternalValue);
  CheckFalse(FProduto.AtivoStrNullable.Value);
  CheckEquals('N', FProduto.AtivoStrNullable.InternalValue);

  CheckEquals(2, FProduto.Enderecos.EnderecoCobranca.IdBairro);
  CheckEquals(3, FProduto.Enderecos.EnderecoCobranca.IdCidade);
  CheckEquals(24, FProduto.Enderecos.EnderecoCobranca.Numero);
  CheckEqualsString('Rua 24 de Novembro, Fundos', FProduto.Enderecos.EnderecoCobranca.Endereco);
  FProduto.Delete;
end;

procedure TestFreedomObject.TestLazyInExtension;
begin
  FProduto := TProduto.Create(2);
  FProduto.Enderecos.EnderecoCobranca.IdCidade := 6;
  FProduto.Update;
  FProduto.Free;
  FProduto := TProduto.Create(2);
  CheckEquals(6, FProduto.Enderecos.EnderecoCobranca.IdCidade);
  CheckEquals(6, FProduto.Enderecos.EnderecoCobranca.Cidade.Id);
  CheckEqualsString('Cachoerinha', FProduto.Enderecos.EnderecoCobranca.Cidade.Descricao);
end;

procedure TestFreedomObject.TestLiveRefresh;
begin
//  FProduto := TProduto.Create(1);
//  CheckEqualsString('Milímetros', FProduto.Unidade.Descricao);
//  FProduto.IdUnidade := 2;
//  CheckEqualsString('Peças', FProduto.Unidade.Descricao);
//  CheckEqualsString('Peças', FProduto.Unidade.Descricao);
end;

procedure TestFreedomObject.TestLoadDetails;
begin
  FProduto := TProduto.Create(1);
  CheckEquals(3, FProduto.MateriasPrimas.Count, Format('Have %d details', [FProduto.MateriasPrimas.Count]));
end;

procedure TestFreedomObject.TestNoLazyJoinedColumn;
begin
//  FProduto := TProduto.Create(2);
//  CheckEquals(6, FProduto.Cidade.Id);
//  CheckEqualsString('Cachoerinha', FProduto.Cidade.Descricao);
end;

procedure TestFreedomObject.TestPersistDetails;
var
  lMateriaPrima: TMateriaPrima;
  lCount, lDeleteCount: Integer;
  lIndex: Integer;
begin
  lDeleteCount := 0;
  FProduto := TProduto.Create(1);
  lCount := FProduto.MateriasPrimas.Count;
  lMateriaPrima := FProduto.MateriasPrimas.Insert;
  lMateriaPrima.MateriaPrima := TProduto.Create(6);
  lMateriaPrima.Qtde := 10;
  FProduto.Update;
  FProduto.Free;
  FProduto := TProduto.Create(1);
  CheckEquals(lCount + 1, FProduto.MateriasPrimas.Count);
  for lIndex := FProduto.MateriasPrimas.Count - 1 downto 0 do
  begin
    lMateriaprima := FProduto.MateriasPrimas.Items[lIndex];
    if lMateriaPrima.MateriaPrima.Codigo > 5 then
    begin
      FProduto.MateriasPrimas.Delete(lMateriaPrima);
      Inc(lDeleteCount);
    end;
  end;
  CheckTrue(lDeleteCount > 0, 'Não possui itens deletados');
  FProduto.Update;
  lCount := FProduto.MateriasPrimas.Count;
  FProduto.Free;
  FProduto := TProduto.Create(1);
  CheckEquals(lCount, FProduto.MateriasPrimas.Count);
end;

procedure TestFreedomObject.TestRecreateClass;
var
  lIndex: Integer;
begin
  for lIndex := 1 to 10 do
  begin
    FreeAndNil(FProduto);
    FProduto := TProduto.Create(2);
    CheckEquals(0, FProduto.Enderecos.EnderecoEntega.Cidade.Id);
    CheckEquals(6, FProduto.Enderecos.EnderecoCobranca.Cidade.Id);
  end;
end;

procedure TestFreedomObject.TestReload;
begin
  FProduto := TProduto.Create(1);
  CheckEquals(1, FProduto.Codigo);
  FProduto.Codigo := 10;
  CheckEquals(10, FProduto.Codigo);
  FProduto.Reload;
  CheckEquals(1, FProduto.Codigo);
end;

procedure TestFreedomObject.TestReloadWithCurrentID;
begin
  FProduto := TProduto.Create(1);
  CheckEquals(1, FProduto.Codigo);
  FProduto.Codigo := 2;
  CheckEquals(2, FProduto.Codigo);
  CheckEqualsString('Arroz', FProduto.Descricao);
  FProduto.ReloadWithCurrentId;
  CheckEquals(2, FProduto.Codigo);
  CheckEqualsString('Feijão', FProduto.Descricao);
end;

initialization
  RegisterTest(TestFreedomObject.Suite);

end.
