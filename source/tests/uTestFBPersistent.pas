unit uTestFBPersistent;

interface

uses
  TestFramework,
  AM.Freedom.Persistent.FBPersistent,
  AM.Freedom.CustomDBPersistent,
  AM.Freedom.SQLMappers.FBSQLMapper,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  uObjectsTests,
  AM.Freedom.DefaultsClassRegister;

type
  TestFBPersistent = class(TTestCase)
  private
    FProduto: TProduto;
  protected
    procedure TearDown; override;
    function GetConnection: TFBPersistent;
  published
    procedure TestCreateProdutoWithID;
    procedure TestAlterProduto;
    procedure TestInsertProduto;
    procedure TestDeleteProduto;
    procedure TestLoadDetails;
    procedure TestPersistDetails;
    procedure TestDeleteWithDetails;
    procedure TestUpdateObjectProduto;
    procedure TestUpdateObjectPessoa;
    procedure TestDropObjectPessoa;
  end;

implementation

uses
  System.SysUtils;

{ TestTFBPersistent }

function TestFBPersistent.GetConnection: TFBPersistent;
begin
  Result := TFBPersistent(TDefaultsClassRegister.DefaultPersistent);
end;

procedure TestFBPersistent.TearDown;
begin
  FreeAndNil(FProduto);
  inherited;
end;

procedure TestFBPersistent.TestAlterProduto;
begin
  FProduto := TProduto.Create(1);
  FProduto.Descricao := 'Arroz';
  FProduto.DataCadastro := Now;
  FProduto.Tipo := TTipoProduto.tpProduto;
  FProduto.Ativo := True;
  FProduto.Unidade := TUnidade.Create(5);
  FProduto.Observacao.Insert(0, '- Alterado por FreedomORM na versão teste em ' + DateTimeToStr(Now));
  FProduto.Update;
end;

procedure TestFBPersistent.TestCreateProdutoWithID;
begin
  FProduto := TProduto.Create(1);
  CheckEqualsString('Arroz', FProduto.Descricao);
  CheckEquals(1, FProduto.Codigo);
  CheckTrue(FProduto.Ativo);
  CheckTrue(FProduto.Tipo = TTipoProduto.tpProduto);
  CheckEquals(5, FProduto.Unidade.Id);
end;

procedure TestFBPersistent.TestDeleteProduto;
begin
  FProduto := TProduto.Create(10);
  FProduto.Delete;
end;

procedure TestFBPersistent.TestDeleteWithDetails;
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

procedure TestFBPersistent.TestDropObjectPessoa;
begin
  GetConnection.DropObject(TPessoa);
end;

procedure TestFBPersistent.TestInsertProduto;
var
  lCodigo: Integer;
  lNow: TDate;
  lDescricao: string;
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
  FProduto.Insert;
  lCodigo := FProduto.Codigo;
  FProduto.Free;
  FProduto := TProduto.Create(lCodigo);
  CheckEquals(lCodigo, FProduto.Codigo);
  CheckEqualsString(lDescricao, FProduto.Descricao);
  CheckEquals(lNow, FProduto.DataCadastro);
  CheckTrue(FProduto.Tipo = TTipoProduto.tpProduto);
  CheckTrue(FProduto.Ativo);
  CheckEquals(12563.21, FProduto.PrecoVenda, 0.01);
  CheckEquals(5, FProduto.Unidade.Id);
end;

procedure TestFBPersistent.TestLoadDetails;
begin
  FProduto := TProduto.Create(1);
  CheckEquals(3, FProduto.MateriasPrimas.Count, Format('Have %d details', [FProduto.MateriasPrimas.Count]));
end;

procedure TestFBPersistent.TestPersistDetails;
var
  lMateriaPrima: TMateriaPrima;
  lCount: Integer;
begin
  FProduto := TProduto.Create(1);
  lCount := FProduto.MateriasPrimas.Count;
  lMateriaPrima := FProduto.MateriasPrimas.Insert;
  lMateriaPrima.MateriaPrima := TProduto.Create(5);
  lMateriaPrima.Qtde := 10;
  FProduto.Update;
  FProduto.Free;
  FProduto := TProduto.Create(1);
  CheckEquals(lCount + 1, FProduto.MateriasPrimas.Count);
  for lMateriaPrima in FProduto.MateriasPrimas do
  begin
    if lMateriaPrima.MateriaPrima.Codigo = 5 then
    begin
      FProduto.MateriasPrimas.Delete(lMateriaPrima);
      Break;
    end;
  end;
  FProduto.Update;
  CheckEquals(lCount, FProduto.MateriasPrimas.Count);
end;

procedure TestFBPersistent.TestUpdateObjectPessoa;
var
  lPessoa: TPessoa;
begin
  GetConnection.UpdateObject(TPessoa);
  lPessoa := TPessoa.Create;
  try
    lPessoa.Nome := 'Alexandro Martinelli' + FormatDateTime('dd/mm/yyy HH:nn:ss:zzz', Now);
    lPessoa.Insert;
    lPessoa.Delete;
  finally
    lPessoa.Free;
  end;
end;

procedure TestFBPersistent.TestUpdateObjectProduto;
begin
  GetConnection.UpdateObject(TProduto);
end;

initialization
  RegisterTest(TestFBPersistent.Suite);

end.
