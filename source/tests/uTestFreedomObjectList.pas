unit uTestFreedomObjectList;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  uObjectsTests,
  AM.Freedom.ObjectMapper,
  AM.Freedom.FreedomObjectList,
  AM.Freedom.IPersistent,
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.ObjectMapper.MapperToObject,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.GroupCriteria,
  AM.Freedom.FreedomObject,
  AM.Freedom.CustomFreedomObjectList,
  AM.Freedom.DefaultsClassRegister,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.ObjectMapper.ObjectWriter,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.EnumerationTypes;

type
  TestTFreedomObjectList = class(TTestCase)
  strict private
    FFreedomObjectList: TFreedomObjectList<TProduto>;
    FListaProdutoVenda: TFreedomObjectList<TProdutoVenda>;
    procedure GetObjectsFromCriteria(pCount: Integer = 0; pSchema: String = '');
    procedure GetObjectsFromDeleteCriteria;
    procedure DoInsert(pCount: Integer);
    procedure DoDelete;
    function DoPerformaceTests: Boolean;
    procedure DoLoad(pCount: Integer);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestFindObjectByID;
    procedure TestFindObjectBySimpleProperty;
    procedure TestFindObjectByComplexProperty;
    procedure TestFindObjectByDetailProperty;
    procedure TestFindObjectByEnumerationColumn;
    procedure TestFindObjectByStringColumn;
    procedure TestFindObjectByAtivoDescricao;
    procedure TestFindObjectByAtivoUnidadeId;
    procedure TestFindNext;
    procedure TestResetNext;

    procedure TestSortById;
    procedure TestSortByPrecoVenda;
    procedure TestSortByPrecoVendaDescricao;
    procedure TestSortByPrecoVendaAscDescricaoDesc;


    procedure TestFilter;
    procedure TestFilter2;
    procedure TestFilter3;
    procedure TestFilter4;
    procedure TestFilter5;
    procedure TestFilter6;
    procedure TestFilter7;

    procedure TestDoSearch;
    procedure TestUnpersistModifiedObjects;
    procedure TestUnpersistDeleteObjects;
    procedure TestUnpersistInsertedObjects;
    procedure TestPersistModifiedObjects;
    procedure TestPersistInsertedObjects;
    procedure TestPersistDeletedObjects;
    procedure TestInsert1000;
    procedure TestDelete1000;
    procedure TestInsert2500;
    procedure TestDelete2500;
    procedure TestInsert5000;
    procedure TestDelete5000;
    procedure TestInsert10000;
    procedure TestDelete10000;
    procedure TestInsert15000;
    procedure TestDelete15000;
    procedure TestInsert20000;
    procedure TestDelete20000;

    procedure TestLoad1;
    procedure TestLoad5;
    procedure TestLoad10;
    procedure TestLoad25;
    procedure TestLoad50;
    procedure TestLoad100;
    procedure TestLoad200;
    procedure TestLoad300;
    procedure TestLoad400;
    procedure TestLoad500;
    procedure TestLoad600;
    procedure TestLoad700;
    procedure TestLoad800;
    procedure TestLoad900;
    procedure TestLoad1000;
    procedure TestLoad2000;
    procedure TestLoad3000;
    procedure TestLoad4000;
    procedure TestLoad5000;
    procedure TestLoad10000;
    procedure TestLoad15000;
    procedure TestLoad20000;

    procedure TestGetObjectInAuditoriaForProduto1;
    procedure TestGetAllProdutosVenda;

  end;

implementation

uses
  System.SysUtils, AM.Freedom.GroupFilterCriteria.FilterCriteria, AM.Freedom.GroupFilterCriteria;

procedure TestTFreedomObjectList.DoDelete;
begin
  GetObjectsFromDeleteCriteria;
  FFreedomObjectList.Clear;
  FFreedomObjectList.PersistObjects;
end;

procedure TestTFreedomObjectList.DoInsert(pCount: Integer);
var
  lProduto: TProduto;
  lDescricao: string;
begin
  GetObjectsFromCriteria(10);
  lProduto := FFreedomObjectList.FindObjectByID(1);
  lDescricao := lProduto.Descricao;
  while pCount >= 0 do
  begin
    lProduto.Descricao := lDescricao + ' ' + IntToStr(pCount);
    lProduto.Insert;
    Dec(pCount);
  end;
end;

procedure TestTFreedomObjectList.DoLoad(pCount: Integer);
begin
  if DoPerformaceTests then
  begin
    GetObjectsFromCriteria(pCount);
  end;
end;

function TestTFreedomObjectList.DoPerformaceTests: Boolean;
begin
  Result := False;
end;

procedure TestTFreedomObjectList.GetObjectsFromCriteria(pCount: Integer; pSchema: String);
var
  lGroupCriteria: TGroupCriteria;
begin
  lGroupCriteria := TGroupCriteria.Create;
  lGroupCriteria.LimitRows := pCount;
  lGroupCriteria.SchemaName := pSchema;
  FFreedomObjectList.GetObjects(lGroupCriteria);
end;

procedure TestTFreedomObjectList.GetObjectsFromDeleteCriteria;
var
  lGroupCriteria: TGroupCriteria;
begin
  lGroupCriteria := TGroupCriteria.Create;
  lGroupCriteria.AddCriteria(TCriteria.CreateAsGreaterThanOrEqualTo(TFieldArgument.Create('CODPRODUTO'), TValueArgument.CreateAsInteger(10)));
  FFreedomObjectList.GetObjects(lGroupCriteria);
end;

procedure TestTFreedomObjectList.SetUp;
begin
  FFreedomObjectList := TFreedomObjectList<TProduto>.Create;
  FListaProdutoVenda := TFreedomObjectList<TProdutoVenda>.Create;
end;

procedure TestTFreedomObjectList.TearDown;
begin
  FreeAndNil(FFreedomObjectList);
  FreeAndNil(FListaProdutoVenda);
end;

procedure TestTFreedomObjectList.TestDelete1000;
begin
  if DoPerformaceTests then
  begin
    DoDelete;
  end;
end;

procedure TestTFreedomObjectList.TestDelete10000;
begin
  if DoPerformaceTests then
  begin
    DoDelete;
  end;
end;

procedure TestTFreedomObjectList.TestDelete15000;
begin
  if DoPerformaceTests then
  begin
    DoDelete;
  end;
end;

procedure TestTFreedomObjectList.TestDelete20000;
begin
  if DoPerformaceTests then
  begin
    DoDelete;
  end;
end;

procedure TestTFreedomObjectList.TestDelete2500;
begin
  if DoPerformaceTests then
  begin
    DoDelete;
  end;
end;

procedure TestTFreedomObjectList.TestDelete5000;
begin
  if DoPerformaceTests then
  begin
    DoDelete;
  end;
end;

procedure TestTFreedomObjectList.TestDoSearch;
begin
  GetObjectsFromCriteria(10);
  CheckTrue(FFreedomObjectList.Count >= 9);
end;

procedure TestTFreedomObjectList.TestFilter;
begin
  FFreedomObjectList.GetAllObjects;
  FFreedomObjectList.Filter.AddFilterCriteria(TFilterCriteria.CreateAsContaining('Descricao', 'Tomate', [cssCaseInsensitive]));
  FFreedomObjectList.ApplyFilter;
  CheckEquals(1, FFreedomObjectList.Count);
  FFreedomObjectList.CancelFilter;
end;

procedure TestTFreedomObjectList.TestFilter2;
var
  lProduto: TProduto;
begin
  FFreedomObjectList.GetAllObjects;
  FFreedomObjectList.Sort('Codigo');
  FFreedomObjectList.Filter.AddFilterCriteria(TFilterCriteria.CreateAsContaining('Descricao', 'Tomate', [cssCaseInsensitive]));
  FFreedomObjectList.ApplyFilter;
  CheckEquals(1, FFreedomObjectList.Count);
  FFreedomObjectList.CancelFilter;
  lProduto := FFreedomObjectList.First;
  CheckEquals(1, lProduto.Codigo);
  CheckEqualsString('Arroz', lProduto.Descricao);
  lProduto := FFreedomObjectList.Items[1];
  CheckEquals(2, lProduto.Codigo);
  CheckEqualsString('Feijão', lProduto.Descricao);
end;

procedure TestTFreedomObjectList.TestFilter3;
var
  lProduto: TProduto;
begin
  FFreedomObjectList.GetAllObjects;
  FFreedomObjectList.Sort('Codigo');
  FFreedomObjectList.Filter.AddFilterCriteria(TFilterCriteria.CreateAsEqual('Ativo', False));
  FFreedomObjectList.ApplyFilter;
  CheckEquals(1, FFreedomObjectList.Count);
  lProduto := FFreedomObjectList.First;
  CheckEquals(8, lProduto.Codigo);
  CheckEqualsString('Manjericão', lProduto.Descricao);
  FFreedomObjectList.CancelFilter;
  lProduto := FFreedomObjectList.First;
  CheckEquals(1, lProduto.Codigo);
  CheckEqualsString('Arroz', lProduto.Descricao);
  lProduto := FFreedomObjectList.Items[1];
  CheckEquals(2, lProduto.Codigo);
  CheckEqualsString('Feijão', lProduto.Descricao);
end;

procedure TestTFreedomObjectList.TestFilter4;
var
  lProduto: TProduto;
begin
  FFreedomObjectList.GetAllObjects;
  FFreedomObjectList.Sort('Codigo');
  FFreedomObjectList.Filter.AddFilterCriteria(TFilterCriteria.CreateAsEqual('PrecoVenda', 6));
  FFreedomObjectList.ApplyFilter;
  CheckEquals(2, FFreedomObjectList.Count);
  lProduto := FFreedomObjectList.First;
  CheckEquals(5, lProduto.Codigo);
  CheckEqualsString('Cebola', lProduto.Descricao);
  lProduto := FFreedomObjectList.Items[1];
  CheckEquals(8, lProduto.Codigo);
  CheckEqualsString('Manjericão', lProduto.Descricao);
  FFreedomObjectList.CancelFilter;
  lProduto := FFreedomObjectList.First;
  CheckEquals(1, lProduto.Codigo);
  CheckEqualsString('Arroz', lProduto.Descricao);
  lProduto := FFreedomObjectList.Items[1];
  CheckEquals(2, lProduto.Codigo);
  CheckEqualsString('Feijão', lProduto.Descricao);
end;

procedure TestTFreedomObjectList.TestFilter5;
var
  lProduto: TProduto;
begin
  FFreedomObjectList.GetAllObjects;
  FFreedomObjectList.Sort('Codigo');
  FFreedomObjectList.Filter.AddFilterCriteria(TFilterCriteria.CreateAsEqual('PrecoVenda', 6));
  FFreedomObjectList.Filter.AddFilterCriteria(TFilterCriteria.CreateAsEqual('Ativo', True));
  FFreedomObjectList.ApplyFilter;
  CheckEquals(1, FFreedomObjectList.Count);
  lProduto := FFreedomObjectList.First;
  CheckEquals(5, lProduto.Codigo);
  CheckEqualsString('Cebola', lProduto.Descricao);
  FFreedomObjectList.CancelFilter;
  lProduto := FFreedomObjectList.First;
  CheckEquals(1, lProduto.Codigo);
  CheckEqualsString('Arroz', lProduto.Descricao);
  lProduto := FFreedomObjectList.Items[1];
  CheckEquals(2, lProduto.Codigo);
  CheckEqualsString('Feijão', lProduto.Descricao);
end;

procedure TestTFreedomObjectList.TestFilter6;
var
  lProduto: TProduto;
begin
  FFreedomObjectList.GetAllObjects;
  FFreedomObjectList.Sort('Codigo');
  FFreedomObjectList.Filter.AddFilterCriteria(TFilterCriteria.CreateAsEqual('PrecoVenda', 6));
  FFreedomObjectList.Filter.AddFilterCriteria(TFilterCriteria.CreateAsEqual('PrecoVenda', 5));
  FFreedomObjectList.Filter.Policy := poOr;
  FFreedomObjectList.ApplyFilter;
  CheckEquals(4, FFreedomObjectList.Count);
  lProduto := FFreedomObjectList.First;
  CheckEquals(4, lProduto.Codigo);
  CheckEqualsString('Macarrão', lProduto.Descricao);
  FFreedomObjectList.CancelFilter;
  lProduto := FFreedomObjectList.First;
  CheckEquals(1, lProduto.Codigo);
  CheckEqualsString('Arroz', lProduto.Descricao);
  lProduto := FFreedomObjectList.Items[1];
  CheckEquals(2, lProduto.Codigo);
  CheckEqualsString('Feijão', lProduto.Descricao);
end;

procedure TestTFreedomObjectList.TestFilter7;
var
  lProduto: TProduto;
begin
  FFreedomObjectList.GetAllObjects;
  FFreedomObjectList.Sort('Codigo');
  FFreedomObjectList.Filter.AddFilterCriteria(TFilterCriteria.CreateAsStartingWith('Descricao', 'S'));
  FFreedomObjectList.ApplyFilter;
  CheckEquals(1, FFreedomObjectList.Count);
  lProduto := FFreedomObjectList.First;
  CheckEquals(6, lProduto.Codigo);
  CheckEqualsString('Salsa', lProduto.Descricao);
  FFreedomObjectList.CancelFilter;
  lProduto := FFreedomObjectList.First;
  CheckEquals(1, lProduto.Codigo);
  CheckEqualsString('Arroz', lProduto.Descricao);
  lProduto := FFreedomObjectList.Items[1];
  CheckEquals(2, lProduto.Codigo);
  CheckEqualsString('Feijão', lProduto.Descricao);
end;

procedure TestTFreedomObjectList.TestFindNext;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  FFreedomObjectList.Sort('Codigo');
  lProduto := FFreedomObjectList.FindObjectByPropertyNames('Descricao', 'ma', [fsoCaseInsensitive, fsoContaining]);
  CheckTrue(Assigned(lProduto));
  CheckEquals(3, lProduto.Codigo);
  CheckEqualsString('Tomate Seco', lProduto.Descricao);
  lProduto := FFreedomObjectList.FindNext;
  CheckTrue(Assigned(lProduto));
  CheckEquals(4, lProduto.Codigo);
  CheckEqualsString('Macarrão', lProduto.Descricao);
end;

procedure TestTFreedomObjectList.TestFindObjectByAtivoDescricao;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  lProduto := FFreedomObjectList.FindObjectByPropertyNames(['Ativo', 'PrecoVenda'], [True, 6]);
  CheckTrue(Assigned(lProduto));
  CheckEquals(5, lProduto.Codigo);
  CheckEqualsString('Cebola', lProduto.Descricao);
  CheckEquals(6, lProduto.PrecoVenda);
  CheckTrue(lProduto.Ativo);
end;

procedure TestTFreedomObjectList.TestFindObjectByAtivoUnidadeId;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  lProduto := FFreedomObjectList.FindObjectByPropertyNames(['Ativo', 'Unidade.Id'], [True, 3]);
  CheckTrue(Assigned(lProduto));
  CheckEquals(3, lProduto.Codigo);
  CheckEqualsString('Tomate Seco', lProduto.Descricao);
  CheckEquals(3, lProduto.Unidade.Id);
end;

procedure TestTFreedomObjectList.TestFindObjectByComplexProperty;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  lProduto := FFreedomObjectList.FindObjectByPropertyNames('Unidade.Id', 3);
  CheckTrue(Assigned(lProduto));
  CheckEquals(3, lProduto.Unidade.Id);
  CheckEquals(8, lProduto.Codigo);
end;

procedure TestTFreedomObjectList.TestFindObjectByDetailProperty;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  lProduto := FFreedomObjectList.FindObjectByPropertyNames('MateriasPrimas.MateriaPrima.Codigo', 5);
  CheckTrue(Assigned(lProduto));
  CheckEquals(1, lProduto.Codigo);
end;

procedure TestTFreedomObjectList.TestFindObjectByEnumerationColumn;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  FFreedomObjectList.Sort('Codigo');
  lProduto := FFreedomObjectList.FindObjectByPropertyNames('Tipo', tpProduto);
  CheckTrue(Assigned(lProduto));
  CheckEquals(1, lProduto.Codigo);
  CheckTrue(lProduto.Tipo = tpProduto);
end;

procedure TestTFreedomObjectList.TestFindObjectByID;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  lProduto := FFreedomObjectList.FindObjectByID(2);
  CheckTrue(Assigned(lProduto));
  CheckEquals(2, lProduto.Codigo);
  CheckEqualsString('Feijão', lProduto.Descricao);
end;

procedure TestTFreedomObjectList.TestFindObjectBySimpleProperty;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  lProduto := FFreedomObjectList.FindObjectByPropertyNames('Codigo', 2);
  CheckTrue(Assigned(lProduto));
  CheckEquals(2, lProduto.Codigo);
  CheckEqualsString('Feijão', lProduto.Descricao);
end;

procedure TestTFreedomObjectList.TestFindObjectByStringColumn;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  lProduto := FFreedomObjectList.FindObjectByPropertyNames('Descricao', 'cebol');
  CheckFalse(Assigned(lProduto));
  lProduto := FFreedomObjectList.FindObjectByPropertyNames('Descricao', 'cebol', [fsoCaseInsensitive]);
  CheckFalse(Assigned(lProduto));
  lProduto := FFreedomObjectList.FindObjectByPropertyNames('Descricao', 'cebol', [fsoContaining]);
  CheckFalse(Assigned(lProduto));
  lProduto := FFreedomObjectList.FindObjectByPropertyNames('Descricao', 'cebol', [fsoContaining, fsoCaseInsensitive]);
  CheckTrue(Assigned(lProduto));
  CheckEquals(5, lProduto.Codigo);
  CheckEqualsString('Cebola', lProduto.Descricao);
end;

procedure TestTFreedomObjectList.TestGetAllProdutosVenda;
var
  lProduto: TProdutoVenda;
begin
  FListaProdutoVenda.GetAllObjects;
  CheckEquals(8, FListaProdutoVenda.Count);
  lProduto := FListaProdutoVenda.First;
  CheckTrue(Assigned(lProduto));
  CheckEquals(1, lProduto.Codigo);
  CheckEquals('Arroz', lProduto.Descricao);
  lProduto := FListaProdutoVenda.Last;
  CheckTrue(Assigned(lProduto));
  CheckEquals(9, lProduto.Codigo);
  CheckEquals('Batata', lProduto.Descricao);
end;

procedure TestTFreedomObjectList.TestGetObjectInAuditoriaForProduto1;
var
  lGroupCriteria: TGroupCriteria;
begin
  lGroupCriteria := TGroupCriteria.Create;
  lGroupCriteria.SchemaName := 'auditoria';
  lGroupCriteria.AddCriteria(TCriteria.CreateAsEqual(TFieldArgument.Create('CODPRODUTO'), TValueArgument.CreateAsInteger(1)));
  FFreedomObjectList.GetObjects(lGroupCriteria);
  CheckTrue(FFreedomObjectList.Count > 0);
end;

procedure TestTFreedomObjectList.TestInsert1000;
begin
  if DoPerformaceTests then
  begin
    DoInsert(1000);
  end;
end;

procedure TestTFreedomObjectList.TestInsert10000;
begin
  if DoPerformaceTests then
  begin
    DoInsert(10000);
  end;
end;

procedure TestTFreedomObjectList.TestInsert15000;
begin
  if DoPerformaceTests then
  begin
    DoInsert(15000);
  end;
end;

procedure TestTFreedomObjectList.TestInsert20000;
begin
  if DoPerformaceTests then
  begin
    DoInsert(20000);
  end;
end;

procedure TestTFreedomObjectList.TestInsert2500;
begin
  if DoPerformaceTests then
  begin
    DoInsert(2500);
  end;
end;

procedure TestTFreedomObjectList.TestInsert5000;
begin
  if DoPerformaceTests then
  begin
    DoInsert(5000);
  end;
end;

procedure TestTFreedomObjectList.TestLoad1;
begin
  DoLoad(1);
end;

procedure TestTFreedomObjectList.TestLoad10;
begin
  DoLoad(10);
end;

procedure TestTFreedomObjectList.TestLoad100;
begin
  DoLoad(100);
end;

procedure TestTFreedomObjectList.TestLoad1000;
begin
  DoLoad(1000)
end;

procedure TestTFreedomObjectList.TestLoad10000;
begin
  DoLoad(10000);
end;

procedure TestTFreedomObjectList.TestLoad15000;
begin
  DoLoad(15000);
end;

procedure TestTFreedomObjectList.TestLoad200;
begin
  DoLoad(200);
end;

procedure TestTFreedomObjectList.TestLoad2000;
begin
  DoLoad(2000);
end;

procedure TestTFreedomObjectList.TestLoad20000;
begin
  DoLoad(20000);
end;

procedure TestTFreedomObjectList.TestLoad25;
begin
  DoLoad(25);
end;

procedure TestTFreedomObjectList.TestLoad300;
begin
  DoLoad(300);
end;

procedure TestTFreedomObjectList.TestLoad3000;
begin
  DoLoad(3000);
end;

procedure TestTFreedomObjectList.TestLoad400;
begin
  DoLoad(400);
end;

procedure TestTFreedomObjectList.TestLoad4000;
begin
  DoLoad(4000);
end;

procedure TestTFreedomObjectList.TestLoad5;
begin
  DoLoad(5);
end;

procedure TestTFreedomObjectList.TestLoad50;
begin
  DoLoad(50);
end;

procedure TestTFreedomObjectList.TestLoad500;
begin
  DoLoad(500);
end;

procedure TestTFreedomObjectList.TestLoad5000;
begin
  DoLoad(5000);
end;

procedure TestTFreedomObjectList.TestLoad600;
begin
  DoLoad(600);
end;

procedure TestTFreedomObjectList.TestLoad700;
begin
  DoLoad(700);
end;

procedure TestTFreedomObjectList.TestLoad800;
begin
  DoLoad(800);
end;

procedure TestTFreedomObjectList.TestLoad900;
begin
  DoLoad(900);
end;

procedure TestTFreedomObjectList.TestPersistDeletedObjects;
begin
  GetObjectsFromDeleteCriteria;
  FFreedomObjectList.Clear;
  FFreedomObjectList.PersistObjects;
  GetObjectsFromDeleteCriteria;
  CheckEquals(0, FFreedomObjectList.Count);
end;

procedure TestTFreedomObjectList.TestPersistInsertedObjects;
var
  lProduto: TProduto;
  lCodigo: Integer;
begin
  GetObjectsFromCriteria;
  lProduto := FFreedomObjectList.Insert;
  lProduto.Descricao := 'zTeste';
  FFreedomObjectList.PersistObjects;
  lCodigo := lProduto.Codigo;
  lProduto := TProduto.Create(lCodigo);
  try
    CheckEqualsString('zTeste', lProduto.Descricao);
    lProduto.Delete;
  finally
    lProduto.Free;
  end;
end;

procedure TestTFreedomObjectList.TestPersistModifiedObjects;
var
  lProduto: TProduto;
  lDescricao: String;
begin
  GetObjectsFromCriteria;
  lProduto := FFreedomObjectList.FindObjectByID(3);
  Check(Assigned(lProduto));
  if lProduto.Descricao = 'Tomate' then
  begin
    lDescricao := 'Tomate Seco';
  end
  else
  begin
    lDescricao := 'Tomate';
  end;
  lProduto.Descricao := lDescricao;
  FFreedomObjectList.PersistObjects;
  GetObjectsFromCriteria;
  lProduto := FFreedomObjectList.FindObjectByID(3);
  Check(Assigned(lProduto));
  CheckEqualsString(lDescricao, lProduto.Descricao);
  if lDescricao = 'Tomate' then
  begin
    lProduto.Descricao := 'Tomate Seco';
  end
  else
  begin
    lProduto.Descricao := 'Tomate';
  end;
  FFreedomObjectList.PersistObjects;

end;

procedure TestTFreedomObjectList.TestResetNext;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  FFreedomObjectList.Sort('Codigo');
  lProduto := FFreedomObjectList.FindObjectByPropertyNames('Descricao', 'ma', [fsoCaseInsensitive, fsoContaining]);
  CheckTrue(Assigned(lProduto));
  CheckEquals(3, lProduto.Codigo);
  CheckEqualsString('Tomate Seco', lProduto.Descricao);
  lProduto := FFreedomObjectList.FindNext;
  CheckTrue(Assigned(lProduto));
  CheckEquals(4, lProduto.Codigo);
  CheckEqualsString('Macarrão', lProduto.Descricao);
  FFreedomObjectList.ResetFind;
  lProduto := FFreedomObjectList.FindNext;
  CheckTrue(Assigned(lProduto));
  CheckEquals(3, lProduto.Codigo);
  CheckEqualsString('Tomate Seco', lProduto.Descricao);
end;

procedure TestTFreedomObjectList.TestSortByPrecoVenda;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  FFreedomObjectList.Sort('PrecoVenda');
  lProduto := FFreedomObjectList.Last;
  CheckEquals(2, lProduto.Codigo);
  CheckEqualsString('Feijão', lProduto.Descricao);
  lProduto := FFreedomObjectList.First;
  CheckEquals(1, lProduto.Codigo);
  CheckEqualsString('Arroz', lProduto.Descricao);

end;

procedure TestTFreedomObjectList.TestSortByPrecoVendaAscDescricaoDesc;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  FFreedomObjectList.Sort('PrecoVenda;Descricao', [Asc, Desc]);
  lProduto := FFreedomObjectList.Last;
  CheckEquals(2, lProduto.Codigo);
  CheckEqualsString('Feijão', lProduto.Descricao);
  lProduto := FFreedomObjectList.First;
  CheckEquals(1, lProduto.Codigo);
  CheckEqualsString('Arroz', lProduto.Descricao);
  lProduto := FFreedomObjectList.Items[1];
  CheckEquals(3, lProduto.Codigo);
  CheckEqualsString('Tomate Seco', lProduto.Descricao);
  lProduto := FFreedomObjectList.Items[2];
  CheckEquals(6, lProduto.Codigo);
  CheckEqualsString('Salsa', lProduto.Descricao);

  lProduto := FFreedomObjectList.Items[3];
  CheckEquals(7, lProduto.Codigo);
  CheckEqualsString('Repolho', lProduto.Descricao);
  lProduto := FFreedomObjectList.Items[4];
  CheckEquals(4, lProduto.Codigo);
  CheckEqualsString('Macarrão', lProduto.Descricao);

end;

procedure TestTFreedomObjectList.TestSortByPrecoVendaDescricao;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  FFreedomObjectList.Sort('PrecoVenda;Descricao');
  lProduto := FFreedomObjectList.Last;
  CheckEquals(2, lProduto.Codigo);
  CheckEqualsString('Feijão', lProduto.Descricao);
  lProduto := FFreedomObjectList.First;
  CheckEquals(1, lProduto.Codigo);
  CheckEqualsString('Arroz', lProduto.Descricao);
  lProduto := FFreedomObjectList.Items[1];
  CheckEquals(6, lProduto.Codigo);
  CheckEqualsString('Salsa', lProduto.Descricao);
  lProduto := FFreedomObjectList.Items[2];
  CheckEquals(3, lProduto.Codigo);
  CheckEqualsString('Tomate Seco', lProduto.Descricao);

  lProduto := FFreedomObjectList.Items[3];
  CheckEquals(4, lProduto.Codigo);
  CheckEqualsString('Macarrão', lProduto.Descricao);
  lProduto := FFreedomObjectList.Items[4];
  CheckEquals(7, lProduto.Codigo);
  CheckEqualsString('Repolho', lProduto.Descricao);

end;

procedure TestTFreedomObjectList.TestSortById;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  FFreedomObjectList.Sort('Codigo');
  lProduto := FFreedomObjectList.Last;
  CheckEquals(9, lProduto.Codigo);
  CheckEqualsString('Batata', lProduto.Descricao);
  lProduto := FFreedomObjectList.First;
  CheckEquals(1, lProduto.Codigo);
  CheckEqualsString('Arroz', lProduto.Descricao);
end;

procedure TestTFreedomObjectList.TestUnpersistDeleteObjects;
var
  lProduto: TProduto;
begin
  GetObjectsFromCriteria;
  lProduto := FFreedomObjectList.FindObjectByID(3);
  if Assigned(lProduto) then
  begin
    FFreedomObjectList.Delete(lProduto);
    CheckTrue(TObjectState.Deleted = lProduto.ObjectState);
    CheckTrue(FFreedomObjectList.FindObjectByID(3) = nil);
    FFreedomObjectList.UnpersistObjects;
    CheckTrue(FFreedomObjectList.FindObjectByID(3) <> nil);
  end;
end;

procedure TestTFreedomObjectList.TestUnpersistInsertedObjects;
var
  lCount: Integer;
begin
  GetObjectsFromCriteria;
  lCount := FFreedomObjectList.Count;
  FFreedomObjectList.Insert;
  CheckEquals(lCount + 1, FFreedomObjectList.Count);
  FFreedomObjectList.UnpersistObjects;
  CheckEquals(lCount, FFreedomObjectList.Count);
end;

procedure TestTFreedomObjectList.TestUnpersistModifiedObjects;
var
  lProduto: TProduto;
  lDescricao: String;
begin
  GetObjectsFromCriteria;
  lProduto := FFreedomObjectList.FindObjectByID(3);
  Check(Assigned(lProduto));
  lDescricao := lProduto.Descricao;
  if lProduto.Descricao = 'Tomate' then
  begin
    lProduto.Descricao := 'Tomate Seco';
  end
  else
  begin
    lProduto.Descricao := 'Tomate';
  end;
  FFreedomObjectList.UnpersistObjects;
  CheckEqualsString(lDescricao, lProduto.Descricao);
  CheckTrue(TObjectState.Clean = lProduto.ObjectState);
end;

initialization

// Register any test cases with the test runner
RegisterTest(TestTFreedomObjectList.Suite);

end.
