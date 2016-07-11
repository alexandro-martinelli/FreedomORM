unit uTestDBPersistent;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  uObjectsTests,
  AM.Freedom.DefaultsClassRegister,
  AM.Freedom.CustomDBPersistent,
  FireDAC.Phys.MSSQL;

type
  TestDBPersistent = class(TTestCase)
  protected
    function GetConnection: TCustomDBPersistent;
  published
    procedure TestUpdateObjectProduto;
    procedure TestUpdateObjectPessoa;
    procedure TestUpdateObjectMovimentoHistorico;
//    procedure TestDropObjectPessoa;
    procedure TestSQLLinker;
    procedure TestGetObjectProduto;
    procedure TestGetObjectProdutoWithUnidade;
    procedure TestAlterTableCommandAddFieldWithDefault;
    procedure TestAlterTableCommandDropFieldWithDefault;
    procedure TestAlterTableCommandAddFieldWithNotNull;
    procedure TestAlterTableCommandDropFieldWithNotNull;
  end;

implementation

uses
  System.SysUtils,
  CodeSiteLogging,
  AM.Freedom.SQLMappers.SQLLinker,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.FreedomObjectList,
  AM.Freedom.SQLCommands.TableCommands,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLCommands.Fields,
  AM.Freedom.EnumerationTypes;

{ TestDBPersistent }

function TestDBPersistent.GetConnection: TCustomDBPersistent;
begin
  Result := TCustomDBPersistent(TDefaultsClassRegister.DefaultPersistent);
end;

procedure TestDBPersistent.TestAlterTableCommandAddFieldWithDefault;
var
  lAlterTable: TAlterTableCommand;
  lField: TCustomFieldCommand;
begin
  lAlterTable := TAlterTableCommand.Create;
  lAlterTable.Name := 'TESTE';
  lField := TIntegerFieldCommand.Create;
  lField.Name := 'TIPO';
  lField.Domain := 'D_ENUMERADO';
  lField.FieldOptions.Default := TValueArgument.CreateAsInteger(1);
  lAlterTable.AddField(lField);
  try
    GetConnection.Execute(lAlterTable);
  except
  end;
end;

procedure TestDBPersistent.TestAlterTableCommandAddFieldWithNotNull;
var
  lAlterTable: TAlterTableCommand;
  lField: TCustomFieldCommand;
begin
  lAlterTable := TAlterTableCommand.Create;
  lAlterTable.Name := 'TESTE';
  lField := TIntegerFieldCommand.Create;
  lField.Name := 'IS_NULLABLE';
  lField.Domain := 'D_ENUMERADO';
  lField.FieldOptions.Nullable := nNotNull;
  lAlterTable.AddField(lField);
  GetConnection.Execute(lAlterTable);
end;

procedure TestDBPersistent.TestAlterTableCommandDropFieldWithDefault;
var
  lAlterTable: TAlterTableCommand;
begin
  {$IFDEF MSSQL}
  ExpectedException := EMSSQLNativeException;
  {$IFEND}
  lAlterTable := TAlterTableCommand.Create;
  lAlterTable.Name := 'TESTE';
  lAlterTable.DropField('TIPO');
  GetConnection.Execute(lAlterTable);
end;

procedure TestDBPersistent.TestAlterTableCommandDropFieldWithNotNull;
var
  lAlterTable: TAlterTableCommand;
begin
  lAlterTable := TAlterTableCommand.Create;
  lAlterTable.Name := 'TESTE';
  lAlterTable.DropField('IS_NULLABLE');
  GetConnection.Execute(lAlterTable);
end;

//procedure TestDBPersistent.TestDropObjectPessoa;
//begin
//  GetConnection.DropObject(TPessoa);
//end;

procedure TestDBPersistent.TestGetObjectProduto;
var
  lProduto: TProduto;
begin
  lProduto := GetConnection.GetObject<TProduto>(1);
  CheckEquals(1, lProduto.Codigo);
end;

procedure TestDBPersistent.TestGetObjectProdutoWithUnidade;
var
  lProduto: TProduto;
  lUnidade: TUnidade;
begin
  lUnidade := GetConnection.GetObject<TUnidade>(1);
  CheckEquals(1, lUnidade.Id);
  lProduto := GetConnection.GetObject<TProduto>(1);
  CheckEquals(1, lProduto.Codigo);
  CheckEqualsString('Arroz', lProduto.Descricao);
end;

procedure TestDBPersistent.TestSQLLinker;
var
  lLinker: TSQLLinker<TProduto>;
  lProdutos: TFreedomObjectList<TProduto>;
begin
  lLinker := GetConnection.CreateLinker<TProduto>;
  try
    lLinker.Where(TCriteria.CreateAsEqual(TFieldArgument.Create('CODUNIDADE', 'P'), TValueArgument.CreateAsInteger(3)));
    lProdutos := lLinker.List;
    try
      CheckEquals(2, lProdutos.Count);
    finally
      lProdutos.Free;
    end;
  finally
    lLinker.Free;
  end;
end;

{$IFNDEF FIREBIRD}

procedure TestDBPersistent.TestUpdateObjectMovimentoHistorico;
begin
  GetConnection.UpdateObject(TDBHistoricoMovimento);
end;

{$IFEND}

procedure TestDBPersistent.TestUpdateObjectPessoa;
begin
  GetConnection.UpdateObject(TPessoa);
end;

procedure TestDBPersistent.TestUpdateObjectProduto;
begin
  GetConnection.UpdateObject(TProduto);
end;

initialization
  RegisterTest(TestDBPersistent.Suite);

end.
