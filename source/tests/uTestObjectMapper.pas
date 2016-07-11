unit uTestObjectMapper;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  System.Classes,
  AM.Freedom.ObjectMapper.CustomMapper,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper,
  AM.Freedom.Attributes,
  AM.Freedom.ObjectMapper.ColumnMappers,
  uObjectsTests,
  AM.Freedom.Consts;

type
  TestTObjectMapper = class(TTestCase)
  strict private
    FObjectMapper: TObjectMapper;
    FTest: TProduto;
    FColumn: TCustomColumnMapper;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNumberOfColumnsMethodsTriggers;
    procedure TestMethods;
    procedure TestTriggers;
    procedure TestColumns;
    procedure TestIDOptions;
    procedure TestOrderOptions;
    procedure TestDefaultValueOptionsWithColumnMapper;
    procedure TestDefaultValueOptionsWithBooleanColumnMapper;
    procedure TestEnumerationColumnMapper;
    //procedure TestAutoMapping;
    procedure TestLazyOptionsWithJoinColumnMapper;
    procedure TestNullableColumns;
    procedure TestRoundedColumns;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.ObjectMapper.ObjectToMapper;

procedure TestTObjectMapper.SetUp;
var
  lParams: TObjectToMapperParams;
begin
  FTest := TProduto.Create;
  lParams := TObjectToMapperParams.Create;
  try
    lParams.ObjectInstance := FTest;
    FObjectMapper := TObjectToMapper.ObjectToMapper(lParams);
  except
    FreeAndNil(FTest);
    raise;
  end;
end;

procedure TestTObjectMapper.TearDown;
begin
  TObjectToMapper.UnlockMapper(FObjectMapper.GetHashCode);
  FreeAndNil(FTest);
end;

procedure TestTObjectMapper.TestNullableColumns;
var
  lColumn: TCustomColumnMapper;
  lObjectMapper: TObjectMapper;
  lProduto: TProduto;
  lParams: TObjectToMapperParams;
begin
  lProduto := TProduto.Create;
  lParams := TObjectToMapperParams.Create;
  try
    lParams.ObjectInstance := lProduto;
    lObjectMapper := TObjectToMapper.ObjectToMapper(lParams);
    try
      CheckEquals(7, lObjectMapper.Columns.NullableColumns.Count);
      for lColumn in lObjectMapper.Columns.NullableColumns do
      begin
        CheckTrue(lColumn.IsNullable);
      end;
    finally
      TObjectToMapper.UnlockMapper(lObjectMapper.GetHashCode);
      lProduto.Free;
    end;
  except
    FreeAndNil(lProduto);
    raise;
  end;
end;

procedure TestTObjectMapper.TestNumberOfColumnsMethodsTriggers;
begin
  CheckEquals({$IFNDEF FIREBIRD} 42 {$ELSE} 37 {$IFEND}, FObjectMapper.Columns.Count);
  CheckEquals(1, FObjectMapper.Methods.Count);
  CheckEquals(2, FObjectMapper.Triggers.Count);
end;

//procedure TestTObjectMapper.TestAutoMapping;
//begin
//  FColumn := FObjectMapper.Columns.FindColumn('DATAULTALTERACAO');
//  Check(Assigned(FColumn), 'DATAULTALTERACAO not found');
//  CheckEquals(TCustomColumnMapper, FColumn.ClassType);
//  CheckEquals('DataUltAlteracao', FColumn.Name);
//  CheckEquals(0, TCustomColumnMapper(FColumn).Size);
//  Check(FColumn.ColumnOptions = []);
//end;

procedure TestTObjectMapper.TestColumns;
var
  lColumn: TCustomColumnMapper;
begin
  FColumn := FObjectMapper.Columns.FindColumn('FEnderecoCobranca');
  Check(Assigned(FColumn), 'FEnderecoCobranca not found');
  CheckEquals(0, FColumn.Schemas.Count);
  CheckTrue(FColumn.ColumnType = ctyExtension);
  CheckTrue(FColumn.ClassType = TExtensionColumnMapper);
  CheckEquals(5, TExtensionColumnMapper(FColumn).ChildColumns.Count);
  lColumn := TExtensionColumnMapper(FColumn).ChildColumns.FindColumn('ENDERECO_COB');
  CheckTrue(Assigned(lColumn), 'ENDERECO_COB not found');
  FColumn := FObjectMapper.Columns.FindColumn('ENDERECO_COB');
  CheckTrue(Assigned(FColumn), 'ENDERECO_COB not found');
  CheckTrue(lColumn = FColumn, 'lColumn is diferrent object to FColumn');
  CheckEquals({$IFNDEF FIREBIRD} 2 {$ELSE} 0 {$IFEND}, lColumn.Schemas.Count);
  {$IFNDEF FIREBIRD}
  FColumn := FObjectMapper.Columns.FindColumn('ATIVO_BOOL');
  CheckTrue(Assigned(FColumn), 'ATIVO_BOOL not found');
  CheckTrue(TBooleanColumnMapper(FColumn).InternalColumnType = ctyBoolean, 'ATIVO_BOOL is not a internal column type Boolean');
  CheckEquals(2, lColumn.Schemas.Count);
  {$IFEND}



end;

procedure TestTObjectMapper.TestDefaultValueOptionsWithBooleanColumnMapper;
begin
  FColumn := FObjectMapper.Columns.FindColumn('ATIVO');
  Check(Assigned(FColumn), 'ATIVO not found');
  CheckEquals(TBooleanColumnMapper, FColumn.ClassType);
  CheckEquals('ATIVO', FColumn.Name);
  Check(FColumn.DefaultValueOptions.Value);
end;

procedure TestTObjectMapper.TestDefaultValueOptionsWithColumnMapper;
begin
  FColumn := FObjectMapper.Columns.FindColumn('DATAHORACADASTRO');
  Check(Assigned(FColumn), 'DATAHORACADASTRO not found');
  CheckEquals(TCustomColumnMapper, FColumn.ClassType);
  CheckEquals('DATAHORACADASTRO', FColumn.Name);
  Check(FColumn.DefaultValueOptions.IsNow);
end;

procedure TestTObjectMapper.TestEnumerationColumnMapper;
begin
  FColumn := FObjectMapper.Columns.FindColumn('CODTIPO_PRODUTO');
  Check(Assigned(FColumn), 'CODTIPO_PRODUTO not found');
  CheckEquals(TEnumerationColumnMapper, FColumn.ClassType);
  CheckEquals('CODTIPO_PRODUTO', FColumn.Name);
  CheckTrue(emByte = TEnumerationColumnMapper(FColumn).EnumType);
end;

procedure TestTObjectMapper.TestIDOptions;
begin
  FColumn := FObjectMapper.Columns.FindColumn('CODPRODUTO');
  Check(Assigned(FColumn), 'CODPRODUTO not found');
  CheckEquals(TCustomColumnMapper, FColumn.ClassType);
  CheckEquals('CODPRODUTO', FColumn.Name);
  CheckEquals(0, TCustomColumnMapper(FColumn).Size);
  CheckEqualsString({$IFNDEF FIREBIRD} '' {$ELSE} 'SEQ_PRODUTOS' {$IFEND}, FColumn.IdOptions.SequenceName);
  Check(FColumn.IdOptions.IdOption = {$IFNDEF FIREBIRD} Identity {$ELSE} Sequence {$IFEND});
  Check(FColumn.ColumnOptions = [Required]);
end;

procedure TestTObjectMapper.TestLazyOptionsWithJoinColumnMapper;
begin
  FColumn := FObjectMapper.Columns.FindColumn('CODUNIDADE', ctyJoin);
  Check(Assigned(FColumn), 'CODUNIDADE not found');
  CheckEquals(TJoinedColumnMapper, FColumn.ClassType);
  CheckEquals('CODUNIDADE', FColumn.Name);
  Check(FColumn.LazyOptions.IsLazy);
  Check(not FColumn.LazyOptions.IsLazyLoaded);
  CheckEquals(TUnidade,  FColumn.LazyOptions.LazyClassType);
end;

procedure TestTObjectMapper.TestMethods;
begin
  CheckEquals(1, FObjectMapper.Methods.Count);
  CheckEquals(TAssignControl, FObjectMapper.Methods.Items[0].MethodControlClass);
  Check(FObjectMapper.Methods.Items[0].Options = TConsts.cMethodAll);
end;

procedure TestTObjectMapper.TestOrderOptions;
begin
  FColumn := FObjectMapper.Columns.FindColumn('DESCRICAO');
  Check(Assigned(FColumn), 'DESCRICAO not found');
  CheckEquals(TCustomColumnMapper, FColumn.ClassType);
  CheckEquals('DESCRICAO', FColumn.Name);
  CheckEquals(1, FColumn.OrderOptions.Index);
  CheckEquals(150, TCustomColumnMapper(FColumn).Size);
  Check(FColumn.ColumnOptions = [Required]);
end;

procedure TestTObjectMapper.TestRoundedColumns;
begin
  FColumn := FObjectMapper.Columns.FindColumn('PRECO_VENDA');
  Check(Assigned(FColumn), 'PRECO_VENDA not found');
  CheckEquals(TCustomColumnMapper, FColumn.ClassType);
  CheckEquals('PRECO_VENDA', FColumn.Name);
  CheckTrue(FColumn.RoundOptions.IsRounded);
  CheckEquals(2, FColumn.RoundOptions.RoundDecimals);
  CheckTrue(FColumn.RoundOptions.RoundDecimalsMode = rdmApproach);
  CheckTrue(FColumn.RoundOptions.CanBeModified);
end;

procedure TestTObjectMapper.TestTriggers;
begin
  CheckEquals(2, FObjectMapper.Triggers.Count);
  CheckEquals(TTriggerProdutos, FObjectMapper.Triggers.Items[0].TriggerClass);
  Check(FObjectMapper.Triggers.Items[0].TriggerOptions = TConsts.cTriggerAll);
end;

initialization
  RegisterTest(TestTObjectMapper.Suite);

end.

