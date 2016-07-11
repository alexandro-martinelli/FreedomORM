unit uTestObjectMapperToDDLEntity;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  AM.Freedom.ObjectMapper,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.ObjectMapperToDDLEntity,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.DDLObjects,
  AM.Freedom.DefaultsClassRegister;

type
  TestTObjectMapperToDDLEntity = class(TTestCase)
  strict private
    FObjectMapperToDDLEntity: TObjectMapperToDDLEntity;
    function DoExtract(pClass: TClass): TDDLEntity;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestExtractDDL;
    procedure TestFieldNames;
    procedure TestExtractProdutosMateriasPrimas;
  end;

implementation

uses
  System.SysUtils,
  uObjectsTests,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.IDBPersistent;

function TestTObjectMapperToDDLEntity.DoExtract(pClass: TClass): TDDLEntity;
var
  lObjectMapper: TObjectMapper;
  lParams: TObjectToMapperParams;
  lDBPersistent: IDBPersistent;
begin
  lParams := TObjectToMapperParams.Create;
  lParams.MetaClassType := pClass;
  lObjectMapper := TObjectToMapper.ObjectToMapper(lParams);
  try
    Supports(TDefaultsClassRegister.DefaultPersistent, IDBPersistent, lDBPersistent);
    Result := FObjectMapperToDDLEntity.ExtractDDL(lObjectMapper, lDBPersistent);
  finally
    TObjectToMapper.UnlockMapper(lObjectMapper.GetHashCode);
  end;
end;

procedure TestTObjectMapperToDDLEntity.SetUp;
begin
  FObjectMapperToDDLEntity := TObjectMapperToDDLEntity.Create;
end;

procedure TestTObjectMapperToDDLEntity.TearDown;
begin
  FObjectMapperToDDLEntity.Free;
  FObjectMapperToDDLEntity := nil;
end;

procedure TestTObjectMapperToDDLEntity.TestExtractDDL;
var
  lReturnValue: TDDLEntity;
begin
  lReturnValue := DoExtract(TProduto);
  CheckTrue(Assigned(lReturnValue));
  lReturnValue.Free;
end;

procedure TestTObjectMapperToDDLEntity.TestExtractProdutosMateriasPrimas;
var
  lReturnValue: TDDLEntity;
  lColumn: TDDLColumn;
begin
  lReturnValue := DoExtract(TMateriaPrima);
  try
    CheckEquals({$IFNDEF FIREBIRD} 8 {$ELSE} 4 {$IFEND}, lReturnValue.Columns.Count);
    lColumn := lReturnValue.Columns.FindColumn('ID', ctyInteger);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyInteger);
    CheckTrue(Required in lColumn.ColumnOptions);

    lColumn := lReturnValue.Columns.FindColumn('CODMATERIA_PRIMA', ctyInteger);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyInteger);
    CheckTrue(Required in lColumn.ColumnOptions);

    lColumn := lReturnValue.Columns.FindColumn('CODPRODUTO', ctyInteger);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyInteger);
    CheckTrue(Required in lColumn.ColumnOptions);

    lColumn := lReturnValue.Columns.FindColumn('QTDE', ctyDouble);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyDouble);
  finally
    lReturnValue.Free;
  end;

end;

procedure TestTObjectMapperToDDLEntity.TestFieldNames;
var
  lReturnValue: TDDLEntity;
  lColumn: TDDLColumn;
  lColumnType: TColumnType;
begin
  lReturnValue := DoExtract(TProduto);
  try
    lColumnType := {$IFDEF MSSQL}ctyByte{$ELSE}ctySmallInt{$IFEND};

    CheckEquals({$IFNDEF FIREBIRD} 32 {$ELSE} 27 {$IFEND}, lReturnValue.Columns.Count);
    lColumn := lReturnValue.Columns.FindColumn('CODPRODUTO', ctyInteger);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyInteger);
    CheckTrue(Required in lColumn.ColumnOptions);

    lColumn := lReturnValue.Columns.FindColumn('DESCRICAO', ctyString);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyString);
    CheckEquals(150, lColumn.Size);

    lColumn := lReturnValue.Columns.FindColumn('DATA_CADASTRO', ctyDate);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyDate);
    CheckTrue(lColumn.DefaultValueOptions.IsNow);

    lColumn := lReturnValue.Columns.FindColumn('ATIVO', lColumnType);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = lColumnType);
    CheckEquals(1, lColumn.DefaultValueOptions.Value);
    CheckTrue(Required in lColumn.ColumnOptions);

    lColumn := lReturnValue.Columns.FindColumn('PRECO_VENDA', ctyDouble);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyDouble);

    lColumn := lReturnValue.Columns.FindColumn('OBS', ctyMemo);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyMemo);

    lColumn := lReturnValue.Columns.FindColumn('HORACADASTRO', ctyTime);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyTime);
    CheckTrue(lColumn.DefaultValueOptions.IsNow);

    lColumn := lReturnValue.Columns.FindColumn('DATAHORACADASTRO', ctyDateTime);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyDateTime);
    CheckTrue(lColumn.DefaultValueOptions.IsNow);

    lColumn := lReturnValue.Columns.FindColumn('CODUNIDADE', ctyInteger);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyInteger);

    lColumn := lReturnValue.Columns.FindColumn('CODTIPO_PRODUTO', lColumnType);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = lColumnType);

    lColumn := lReturnValue.Columns.FindColumn('PRECO_VENDA_NULLABLE', ctyDouble);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyDouble);

    lColumn := lReturnValue.Columns.FindColumn('DATA_CADASTRO_NULLABLE', ctyDate);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyDate);
    CheckTrue(lColumn.DefaultValueOptions.IsNow);

    lColumn := lReturnValue.Columns.FindColumn('HORACADASTRO_NULLABLE', ctyTime);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyTime);
    CheckTrue(lColumn.DefaultValueOptions.IsNow);

    lColumn := lReturnValue.Columns.FindColumn('DATAHORACADASTRO_NULLABLE', ctyDateTime);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyDateTime);
    CheckTrue(lColumn.DefaultValueOptions.IsNow);

    lColumn := lReturnValue.Columns.FindColumn('ATIVO_NULLABLE', lColumnType);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = lColumnType);
    CheckEquals(1, lColumn.DefaultValueOptions.Value);

    lColumn := lReturnValue.Columns.FindColumn('ATIVO_STR_NULLABLE', ctyString);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyString);
    CheckEquals('S', lColumn.DefaultValueOptions.Value);

    lColumn := lReturnValue.Columns.FindColumn('CODUNIDADE_NULLABLE', ctyInteger);
    CheckTrue(Assigned(lColumn));
    CheckTrue(lColumn.ColumnType = ctyInteger);

  finally
    lReturnValue.Free;
  end;
end;

initialization
  RegisterTest(TestTObjectMapperToDDLEntity.Suite);


end.

