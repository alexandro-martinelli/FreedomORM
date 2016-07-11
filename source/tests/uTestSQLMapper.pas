unit uTestSQLMapper;

{$I FreedomORM.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  TestFramework,
  System.Generics.Collections,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.DefaultsClassRegister,
  AM.Freedom.EnumerationTypes,
  uObjectsTests,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.SQLCommands.TableRowCommands,
  AM.Freedom.SQLMapper.CustomArgument;

type
  TestSQLMapper = class(TTestCase)
  strict private
    FSQLMapper: ISQLMapper;
    function DoCreateCommand(pObject: TObject; pSchema: string; pObjectState: TObjectState): TCustomCommand;
    function DoCreateInsertCommand(pObject: TObject; pSchema: string): TInsertCommand;
    function DoCreateUpdateCommand(pObject: TObject; pSchema: string): TUpdateCommand;
    function DoCreateDeleteCommand(pObject: TObject; pSchema: string): TDeleteCommand;
    function CreateProduto(pNewObject: Boolean; pIdObject: Integer = 0): TProduto;
    function CreateProdutoForUpdate: TProduto;
  public
    procedure SetUp; override;
    procedure TearDown; override;
    function CheckEqualsDates(pDateOne, pDateTwo: TDate): Boolean;
    function CheckEqualsTimes(pTimeOne, pTimeTwo: TTime): Boolean;
    function CheckEqualsDateTimes(pDateTimeOne, pDateTimeTwo: TDateTime): Boolean;
  published
    procedure TestGenerateInsertCommandWithNewObject;
    procedure TestGenerateInsertCommandWithExistingObject;
    {$IFNDEF FIREBIRD}
    procedure TestGenerateInsertCommandWithNewObjectInAuditoriaSchema;
    procedure TestGenerateInsertCommandWithExistingObjectInAuditoriaSchema;
    {$IFEND}
    procedure TestGenerateUpdateCommand;
    {$IFNDEF FIREBIRD}
    procedure TestGenerateUpdateCommandInAuditoriaSchema;
    {$IFEND}
    procedure TestGenerateDeleteCommand;
    {$IFNDEF FIREBIRD}
    procedure TestGenerateDeleteCommandInAuditoriaSchema;
    {$IFEND}
  end;

implementation

uses
  AM.Freedom.ObjectMapper,
  AM.Freedom.ObjectMapper.ObjectMapperToSchemaMapper,
  AM.Freedom.ObjectMapper.Schemas,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.Helper.Variant,
  AM.Freedom.Exceptions;

function TestSQLMapper.CheckEqualsDates(pDateOne, pDateTwo: TDate): Boolean;
begin
  Result := Trunc(pDateOne) = Trunc(pDateTwo);
end;

function TestSQLMapper.CheckEqualsDateTimes(pDateTimeOne, pDateTimeTwo: TDateTime): Boolean;
begin
  Result := pDateTimeOne = pDateTimeTwo;
end;

function TestSQLMapper.CheckEqualsTimes(pTimeOne, pTimeTwo: TTime): Boolean;
begin
  Result := pTimeOne = pTimeTwo;
end;

function TestSQLMapper.CreateProduto(pNewObject: Boolean; pIdObject: Integer): TProduto;
begin
  if (pNewObject) then
  begin
    Result := TProduto.Create;
    Result.Descricao := 'Arroz';
    Result.PrecoVenda := 263.23;
    Result.DataHoraCadastro := Now;
    Result.DataCadastro := Result.DataHoraCadastro;
    Result.HoraCadastro := Result.DataHoraCadastro;
    Result.Ativo := False;
    Result.Tipo := tpServico;
    Result.Observacao.Add('Adicionado por FreedomORM em ' + FormatDateTime('dd/MM/yyyy HH:nn:ss', Result.DataHoraCadastro));
    Result.IdUnidade := 5;
    Result.AtivoStr := False;
    Result.AtivoNullable.Value := False;
    Result.AtivoStrNullable.Value := False;
    Result.DataCadastroNullable.Value := 0;
  end
  else
  begin
    Result := TProduto.Create(pIdObject);
  end;
end;

function TestSQLMapper.CreateProdutoForUpdate: TProduto;
begin
  Result := CreateProduto(False, 8);
  Result.Descricao := 'Arroz';
  Result.PrecoVenda := 263.23;
  Result.DataHoraCadastro := Now;
  Result.DataCadastro := Result.DataHoraCadastro;
  Result.HoraCadastro := Result.DataHoraCadastro;
  Result.Ativo := False;
  Result.Tipo := tpServico;
  Result.Observacao.Clear;
  Result.Observacao.Add('Alterado por FreedomORM em ' + FormatDateTime('dd/MM/yyyy HH:nn:ss', Result.DataHoraCadastro));
  Result.IdUnidade := 5;
  Result.AtivoStr := False;
  Result.AtivoNullable.Value := False;
  Result.AtivoStrNullable.Value := False;
  Result.DataCadastroNullable.Value := 0;
end;

function TestSQLMapper.DoCreateCommand(pObject: TObject; pSchema: string; pObjectState: TObjectState): TCustomCommand;
var
  lParams: TObjectToMapperParams;
  lObjectMapper: TObjectMapper;
  lSchemaItem: TSchemaItem;
  lSchemaMapper: TObjectMapper;
begin
  lParams := TObjectToMapperParams.Create;
  lParams.ObjectInstance := pObject;
  lObjectMapper := TObjectToMapper.ObjectToMapper(lParams);
  lSchemaItem := TSchemaItem.Create(pSchema, True);
  lObjectMapper.CurrentSchema := pSchema;
  lSchemaMapper := TObjectMapperToSchemaMapper.ExtractSchemaMapper(lObjectMapper, lSchemaItem);
  try
    Result := FSQLMapper.GenerateCommand(lSchemaMapper, pObjectState);
  finally
    TObjectToMapper.UnlockMapper(lObjectMapper.GetHashCode);
    lSchemaMapper.Free;
    lSchemaItem.Free;
    pObject.Free;
  end;
end;

function TestSQLMapper.DoCreateDeleteCommand(pObject: TObject; pSchema: string): TDeleteCommand;
begin
  Result := TDeleteCommand(DoCreateCommand(pObject, pSchema, Deleted));
end;

function TestSQLMapper.DoCreateInsertCommand(pObject: TObject; pSchema: string): TInsertCommand;
begin
  Result := TInsertCommand(DoCreateCommand(pObject, pSchema, Inserted));
end;

function TestSQLMapper.DoCreateUpdateCommand(pObject: TObject; pSchema: string): TUpdateCommand;
begin
  Result := TUpdateCommand(DoCreateCommand(pObject, pSchema, Clean));
end;

procedure TestSQLMapper.SetUp;
begin
  FSQLMapper := TDefaultsClassRegister.DefaultSQLMapper;
end;

procedure TestSQLMapper.TearDown;
begin
  FSQLMapper := nil;
end;

procedure TestSQLMapper.TestGenerateDeleteCommand;
var
  lProduto: TProduto;
  lReturnValue: TDeleteCommand;
begin
  lProduto := CreateProduto(False, 8);
  lReturnValue := nil;
  try
    lReturnValue := DoCreateDeleteCommand(lProduto, TObjectToMapper.ExtractDefaultSchema(lProduto.ClassInfo));
    CheckEqualsString('PRODUTOS', lReturnValue.FromTable.Name);
    CheckEquals(1, lReturnValue.WhereClause.ListCriterias.Count);
  finally
    FreeAndNil(lReturnValue);
  end;
end;

{$IFNDEF FIREBIRD}
procedure TestSQLMapper.TestGenerateDeleteCommandInAuditoriaSchema;
var
  lProduto: TProduto;
  lReturnValue: TDeleteCommand;
begin
  lProduto := CreateProduto(False, 8);
  lReturnValue := nil;
  ExpectedException := ECannotPersistWithoutIDColumnValue;
  try
    lReturnValue := DoCreateDeleteCommand(lProduto, 'auditoria');
  finally
    FreeAndNil(lReturnValue);
  end;
end;
{$ENDIF}

procedure TestSQLMapper.TestGenerateInsertCommandWithExistingObject;
var
  lReturnValue: TInsertCommand;
  lProduto: TProduto;
begin
  lProduto := CreateProduto(False, 8);
  lReturnValue := nil;
  try
    lReturnValue := DoCreateInsertCommand(lProduto, TObjectToMapper.ExtractDefaultSchema(lProduto.ClassInfo));
    CheckEqualsString('PRODUTOS', lReturnValue.Into.Name);
    CheckEquals(1, lReturnValue.ReturningFields.Count);
    CheckEqualsString('CODPRODUTO', lReturnValue.ReturningFields.Items[0]);
    CheckEquals(lReturnValue.Values.Count, lReturnValue.Fields.Count);
    CheckEquals(16, lReturnValue.Values.Count);
    {$IFNDEF FIREBIRD}
    CheckFalse(Assigned(lReturnValue.FindField('CODPRODUTO')));
    {$ELSE}
    CheckTrue(Assigned(lReturnValue.FindField('CODPRODUTO')));
    CheckEqualsString('gen_id(SEQ_PRODUTOS, 1)', lReturnValue.ValueByFieldName('CODPRODUTO'));
    {$ENDIF}
    CheckFalse(Assigned(lReturnValue.FindField('ID_AUDITORIA')));
    CheckFalse(Assigned(lReturnValue.FindField('USUARIO_ID')));
    CheckFalse(Assigned(lReturnValue.FindField('DATA_AUDITORIA')));
    CheckFalse(Assigned(lReturnValue.FindField('OPERACAO')));
    CheckEqualsString('Manjericão', lReturnValue.ValueByFieldName('DESCRICAO'));
    CheckTrue(CheckEqualsDates(StrToDate('07/02/2014'), lReturnValue.ValueByFieldName('DATA_CADASTRO')));
    CheckEquals(6, lReturnValue.ValueByFieldName('PRECO_VENDA'), 0.001);
    CheckEquals(0, lReturnValue.ValueByFieldName('ATIVO'));
    CheckEquals(0, lReturnValue.ValueByFieldName('CODTIPO_PRODUTO'));
    CheckEquals(3, lReturnValue.ValueByFieldName('CODUNIDADE'));
    CheckEqualsString('S', lReturnValue.ValueByFieldName('ATIVO_STR'));
    CheckEqualsString('S', lReturnValue.ValueByFieldName('ATIVO_STR_NULLABLE'));
    CheckEquals(1, lReturnValue.ValueByFieldName('ATIVO_NULLABLE'));
    CheckTrue(lReturnValue.ValueByFieldName('PRECO_VENDA_NULLABLE').IsNull);
    CheckTrue(lReturnValue.ValueByFieldName('DATA_CADASTRO_NULLABLE').IsNull);
    CheckTrue(lReturnValue.ValueByFieldName('HORACADASTRO_NULLABLE').IsNull);
    CheckFalse(Assigned(lReturnValue.FindField('OBS')));
    CheckFalse(Assigned(lReturnValue.FindField('HORACADASTRO')));
    CheckFalse(Assigned(lReturnValue.FindField('DATAHORACADASTRO')));
    CheckFalse(Assigned(lReturnValue.FindField('ID_BAIRRO_COB')));
  finally
    lReturnValue.Free;
  end;
end;

procedure TestSQLMapper.TestGenerateInsertCommandWithNewObject;
var
  lReturnValue: TInsertCommand;
  lProduto: TProduto;
  lStream: TStream;
  lStrings: TStrings;
  lDateTime: TDateTime;
  lSchema: string;
begin
  lProduto := CreateProduto(True);
  lDateTime := lProduto.DataHoraCadastro;
  lReturnValue := nil;
  try
    lSchema := TObjectToMapper.ExtractDefaultSchema(lProduto.ClassInfo);
    lReturnValue := DoCreateInsertCommand(lProduto, lSchema);
    CheckEqualsString('PRODUTOS', lReturnValue.Into.Name);
    CheckEquals(1, lReturnValue.ReturningFields.Count);
    CheckEqualsString('CODPRODUTO', lReturnValue.ReturningFields.Items[0]);
    CheckEquals(lReturnValue.Values.Count, lReturnValue.Fields.Count);
    CheckEquals(19, lReturnValue.Values.Count);

    {$IFNDEF FIREBIRD}
    CheckFalse(Assigned(lReturnValue.FindField('CODPRODUTO')));
    {$ELSE}
    CheckTrue(Assigned(lReturnValue.FindField('CODPRODUTO')));
    CheckEqualsString('gen_id(SEQ_PRODUTOS, 1)', lReturnValue.ValueByFieldName('CODPRODUTO'));
    {$ENDIF}
    CheckFalse(Assigned(lReturnValue.FindField('ID_AUDITORIA')));
    CheckFalse(Assigned(lReturnValue.FindField('USUARIO_ID')));
    CheckFalse(Assigned(lReturnValue.FindField('DATA_AUDITORIA')));
    CheckFalse(Assigned(lReturnValue.FindField('OPERACAO')));
    CheckTrue(lReturnValue.ValueByFieldName('PRECO_VENDA_NULLABLE').IsNull);
    CheckEqualsString('Arroz', lReturnValue.ValueByFieldName('DESCRICAO'));
    CheckEquals(263.23, lReturnValue.ValueByFieldName('PRECO_VENDA'), 0.001);
    CheckTrue(CheckEqualsDates(lDateTime, lReturnValue.ValueByFieldName('DATA_CADASTRO')));
    CheckTrue(CheckEqualsTimes(lDateTime, lReturnValue.ValueByFieldName('HORACADASTRO')));
    CheckTrue(CheckEqualsDateTimes(lDateTime, lReturnValue.ValueByFieldName('DATAHORACADASTRO')));
    CheckEquals(0, lReturnValue.ValueByFieldName('ATIVO'));
    CheckEquals(1, lReturnValue.ValueByFieldName('CODTIPO_PRODUTO'));
    CheckEquals(5, lReturnValue.ValueByFieldName('CODUNIDADE'));
    CheckEqualsString('N', lReturnValue.ValueByFieldName('ATIVO_STR'));
    CheckEqualsString('N', lReturnValue.ValueByFieldName('ATIVO_STR_NULLABLE'));
    CheckEquals(0, lReturnValue.ValueByFieldName('ATIVO_NULLABLE'));
    CheckTrue(lReturnValue.ValueByFieldName('PRECO_VENDA_NULLABLE').IsNull);
    CheckTrue(CheckEqualsDates(0, lReturnValue.ValueByFieldName('DATA_CADASTRO_NULLABLE')));
    CheckTrue(lReturnValue.ValueByFieldName('HORACADASTRO_NULLABLE').IsNull);
    lStream := lReturnValue.StreamByFieldName('OBS');
    CheckTrue(Assigned(lStream));
    lStrings := TStringList.Create;
    try
      lStream.Position := 0;
      lStrings.LoadFromStream(lStream);
      CheckEqualsString('Adicionado por FreedomORM em ' + FormatDateTime('dd/MM/yyyy HH:nn:ss', lDateTime), Trim(lStrings.Text));
    finally
      lStrings.Free;
    end;
  finally
    lReturnValue.Free;
  end;
end;

{$IFNDEF FIREBIRD}
procedure TestSQLMapper.TestGenerateInsertCommandWithExistingObjectInAuditoriaSchema;
var
  lReturnValue: TInsertCommand;
  lProduto: TProduto;
  lDateTime: TDateTime;
begin
  lProduto := CreateProduto(False, 8);
  lDateTime := Now;
  lProduto.CodUsuario := 1;
  lProduto.DataAuditoria := lDateTime;
  lProduto.Status := Inserted;
  lReturnValue := nil;
  try
    lReturnValue := DoCreateInsertCommand(lProduto, 'auditoria');
    CheckEqualsString('PRODUTOS', lReturnValue.Into.Name);
    CheckEquals(1, lReturnValue.ReturningFields.Count);
    CheckEqualsString('ID_AUDITORIA', lReturnValue.ReturningFields.Items[0]);
    CheckEquals(lReturnValue.Values.Count, lReturnValue.Fields.Count);
    CheckEquals(20, lReturnValue.Values.Count);
    CheckTrue(Assigned(lReturnValue.FindField('CODPRODUTO')));
    CheckEquals(8, lReturnValue.ValueByFieldName('CODPRODUTO'));
    CheckFalse(Assigned(lReturnValue.FindField('ID_AUDITORIA')));
    CheckTrue(Assigned(lReturnValue.FindField('USUARIO_ID')));
    CheckEquals(1, lReturnValue.ValueByFieldName('USUARIO_ID'));
    CheckTrue(Assigned(lReturnValue.FindField('DATA_AUDITORIA')));
    CheckTrue(CheckEqualsDateTimes(lDateTime, lReturnValue.ValueByFieldName('DATA_AUDITORIA')));
    CheckTrue(Assigned(lReturnValue.FindField('OPERACAO')));
    CheckEquals(Ord(Inserted), lReturnValue.ValueByFieldName('OPERACAO'));
    CheckEqualsString('Manjericão', lReturnValue.ValueByFieldName('DESCRICAO'));
    CheckTrue(CheckEqualsDates(StrToDate('07/02/2014'), lReturnValue.ValueByFieldName('DATA_CADASTRO')));
    CheckEquals(6, lReturnValue.ValueByFieldName('PRECO_VENDA'), 0.001);
    CheckEquals(0, lReturnValue.ValueByFieldName('ATIVO'));
    CheckEquals(0, lReturnValue.ValueByFieldName('CODTIPO_PRODUTO'));
    CheckEquals(3, lReturnValue.ValueByFieldName('CODUNIDADE'));
    CheckEqualsString('S', lReturnValue.ValueByFieldName('ATIVO_STR'));
    CheckEqualsString('S', lReturnValue.ValueByFieldName('ATIVO_STR_NULLABLE'));
    CheckEquals(1, lReturnValue.ValueByFieldName('ATIVO_NULLABLE'));
    CheckTrue(lReturnValue.ValueByFieldName('PRECO_VENDA_NULLABLE').IsNull);
    CheckTrue(lReturnValue.ValueByFieldName('DATA_CADASTRO_NULLABLE').IsNull);
    CheckTrue(lReturnValue.ValueByFieldName('HORACADASTRO_NULLABLE').IsNull);
    CheckFalse(Assigned(lReturnValue.FindField('OBS')));
    CheckFalse(Assigned(lReturnValue.FindField('HORACADASTRO')));
    CheckFalse(Assigned(lReturnValue.FindField('DATAHORACADASTRO')));
    CheckFalse(Assigned(lReturnValue.FindField('ID_BAIRRO_COB')));
  finally
    lReturnValue.Free;
  end;
end;

procedure TestSQLMapper.TestGenerateInsertCommandWithNewObjectInAuditoriaSchema;
var
  lReturnValue: TInsertCommand;
  lProduto: TProduto;
  lStream: TStream;
  lStrings: TStrings;
  lDateTime: TDateTime;
  lDataHoraCadastro: TDateTime;
begin
  lProduto := CreateProduto(True);
  lDataHoraCadastro := lProduto.DataHoraCadastro;
  lDateTime := Now;
  lProduto.Codigo := 999;
  lProduto.CodUsuario := 1;
  lProduto.DataAuditoria := lDateTime;
  lProduto.Status := Inserted;
  lReturnValue := nil;
  try
    lReturnValue := DoCreateInsertCommand(lProduto, 'auditoria');
    CheckEqualsString('PRODUTOS', lReturnValue.Into.Name);
    CheckEquals(1, lReturnValue.ReturningFields.Count);
    CheckEqualsString('ID_AUDITORIA', lReturnValue.ReturningFields.Items[0]);
    CheckEquals(lReturnValue.Values.Count, lReturnValue.Fields.Count);
    CheckEquals(23, lReturnValue.Values.Count);
    CheckTrue(Assigned(lReturnValue.FindField('CODPRODUTO')));
    CheckEquals(999, lReturnValue.ValueByFieldName('CODPRODUTO'));
    CheckFalse(Assigned(lReturnValue.FindField('ID_AUDITORIA')));
    CheckTrue(Assigned(lReturnValue.FindField('USUARIO_ID')));
    CheckEquals(1, lReturnValue.ValueByFieldName('USUARIO_ID'));
    CheckTrue(Assigned(lReturnValue.FindField('DATA_AUDITORIA')));
    CheckTrue(CheckEqualsDateTimes(lReturnValue.ValueByFieldName('DATA_AUDITORIA'), lDateTime));
    CheckTrue(Assigned(lReturnValue.FindField('OPERACAO')));
    CheckEquals(Ord(Inserted), lReturnValue.ValueByFieldName('OPERACAO'));
    CheckTrue(lReturnValue.ValueByFieldName('PRECO_VENDA_NULLABLE').IsNull);
    CheckEqualsString('Arroz', lReturnValue.ValueByFieldName('DESCRICAO'));
    CheckEquals(263.23, lReturnValue.ValueByFieldName('PRECO_VENDA'), 0.001);
    CheckTrue(CheckEqualsDates(lDataHoraCadastro, lReturnValue.ValueByFieldName('DATA_CADASTRO')));
    CheckTrue(CheckEqualsTimes(lDataHoraCadastro, lReturnValue.ValueByFieldName('HORACADASTRO')));
    CheckTrue(CheckEqualsDateTimes(lDataHoraCadastro, lReturnValue.ValueByFieldName('DATAHORACADASTRO')));
    CheckEquals(0, lReturnValue.ValueByFieldName('ATIVO'));
    CheckEquals(1, lReturnValue.ValueByFieldName('CODTIPO_PRODUTO'));
    CheckEquals(5, lReturnValue.ValueByFieldName('CODUNIDADE'));
    CheckEqualsString('N', lReturnValue.ValueByFieldName('ATIVO_STR'));
    CheckEqualsString('N', lReturnValue.ValueByFieldName('ATIVO_STR_NULLABLE'));
    CheckEquals(0, lReturnValue.ValueByFieldName('ATIVO_NULLABLE'));
    CheckTrue(lReturnValue.ValueByFieldName('PRECO_VENDA_NULLABLE').IsNull);
    CheckTrue(CheckEqualsDates(0, lReturnValue.ValueByFieldName('DATA_CADASTRO_NULLABLE')));
    CheckTrue(lReturnValue.ValueByFieldName('HORACADASTRO_NULLABLE').IsNull);
    lStream := lReturnValue.StreamByFieldName('OBS');
    CheckTrue(Assigned(lStream));
    lStrings := TStringList.Create;
    try
      lStream.Position := 0;
      lStrings.LoadFromStream(lStream);
      CheckEqualsString('Adicionado por FreedomORM em ' + FormatDateTime('dd/MM/yyyy HH:nn:ss', lDataHoraCadastro), Trim(lStrings.Text));
    finally
      lStrings.Free;
    end;
  finally
    lReturnValue.Free;
  end;
end;
{$ENDIF}

procedure TestSQLMapper.TestGenerateUpdateCommand;
var
  lReturnValue: TUpdateCommand;
  lProduto: TProduto;
  lStream: TStream;
  lStrings: TStrings;
  lDateTime: TDateTime;
begin
  lProduto := CreateProdutoForUpdate;
  lDateTime := lProduto.DataHoraCadastro;
  lReturnValue := nil;
  try
    lReturnValue := DoCreateUpdateCommand(lProduto, TObjectToMapper.ExtractDefaultSchema(lProduto.ClassInfo));
    CheckEqualsString('PRODUTOS', lReturnValue.Table.Name);
    CheckEquals(12, lReturnValue.FieldsValues.Count);
    CheckFalse(Assigned(lReturnValue.FindField('CODPRODUTO')));
    CheckFalse(Assigned(lReturnValue.FindField('ID_AUDITORIA')));
    CheckFalse(Assigned(lReturnValue.FindField('USUARIO_ID')));
    CheckFalse(Assigned(lReturnValue.FindField('DATA_AUDITORIA')));
    CheckFalse(Assigned(lReturnValue.FindField('OPERACAO')));
    CheckFalse(Assigned(lReturnValue.FindField('PRECO_VENDA_NULLABLE')));
    CheckEqualsString('Arroz', lReturnValue.FindField('DESCRICAO').AsVariant);
    CheckEquals(263.23, lReturnValue.FindField('PRECO_VENDA').AsVariant, 0.001);
    CheckTrue(CheckEqualsDates(lDateTime, lReturnValue.FindField('DATA_CADASTRO').AsVariant));
    CheckTrue(CheckEqualsTimes(lDateTime, lReturnValue.FindField('HORACADASTRO').AsVariant));
    CheckTrue(CheckEqualsDateTimes(lDateTime, lReturnValue.FindField('DATAHORACADASTRO').AsVariant));
    CheckFalse(Assigned(lReturnValue.FindField('ATIVO')));
    CheckEquals(1, lReturnValue.FindField('CODTIPO_PRODUTO').AsVariant);
    CheckEquals(5, lReturnValue.FindField('CODUNIDADE').AsVariant);
    CheckEqualsString('N', lReturnValue.FindField('ATIVO_STR').AsVariant);
    CheckEqualsString('N', lReturnValue.FindField('ATIVO_STR_NULLABLE').AsVariant);
    CheckEquals(0, lReturnValue.FindField('ATIVO_NULLABLE').AsVariant);
    CheckFalse(Assigned(lReturnValue.FindField('PRECO_VENDA_NULLABLE')));
    CheckTrue(CheckEqualsDates(0, lReturnValue.FindField('DATA_CADASTRO_NULLABLE').AsVariant));
    CheckFalse(Assigned(lReturnValue.FindField('HORACADASTRO_NULLABLE')));
    lStream := lReturnValue.FindField('OBS').AsStream;
    CheckTrue(Assigned(lStream));
    lStrings := TStringList.Create;
    try
      lStream.Position := 0;
      lStrings.LoadFromStream(lStream);
      CheckEqualsString('Alterado por FreedomORM em ' + FormatDateTime('dd/MM/yyyy HH:nn:ss', lDateTime), Trim(lStrings.Text));
    finally
      lStrings.Free;
    end;
  finally
    lReturnValue.Free;
  end;
end;

{$IFNDEF FIREBIRD}
procedure TestSQLMapper.TestGenerateUpdateCommandInAuditoriaSchema;
var
  lReturnValue: TUpdateCommand;
  lProduto: TProduto;
  lDateTime: TDateTime;
begin
  lProduto := CreateProdutoForUpdate;
  lDateTime := lProduto.DataHoraCadastro;
  lProduto.CodUsuario := 1;
  lProduto.DataAuditoria := lDateTime;
  lProduto.Status := Clean;
  lReturnValue := nil;
  ExpectedException := ECannotPersistWithoutIDColumnValue;
  try
    lReturnValue := DoCreateUpdateCommand(lProduto, 'auditoria');
  finally
    FreeAndNil(lReturnValue);
  end;
end;
{$ENDIF}

initialization
  RegisterTest(TestSQLMapper.Suite);

end.
