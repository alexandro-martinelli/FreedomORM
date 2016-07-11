unit uTestIndexCommandsTextGenerator;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  System.SysUtils,
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.TextGenerator.IndexCommandsTextGenerator,
  AM.Freedom.SQLCommands.IndexCommands,
  AM.Freedom.Helper.CommandType;

type
  TestTCreateIndexCommandTextGenerator = class(TTestCase)
  strict private
    FCreateIndexCommandTextGenerator: TCreateIndexCommandTextGenerator;
    function GetIndexOptionForTest(pCorrectly: Boolean = False): TIndexOption;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateText;
    procedure TestGenerateComplexText;
  end;

  TestTDropIndexCommandTextGenerator = class(TTestCase)
  strict private
    FDropIndexCommandTextGenerator: TDropIndexCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateText;
    procedure TestGenerateTextWithSchema;
  end;

implementation

function TestTCreateIndexCommandTextGenerator.GetIndexOptionForTest(pCorrectly: Boolean): TIndexOption;
begin
  if (not pCorrectly) then
  begin
    {$IFDEF POSTGRE}
    Result := ioClustered;
    {$ENDIF}
    {$IFDEF MSSQL}
    Result := ioDescending;
    {$ENDIF}
    {$IFDEF FIREBIRD}
    Result := ioConcurrently;
    {$ENDIF}
  end
  else
  begin
    {$IFDEF POSTGRE}
    Result := ioConcurrently;
    {$ENDIF}
    {$IFDEF MSSQL}
    Result := ioClustered;
    {$ENDIF}
    {$IFDEF FIREBIRD}
    Result := ioDescending;
    {$ENDIF}
  end;
end;

procedure TestTCreateIndexCommandTextGenerator.SetUp;
begin
  FCreateIndexCommandTextGenerator := TCreateIndexCommandTextGenerator.Create;
end;

procedure TestTCreateIndexCommandTextGenerator.TearDown;
begin
  FCreateIndexCommandTextGenerator.Free;
  FCreateIndexCommandTextGenerator := nil;
end;

procedure TestTCreateIndexCommandTextGenerator.TestGenerateComplexText;
var
  lCreateIndex: TCreateIndexCommand;
  lReturnValue: string;
begin
  lCreateIndex := TCreateIndexCommand.Create('auditoria');
  try
    lCreateIndex.Name := 'IDX_PRODUTO_DATA_CADASTRO';
    lCreateIndex.OnTable := 'PRODUTO';
    lCreateIndex.Option := GetIndexOptionForTest(True);
    lCreateIndex.Columns.AddColumn('DATA_CADASTRO', Desc, noFirst);
    lReturnValue := FCreateIndexCommandTextGenerator.GenerateText(lCreateIndex);
    {$IFDEF POSTGRE}
    CheckEqualsString('create index concurrently IDX_PRODUTO_DATA_CADASTRO on AUDITORIA.PRODUTO (DATA_CADASTRO desc nulls first)', lReturnValue);
    {$ENDIF}
    {$IFDEF MSSQL}
    CheckEqualsString('create clustered index IDX_PRODUTO_DATA_CADASTRO on AUDITORIA.PRODUTO (DATA_CADASTRO desc)', lReturnValue);
    {$ENDIF}
    {$IFDEF FIREBIRD}
    CheckEqualsString('create descending index IDX_PRODUTO_DATA_CADASTRO on PRODUTO (DATA_CADASTRO)', lReturnValue);
    {$ENDIF}
  finally
    lCreateIndex.Free;
  end;
end;

procedure TestTCreateIndexCommandTextGenerator.TestGenerateText;
var
  lCreateIndex: TCreateIndexCommand;
  lReturnValue: string;
begin
  lCreateIndex := TCreateIndexCommand.Create('auditoria');
  try
    lCreateIndex.Name := 'IDX_PRODUTO_DATA_CADASTRO';
    lCreateIndex.OnTable := 'PRODUTO';
    lCreateIndex.Option := GetIndexOptionForTest;
    lCreateIndex.Columns.AddColumn('DATA_CADASTRO', Desc, noFirst);
    lReturnValue := FCreateIndexCommandTextGenerator.GenerateText(lCreateIndex);
    {$IFDEF POSTGRE}
    CheckEqualsString('create index IDX_PRODUTO_DATA_CADASTRO on AUDITORIA.PRODUTO (DATA_CADASTRO desc nulls first)', lReturnValue);
    {$ENDIF}
    {$IFDEF MSSQL}
    CheckEqualsString('create index IDX_PRODUTO_DATA_CADASTRO on AUDITORIA.PRODUTO (DATA_CADASTRO desc)', lReturnValue);
    {$ENDIF}
    {$IFDEF FIREBIRD}
    CheckEqualsString('create index IDX_PRODUTO_DATA_CADASTRO on PRODUTO (DATA_CADASTRO)', lReturnValue);
    {$ENDIF}
  finally
    lCreateIndex.Free;
  end;
end;

procedure TestTDropIndexCommandTextGenerator.SetUp;
begin
  FDropIndexCommandTextGenerator := TDropIndexCommandTextGenerator.Create;
end;

procedure TestTDropIndexCommandTextGenerator.TearDown;
begin
  FDropIndexCommandTextGenerator.Free;
  FDropIndexCommandTextGenerator := nil;
end;

procedure TestTDropIndexCommandTextGenerator.TestGenerateText;
var
  lDropIndex: TDropIndexCommand;
  lReturnValue: string;
begin
  lDropIndex := TDropIndexCommand.Create('IDX_PRODUTO_DATA_CADASTRO', 'PRODUTO');
  try
    lReturnValue := FDropIndexCommandTextGenerator.GenerateText(lDropIndex);
    {$IFDEF MSSQL}
    CheckEqualsString('drop index IDX_PRODUTO_DATA_CADASTRO on PRODUTO', lReturnValue);
    {$ELSE}
    CheckEqualsString('drop index IDX_PRODUTO_DATA_CADASTRO', lReturnValue);
    {$ENDIF}
  finally
    lDropIndex.Free;
  end;
end;

procedure TestTDropIndexCommandTextGenerator.TestGenerateTextWithSchema;
var
  lDropIndex: TDropIndexCommand;
  lReturnValue: string;
begin
  lDropIndex := TDropIndexCommand.Create('IDX_PRODUTO_DATA_CADASTRO', 'PRODUTO', 'auditoria');
  try
    lReturnValue := FDropIndexCommandTextGenerator.GenerateText(lDropIndex);
    {$IFDEF POSTGRE}
    CheckEqualsString('drop index AUDITORIA.IDX_PRODUTO_DATA_CADASTRO', lReturnValue);
    {$ENDIF}
    {$IFDEF MSSQL}
    CheckEqualsString('drop index IDX_PRODUTO_DATA_CADASTRO on AUDITORIA.PRODUTO', lReturnValue);
    {$ENDIF}
    {$IFDEF FIREBIRD}
    CheckEqualsString('drop index IDX_PRODUTO_DATA_CADASTRO', lReturnValue);
    {$ENDIF}
  finally
    lDropIndex.Free;
  end;
end;

initialization
  RegisterTest(TestTCreateIndexCommandTextGenerator.Suite);
  RegisterTest(TestTDropIndexCommandTextGenerator.Suite);

end.
