unit uTestFieldCommandTextGenerator;

{$I FreedomORM.inc}

interface

uses
  TestFramework, AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.TextGenerator.CustomFieldCommandTextGenerator,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.TextGenerator.FieldCommandTextGenerator;

type
  TestTCustomFieldCommandTextGenerator = class(TTestCase)
  strict private
    FCustomFieldCommandTextGenerator: TCustomFieldCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSmallintFieldCommand;
    procedure TestIntegerFieldCommand;
    procedure TestInt64FieldCommand;
    procedure TestFloatFieldCommand;
    procedure TestXMLFieldCommand;
  end;

  TestTSizedFieldCommandTextGenerator = class(TTestCase)
  strict private
    FSizedFieldCommandTextGenerator: TSizedFieldCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestVarcharFieldCommand;
    procedure TestCharFieldCommand;
  end;

  TestTScaledFieldCommandTextGenerator = class(TTestCase)
  strict private
    FScaledFieldCommandTextGenerator: TScaledFieldCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNumericFieldCommand;
  end;


implementation

uses
  AM.Freedom.SQLCommands.Fields, AM.Freedom.SQLMappers.MSSQLMapper,
  System.SysUtils, AM.Freedom.EnumerationTypes, AM.Freedom.Exceptions;

procedure TestTCustomFieldCommandTextGenerator.SetUp;
begin
  FCustomFieldCommandTextGenerator := TCustomFieldCommandTextGenerator.Create;
end;

procedure TestTCustomFieldCommandTextGenerator.TearDown;
begin
  FCustomFieldCommandTextGenerator.Free;
  FCustomFieldCommandTextGenerator := nil;
end;

procedure TestTCustomFieldCommandTextGenerator.TestFloatFieldCommand;
var
  lReturnValue: string;
  lField: TFloatFieldCommand;
begin
  lField := TFloatFieldCommand.Create('ATIVO', nNotNull);
  lReturnValue := FCustomFieldCommandTextGenerator.GenerateText(lField);
  {$IFNDEF POSTGRE}
  CheckEqualsString('ATIVO float not null', lReturnValue);
  {$ELSE}
  CheckEqualsString('ATIVO real not null', lReturnValue);
  {$ENDIF}
  FreeAndNil(lField);
end;

procedure TestTCustomFieldCommandTextGenerator.TestInt64FieldCommand;
var
  lReturnValue: string;
  lField: TInt64FieldCommand;
begin
  lField := TInt64FieldCommand.Create('ATIVO', nNotNull, '', TValueArgument.CreateAs<Integer>(1));
  lReturnValue := FCustomFieldCommandTextGenerator.GenerateText(lField);
  CheckEqualsString('ATIVO bigint not null default 1', lReturnValue);
  FreeAndNil(lField);
end;

procedure TestTCustomFieldCommandTextGenerator.TestIntegerFieldCommand;
var
  lReturnValue: string;
  lField: TIntegerFieldCommand;
begin
  lField := TIntegerFieldCommand.Create('ATIVO', nNotNull, '', TValueArgument.CreateAs<Integer>(1));
  lReturnValue := FCustomFieldCommandTextGenerator.GenerateText(lField);
  CheckEqualsString('ATIVO integer not null default 1', lReturnValue);
  FreeAndNil(lField);
end;

procedure TestTCustomFieldCommandTextGenerator.TestSmallintFieldCommand;
var
  lReturnValue: string;
  lField: TSmallintFieldCommand;
begin
  lField := TSmallintFieldCommand.Create('ATIVO', nNotNull, '', TValueArgument.CreateAs<Integer>(1));
  lReturnValue := FCustomFieldCommandTextGenerator.GenerateText(lField);
  CheckEqualsString('ATIVO smallint not null default 1', lReturnValue);
  FreeAndNil(lField);
end;

procedure TestTCustomFieldCommandTextGenerator.TestXMLFieldCommand;
var
  lReturnValue: string;
  lField: TXMLFieldCommand;
begin
  lField := TXMLFieldCommand.Create('ARQUIVO_XML', nNotNull);
  try
    {$IFDEF FIREBIRD}
    ExpectedException := EInvalidFieldCommandForPersistentClass;
    {$ENDIF}
    lReturnValue := FCustomFieldCommandTextGenerator.GenerateText(lField);
    {$IFNDEF FIREBIRD}
    CheckEqualsString('ARQUIVO_XML XML not null', lReturnValue);
    {$ENDIF}
  finally
    FreeAndNil(lField);
  end;
end;

{ TestTSizedFieldCommandTextGenerator }

procedure TestTSizedFieldCommandTextGenerator.SetUp;
begin
  inherited;
  FSizedFieldCommandTextGenerator := TSizedFieldCommandTextGenerator.Create;
end;

procedure TestTSizedFieldCommandTextGenerator.TearDown;
begin
  inherited;
  FreeAndNil(FSizedFieldCommandTextGenerator);
end;

procedure TestTSizedFieldCommandTextGenerator.TestCharFieldCommand;
var
  lReturnValue: string;
  lField: TCharFieldCommand;
begin
  lField := TCharFieldCommand.Create('ATIVO', 1, nNotNull, '', TValueArgument.CreateAs<String>('S'));
  lReturnValue := FSizedFieldCommandTextGenerator.GenerateText(lField);
  CheckEqualsString('ATIVO char(1) not null default ''S''', lReturnValue);
  FreeAndNil(lField);
end;

procedure TestTSizedFieldCommandTextGenerator.TestVarcharFieldCommand;
var
  lReturnValue: string;
  lField: TVarcharFieldCommand;
begin
  lField := TVarcharFieldCommand.Create('ATIVO', 1, nNotNull, '', TValueArgument.CreateAs<String>('S'));
  lReturnValue := FSizedFieldCommandTextGenerator.GenerateText(lField);
  CheckEqualsString('ATIVO varchar(1) not null default ''S''', lReturnValue);
  FreeAndNil(lField);
end;

{ TestTScaledFieldCommandTextGenerator }

procedure TestTScaledFieldCommandTextGenerator.SetUp;
begin
  inherited;
  FScaledFieldCommandTextGenerator := TScaledFieldCommandTextGenerator.Create;
end;

procedure TestTScaledFieldCommandTextGenerator.TearDown;
begin
  inherited;
  FreeAndNil(FScaledFieldCommandTextGenerator);
end;

procedure TestTScaledFieldCommandTextGenerator.TestNumericFieldCommand;
var
  lReturnValue: string;
  lField: TNumericFieldCommand;
begin
  lField := TNumericFieldCommand.Create('ATIVO', 15, 4, nNotNull);
  lReturnValue := FScaledFieldCommandTextGenerator.GenerateText(lField);
  CheckEqualsString('ATIVO numeric(15, 4) not null', lReturnValue);
  FreeAndNil(lField);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTCustomFieldCommandTextGenerator.Suite);
  RegisterTest(TestTSizedFieldCommandTextGenerator.Suite);
  RegisterTest(TestTScaledFieldCommandTextGenerator.Suite);
end.

