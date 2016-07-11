unit uTestSequenceTextGenerator;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.SQLCommands.SequenceCommands,
  AM.Freedom.TextGenerator.SequenceCommandsTextGenerators,
  AM.Freedom.TextGenerator.CustomTextGenerator;

type
  TestTCreateSequenceTextGenerator = class(TTestCase)
  strict private
    FCreateSequenceTextGenerator: TCreateSequenceCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestText;
  end;

  TestTAlterSequenceTextGenerator = class(TTestCase)
  strict private
    FAlterSequenceTextGenerator: TAlterSequenceCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestText;
  end;

  TestTDropSequenceTextGenerator = class(TTestCase)
  strict private
    FDropSequenceTextGenerator: TDropSequenceCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestText;
  end;

implementation

procedure TestTCreateSequenceTextGenerator.SetUp;
begin
  FCreateSequenceTextGenerator := TCreateSequenceCommandTextGenerator.Create;
end;

procedure TestTCreateSequenceTextGenerator.TearDown;
begin
  FCreateSequenceTextGenerator.Free;
  FCreateSequenceTextGenerator := nil;
end;

procedure TestTCreateSequenceTextGenerator.TestText;
var
  lCreateSequence: TCreateSequenceCommand;
  lReturnValue: string;
begin
  lCreateSequence := TCreateSequenceCommand.Create;
  try
    lCreateSequence.Name := 'SEQ_PESSOA';
    lReturnValue := FCreateSequenceTextGenerator.GenerateText(lCreateSequence);
    CheckEqualsString('create sequence SEQ_PESSOA', lReturnValue);
  finally
    lCreateSequence.Free;
  end;
end;

procedure TestTAlterSequenceTextGenerator.SetUp;
begin
  FAlterSequenceTextGenerator := TAlterSequenceCommandTextGenerator.Create;
end;

procedure TestTAlterSequenceTextGenerator.TearDown;
begin
  FAlterSequenceTextGenerator.Free;
  FAlterSequenceTextGenerator := nil;
end;

procedure TestTAlterSequenceTextGenerator.TestText;
var
  lAlterSequence: TAlterSequenceCommand;
  lReturnValue: string;
begin
  lAlterSequence := TAlterSequenceCommand.Create;
  try
    lAlterSequence.Name := 'SEQ_PESSOA';
    lAlterSequence.RestartWith := 100;
    lReturnValue := FAlterSequenceTextGenerator.GenerateText(lAlterSequence);
    {$IFNDEF MSSQL}
      CheckEqualsString('alter sequence SEQ_PESSOA restart with 100', lReturnValue);
    {$ENDIF}
  finally
    lAlterSequence.Free;
  end;
end;

{ TestTDropSequenceTextGenerator }

procedure TestTDropSequenceTextGenerator.SetUp;
begin
  inherited;
  FDropSequenceTextGenerator := TDropSequenceCommandTextGenerator.Create;
end;

procedure TestTDropSequenceTextGenerator.TearDown;
begin
  inherited;
  FDropSequenceTextGenerator.Free;
  FDropSequenceTextGenerator := nil;
end;

procedure TestTDropSequenceTextGenerator.TestText;
var
  lDropSequence: TDropSequenceCommand;
  lReturnValue: string;
begin
  lDropSequence := TDropSequenceCommand.Create;
  try
    lDropSequence.Name := 'SEQ_PESSOA';
    lReturnValue := FDropSequenceTextGenerator.GenerateText(lDropSequence);
    CheckEqualsString('drop sequence SEQ_PESSOA', lReturnValue);
  finally
    lDropSequence.Free;
  end;
end;

initialization
  RegisterTest(TestTCreateSequenceTextGenerator.Suite);
  RegisterTest(TestTAlterSequenceTextGenerator.Suite);
  RegisterTest(TestTDropSequenceTextGenerator.Suite);

end.
