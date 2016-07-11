unit uTestSequenceCommands;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  AM.Freedom.SQLCommands.SequenceCommands,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.EnumerationTypes;

type
  TestTCreateSequenceCommand = class(TTestCase)
  strict private
    FCreateSequenceCommand: TCreateSequenceCommand;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTAlterSequenceCommand = class(TTestCase)
  strict private
    FAlterSequenceCommand: TAlterSequenceCommand;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTDropSequenceCommand = class(TTestCase)
  strict private
    FDropSequenceCommand: TDropSequenceCommand;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

implementation

procedure TestTCreateSequenceCommand.SetUp;
begin
  FCreateSequenceCommand := TCreateSequenceCommand.Create;
end;

procedure TestTCreateSequenceCommand.TearDown;
begin
  FCreateSequenceCommand.Free;
  FCreateSequenceCommand := nil;
end;

procedure TestTCreateSequenceCommand.TestProperties;
begin
  FCreateSequenceCommand.Name := 'SEQ_PESSOA';
  CheckEqualsString('SEQ_PESSOA', FCreateSequenceCommand.Name);
end;

procedure TestTAlterSequenceCommand.SetUp;
begin
  FAlterSequenceCommand := TAlterSequenceCommand.Create;
end;

procedure TestTAlterSequenceCommand.TearDown;
begin
  FAlterSequenceCommand.Free;
  FAlterSequenceCommand := nil;
end;

procedure TestTAlterSequenceCommand.TestProperties;
begin
  FAlterSequenceCommand.Name := 'SEQ_PESSOA';
  FAlterSequenceCommand.RestartWith := 1000;
  CheckEqualsString('SEQ_PESSOA', FAlterSequenceCommand.Name);
  CheckEquals(1000, FAlterSequenceCommand.RestartWith);
end;

{ TestTDropSequenceCommand }

procedure TestTDropSequenceCommand.SetUp;
begin
  inherited;
  FDropSequenceCommand := TDropSequenceCommand.Create;
end;

procedure TestTDropSequenceCommand.TearDown;
begin
  inherited;
  FDropSequenceCommand.Free;
  FDropSequenceCommand := nil;
end;

procedure TestTDropSequenceCommand.TestProperties;
begin
  FDropSequenceCommand.Name := 'SEQ_PESSOA';
  CheckEqualsString('SEQ_PESSOA', FDropSequenceCommand.Name);
end;

initialization
  RegisterTest(TestTCreateSequenceCommand.Suite);
  RegisterTest(TestTAlterSequenceCommand.Suite);
  RegisterTest(TestTDropSequenceCommand.Suite);

end.
