unit uTestTableCommands;

{$I FreedomORM.inc}

interface

uses
  TestFramework, AM.Freedom.SQLCommands.Constraints,
  AM.Freedom.SQLCommands.TableFieldCommands,
  AM.Freedom.SQLCommands.CustomTableCommand,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLCommands.Fields;

type
  TestTAddFieldCommand = class(TTestCase)
  strict private
    FAddFieldCommand: TAddFieldCommand;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties; virtual;
  end;

  TestTAlterFieldCommand = class(TTestCase)
  strict private
    FAlterFieldCommand: TAlterFieldCommand;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties; virtual;
  end;

  TestTDropFieldCommand = class(TTestCase)
  strict private
    FDropFieldCommand: TDropFieldCommand;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties; virtual;
  end;

  TestTAddConstraintCommand = class(TTestCase)
  strict private
    FAddConstraintCommand: TAddConstraintCommand;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties; virtual;
  end;

  TestTDropConstraintCommand = class(TTestCase)
  strict private
    FDropConstraintCommand: TDropConstraintCommand;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties; virtual;
  end;

implementation

uses
  System.SysUtils;

{ TestTDropFieldCommand }

procedure TestTDropFieldCommand.SetUp;
begin
  FDropFieldCommand := TDropFieldCommand.Create('CODIGO');
end;

procedure TestTDropFieldCommand.TearDown;
begin
  FDropFieldCommand.Free;
  FDropFieldCommand := nil;
end;

procedure TestTDropFieldCommand.TestProperties;
begin
  CheckEqualsString('CODIGO', FDropFieldCommand.Name);
end;

{ TestTAddConstraintCommand }

procedure TestTAddConstraintCommand.SetUp;
begin
  FAddConstraintCommand := TAddConstraintCommand.Create(TPrimaryKey.Create('PK_CLIENTES', ['ID']));
end;

procedure TestTAddConstraintCommand.TearDown;
begin
  FAddConstraintCommand.Free;
  FAddConstraintCommand := nil;
end;

procedure TestTAddConstraintCommand.TestProperties;
begin
  CheckEquals(TPrimaryKey, FAddConstraintCommand.Constraint.ClassType);
  FAddConstraintCommand.Constraint := TForeignKey.Create('FK_CLIENTES_CIDADES', ['ID_CIDADE'], 'CIDADES', ['ID']);
  CheckEquals(TForeignKey, FAddConstraintCommand.Constraint.ClassType);
end;

{ TestTDropConstraintCommand }

procedure TestTDropConstraintCommand.SetUp;
begin
  FDropConstraintCommand := TDropConstraintCommand.Create('FK_CLIENTES_CIDADES');
end;

procedure TestTDropConstraintCommand.TearDown;
begin
  FDropConstraintCommand.Free;
  FDropConstraintCommand := nil;
end;

procedure TestTDropConstraintCommand.TestProperties;
begin
  CheckEqualsString('FK_CLIENTES_CIDADES', FDropConstraintCommand.Name);
end;

{ TestTAddFieldCommand }

procedure TestTAddFieldCommand.SetUp;
begin
  FAddFieldCommand := TAddFieldCommand.Create(TIntegerFieldCommand.Create('CODIGO'));
end;

procedure TestTAddFieldCommand.TearDown;
begin
  FreeAndNil(FAddFieldCommand);
end;

procedure TestTAddFieldCommand.TestProperties;
begin
  CheckEquals(TIntegerFieldCommand, FAddFieldCommand.Field.ClassType);
  FAddFieldCommand.Field := TVarcharFieldCommand.Create('NOME', 150);
  CheckEquals(TVarcharFieldCommand, FAddFieldCommand.Field.ClassType);
end;

{ TestTAlterFieldCommand }

procedure TestTAlterFieldCommand.SetUp;
begin
  FAlterFieldCommand := TAlterFieldCommand.Create(TVarcharFieldCommand.Create('NOME', 200));
end;

procedure TestTAlterFieldCommand.TearDown;
begin
  inherited;
  FreeAndNil(FAlterFieldCommand);
end;

procedure TestTAlterFieldCommand.TestProperties;
begin
  CheckEquals(TVarcharFieldCommand, FAlterFieldCommand.Field.ClassType);
  FAlterFieldCommand.Field := TIntegerFieldCommand.Create('CODIGO');
  CheckEquals(TIntegerFieldCommand, FAlterFieldCommand.Field.ClassType);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTAddFieldCommand.Suite);
  RegisterTest(TestTAlterFieldCommand.Suite);
  RegisterTest(TestTDropFieldCommand.Suite);
  RegisterTest(TestTAddConstraintCommand.Suite);
  RegisterTest(TestTDropConstraintCommand.Suite);
  
end.

