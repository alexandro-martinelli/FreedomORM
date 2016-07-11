unit uTestAlterTableCommand;

{$I FreedomORM.inc}

interface

uses
  TestFramework, System.Generics.Collections,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLCommands.CustomTableCommand,
  AM.Freedom.SQLCommands.Constraints,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLCommands.Fields,
  AM.Freedom.SQLCommands.TableFieldCommands,
  AM.Freedom.SQLCommands.TableCommands;

type
  TestTAlterTableCommand = class(TTestCase)
  strict private
    FAlterTableCommand: TAlterTableCommand;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddField; virtual;
    procedure TestAlterField; virtual;
    procedure TestDropField; virtual;
    procedure TestAddConstraint; virtual;
    procedure TestDropConstraint; virtual;
  end;

implementation

uses AM.Freedom.EnumerationTypes;

procedure TestTAlterTableCommand.SetUp;
begin
  FAlterTableCommand := TAlterTableCommand.Create;
end;

procedure TestTAlterTableCommand.TearDown;
begin
  FAlterTableCommand.Free;
  FAlterTableCommand := nil;
end;

procedure TestTAlterTableCommand.TestAddField;
begin
  FAlterTableCommand.AddField(TVarcharFieldCommand.Create('NOME', 150));
  CheckEquals(1, FAlterTableCommand.Commands.Count);
  CheckEquals(TAddFieldCommand, FAlterTableCommand.Commands.Items[0].ClassType);
end;

procedure TestTAlterTableCommand.TestAlterField;
begin
  FAlterTableCommand.AlterField(TVarcharFieldCommand.Create('NOME', 150), [DataType]);
  CheckEquals(1, FAlterTableCommand.Commands.Count);
  CheckEquals(TAlterFieldCommand, FAlterTableCommand.Commands.Items[0].ClassType);
end;

procedure TestTAlterTableCommand.TestDropField;
begin
  FAlterTableCommand.DropField('NOME');
  CheckEquals(1, FAlterTableCommand.Commands.Count);
  CheckEquals(TDropFieldCommand, FAlterTableCommand.Commands.Items[0].ClassType);
end;

procedure TestTAlterTableCommand.TestAddConstraint;
begin
  FAlterTableCommand.AddConstraint(TForeignKey.Create);
  CheckEquals(1, FAlterTableCommand.Commands.Count);
  CheckEquals(TAddConstraintCommand, FAlterTableCommand.Commands.Items[0].ClassType);
end;

procedure TestTAlterTableCommand.TestDropConstraint;
begin
  FAlterTableCommand.DropConstraint('FK_CLIENTES_CIDADES');
  CheckEquals(1, FAlterTableCommand.Commands.Count);
  CheckEquals(TDropConstraintCommand, FAlterTableCommand.Commands.Items[0].ClassType);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTAlterTableCommand.Suite);
end.

