unit uTestCreateTableCommand;

{$I FreedomORM.inc}

interface

uses
  TestFramework, System.Generics.Collections,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.Constraints,
  AM.Freedom.SQLCommands.TableCommands,
  AM.Freedom.SQLCommands.Fields;

type
  TestTCreateTableCommand = class(TTestCase)
  strict private
    FCreateTableCommand: TCreateTableCommand;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddField;
    procedure TestAddPrimaryKey;
    procedure TestAddForeignKey;
  end;

implementation

procedure TestTCreateTableCommand.SetUp;
begin
  FCreateTableCommand := TCreateTableCommand.Create;
end;

procedure TestTCreateTableCommand.TearDown;
begin
  FCreateTableCommand.Free;
  FCreateTableCommand := nil;
end;

procedure TestTCreateTableCommand.TestAddField;
begin
  FCreateTableCommand.AddField(TVarcharFieldCommand.Create('NOME', 150));
  CheckEquals(1, FCreateTableCommand.Fields.Count);
  CheckEquals(TVarcharFieldCommand, FCreateTableCommand.Fields.Items[0].ClassType);
end;

procedure TestTCreateTableCommand.TestAddPrimaryKey;
begin
  FCreateTableCommand.AddPrimaryKey(TPrimaryKey.Create);
  Check(Assigned(FCreateTableCommand.PrimaryKey));
end;

procedure TestTCreateTableCommand.TestAddForeignKey;
begin
  FCreateTableCommand.AddForeignKey(TForeignKey.Create);
  CheckEquals(1, FCreateTableCommand.ForeignKeys.Count);
  CheckEquals(TForeignKey, FCreateTableCommand.ForeignKeys.Items[0].ClassType);
end;

initialization
  RegisterTest(TestTCreateTableCommand.Suite);

end.

