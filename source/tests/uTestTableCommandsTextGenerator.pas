unit uTestTableCommandsTextGenerator;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  System.Generics.Collections,
  AM.Freedom.TextGenerator.CustomTableCommandsTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.SQLCommands.TableRowCommands,
  AM.Freedom.TextGenerator.TableCommandsTextGenerator,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLCommands.CustomTableCommand,
  AM.Freedom.SQLCommands.Constraints,
  AM.Freedom.SQLCommands.TableCommands,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLMappers.MSSQLMapper;

type
  TestAddOrAlterFieldCommandTextGenerator = class(TTestCase)
  strict private
    FAddOrAlterFieldCommandTextGenerator: TAddOrAlterFieldCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddField;
    procedure TestAddFieldWithDomain;
    procedure TestAddFieldWithDomainAndIdentity;
    procedure TestAlterField;
    procedure TestAlterFieldWithDomain;
    procedure TestAlterFieldWithDomainAndIdentity;
  end;

  TestDropFieldCommandTextGenerator = class(TTestCase)
  strict private
    FDropFieldCommandTextGenerator: TDropFieldCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateText;
  end;

  TestAddConstraintCommandTextGenerator = class(TTestCase)
  strict private
    FAddConstraintCommandTextGenerator: TAddConstraintCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateText;
  end;

  TestDropConstraintCommandTextGenerator = class(TTestCase)
  strict private
    FDropConstraintCommandTextGenerator: TDropConstraintCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateText;
  end;

  TestCreateTableCommandTextGenerator = class(TTestCase)
  strict private
    FCreateTableCommandTextGenerator: TCreateTableCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateText;
  end;

  TestAlterTableCommandTextGenerator = class(TTestCase)
  strict private
    FAlterTableCommandTextGenerator: TAlterTableCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateText;
  end;

  TestDropTableCommandTextGenerator = class(TTestCase)
  strict private
    FDropTableCommandTextGenerator: TDropTableCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateText;
  end;

  TestUpdateCommandTextGenerator = class(TTestCase)
  strict private
    FUpdateCommandTextGenerator: TUpdateCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateText;
  end;

  TestInsertCommandTextGenerator = class(TTestCase)
  strict private
    FInsertCommandTextGenerator: TInsertCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateTextWithValues;
    procedure TestGenerateTextWithSelect;
  end;

  TestDeleteCommandTextGenerator = class(TTestCase)
  strict private
    FDeleteCommandTextGenerator: TDeleteCommandTextGenerator;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateText;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.SQLCommands.TableFieldCommands,
  AM.Freedom.SQLCommands.Fields,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMappers.SelectClause, AM.Freedom.EnumerationTypes;

procedure TestAddOrAlterFieldCommandTextGenerator.SetUp;
begin
  FAddOrAlterFieldCommandTextGenerator := TAddOrAlterFieldCommandTextGenerator.Create;
end;

procedure TestAddOrAlterFieldCommandTextGenerator.TearDown;
begin
  FAddOrAlterFieldCommandTextGenerator.Free;
  FAddOrAlterFieldCommandTextGenerator := nil;
end;

procedure TestAddOrAlterFieldCommandTextGenerator.TestAddField;
var
  lCommand: TAddFieldCommand;
  lReturnValue: string;
begin
  lCommand := TAddFieldCommand.Create(TIntegerFieldCommand.Create('ID', nNotNull));
  lReturnValue := FAddOrAlterFieldCommandTextGenerator.GenerateText(lCommand);
  FreeAndNil(lCommand);
  CheckEqualsString('add ID integer not null', lReturnValue);
end;

procedure TestAddOrAlterFieldCommandTextGenerator.TestAddFieldWithDomain;
var
  lCommand: TAddFieldCommand;
  lReturnValue: string;
begin
  lCommand := TAddFieldCommand.Create(TIntegerFieldCommand.Create('ID', nNull, 'D_ID'));
  lReturnValue := FAddOrAlterFieldCommandTextGenerator.GenerateText(lCommand);
  FreeAndNil(lCommand);
  CheckEqualsString('add ID D_ID', lReturnValue);
end;

procedure TestAddOrAlterFieldCommandTextGenerator.TestAddFieldWithDomainAndIdentity;
var
  lCommand: TAddFieldCommand;
  lReturnValue: string;
begin
  lCommand := TAddFieldCommand.Create(TIntegerFieldCommand.Create('ID', nNull, 'D_ID', nil, '', True));
  lReturnValue := FAddOrAlterFieldCommandTextGenerator.GenerateText(lCommand);
  FreeAndNil(lCommand);
  CheckEqualsString('add ID D_ID' {$IFNDEF POSTGRE} + ' identity'{$IFEND}, lReturnValue);
end;

procedure TestAddOrAlterFieldCommandTextGenerator.TestAlterField;
var
  lCommand: TAlterFieldCommand;
  lReturnValue: string;
begin
  lCommand := TAlterFieldCommand.Create(TIntegerFieldCommand.Create('ID', nNotNull));
  lReturnValue := FAddOrAlterFieldCommandTextGenerator.GenerateText(lCommand);
  FreeAndNil(lCommand);
  CheckEqualsString('alter ' + {$IFNDEF FIREBIRD} 'column ' +{$IFEND} 'ID integer not null', lReturnValue);
end;

procedure TestAddOrAlterFieldCommandTextGenerator.TestAlterFieldWithDomain;
var
  lCommand: TAlterFieldCommand;
  lReturnValue: string;
begin
  lCommand := TAlterFieldCommand.Create(TIntegerFieldCommand.Create('ID', nNull, 'D_ID'));
  lReturnValue := FAddOrAlterFieldCommandTextGenerator.GenerateText(lCommand);
  FreeAndNil(lCommand);
  CheckEqualsString('alter ' + {$IFNDEF FIREBIRD} 'column ' +{$IFEND} 'ID D_ID', lReturnValue);
end;

procedure TestAddOrAlterFieldCommandTextGenerator.TestAlterFieldWithDomainAndIdentity;
var
  lCommand: TAlterFieldCommand;
  lReturnValue: string;
begin
  lCommand := TAlterFieldCommand.Create(TIntegerFieldCommand.Create('ID', nNull, 'D_ID', nil, '', True));
  lReturnValue := FAddOrAlterFieldCommandTextGenerator.GenerateText(lCommand);
  FreeAndNil(lCommand);
  CheckEqualsString('alter ' + {$IFNDEF FIREBIRD} 'column ' +{$IFEND} 'ID D_ID'{$IFNDEF POSTGRE} + ' identity'{$IFEND}, lReturnValue);
end;

procedure TestDropFieldCommandTextGenerator.SetUp;
begin
  FDropFieldCommandTextGenerator := TDropFieldCommandTextGenerator.Create;
end;

procedure TestDropFieldCommandTextGenerator.TearDown;
begin
  FDropFieldCommandTextGenerator.Free;
  FDropFieldCommandTextGenerator := nil;
end;

procedure TestDropFieldCommandTextGenerator.TestGenerateText;
var
  lCommand: TDropFieldCommand;
  lReturnValue: string;
begin
  lCommand := TDropFieldCommand.Create('ID');
  lReturnValue := FDropFieldCommandTextGenerator.GenerateText(lCommand);
  FreeAndNil(lCommand);
  CheckEqualsString('drop ' + {$IFNDEF FIREBIRD} 'column ' +{$IFEND} 'ID', lReturnValue);
end;

procedure TestAddConstraintCommandTextGenerator.SetUp;
begin
  FAddConstraintCommandTextGenerator := TAddConstraintCommandTextGenerator.Create;
end;

procedure TestAddConstraintCommandTextGenerator.TearDown;
begin
  FAddConstraintCommandTextGenerator.Free;
  FAddConstraintCommandTextGenerator := nil;
end;

procedure TestAddConstraintCommandTextGenerator.TestGenerateText;
var
  lCommand: TAddConstraintCommand;
  lReturnValue: string;
begin
  lCommand := TAddConstraintCommand.Create(TPrimaryKey.Create('PK_PESSOAS', ['ID']));
  lReturnValue := FAddConstraintCommandTextGenerator.GenerateText(lCommand);
  FreeAndNil(lCommand);
  CheckEqualsString('add constraint PK_PESSOAS primary key (ID)', lReturnValue);
end;

procedure TestDropConstraintCommandTextGenerator.SetUp;
begin
  FDropConstraintCommandTextGenerator := TDropConstraintCommandTextGenerator.Create;
end;

procedure TestDropConstraintCommandTextGenerator.TearDown;
begin
  FDropConstraintCommandTextGenerator.Free;
  FDropConstraintCommandTextGenerator := nil;
end;

procedure TestDropConstraintCommandTextGenerator.TestGenerateText;
var
  lCommand: TDropConstraintCommand;
  lReturnValue: string;
begin
  lCommand := TDropConstraintCommand.Create('PK_PESSOAS');
  lReturnValue := FDropConstraintCommandTextGenerator.GenerateText(lCommand);
  FreeAndNil(lCommand);
  CheckEqualsString('drop constraint PK_PESSOAS', lReturnValue);
end;

procedure TestCreateTableCommandTextGenerator.SetUp;
begin
  FCreateTableCommandTextGenerator := TCreateTableCommandTextGenerator.Create;
end;

procedure TestCreateTableCommandTextGenerator.TearDown;
begin
  FCreateTableCommandTextGenerator.Free;
  FCreateTableCommandTextGenerator := nil;
end;

procedure TestCreateTableCommandTextGenerator.TestGenerateText;
var
  lCommand: TCreateTableCommand;
  lReturnValue: string;
begin
  lCommand := TCreateTableCommand.Create;
  lCommand.AddField(TIntegerFieldCommand.Create('ID', nNotNull)).
           AddField(TVarcharFieldCommand.Create('NOME', 150)).
           AddField(TIntegerFieldCommand.Create('ID_CIDADE')).
           AddPrimaryKey(TPrimaryKey.Create('PK_PESSOA', ['ID'])).
           AddForeignKey(TForeignKey.Create('FK_PESSOA_CIDADE', ['ID_CIDADE'], 'CIDADE', ['ID'])).Name := 'PESSOA';
  lReturnValue := FCreateTableCommandTextGenerator.GenerateText(lCommand);
  FreeAndNil(lCommand);
  CheckEqualsString('create table PESSOA (ID integer not null, NOME varchar(150), ID_CIDADE integer,' +
      ' constraint PK_PESSOA primary key (ID),' +
      ' constraint FK_PESSOA_CIDADE foreign key (ID_CIDADE) references CIDADE (ID))', lReturnValue);
end;

procedure TestAlterTableCommandTextGenerator.SetUp;
begin
  FAlterTableCommandTextGenerator := TAlterTableCommandTextGenerator.Create;
end;

procedure TestAlterTableCommandTextGenerator.TearDown;
begin
  FAlterTableCommandTextGenerator.Free;
  FAlterTableCommandTextGenerator := nil;
end;

procedure TestAlterTableCommandTextGenerator.TestGenerateText;
var
  lCommand: TAlterTableCommand;
  lReturnValue: string;
begin
  lCommand := TAlterTableCommand.Create;
  lCommand.AddField(TIntegerFieldCommand.Create('ID', nNotNull)).
           AddField(TVarcharFieldCommand.Create('NOME', 150)).
           AddField(TIntegerFieldCommand.Create('ID_CIDADE')).
           AddConstraint(TPrimaryKey.Create('PK_PESSOA', ['ID'])).
           AddConstraint(TForeignKey.Create('FK_PESSOA_CIDADE', ['ID_CIDADE'], 'CIDADE', ['ID'])).Name := 'PESSOA';
  lReturnValue := FAlterTableCommandTextGenerator.GenerateText(lCommand);
  FreeAndNil(lCommand);
  {$IFDEF MSSQL}
  CheckEqualsString('alter table PESSOA add ID integer not null; alter table PESSOA add NOME varchar(150);' +
      ' alter table PESSOA add ID_CIDADE integer;' +
      ' alter table PESSOA add constraint PK_PESSOA primary key (ID);' +
      ' alter table PESSOA add constraint FK_PESSOA_CIDADE foreign key (ID_CIDADE) references CIDADE (ID);', lReturnValue);
  {$ELSE}
    CheckEqualsString('alter table PESSOA add ID integer not null, add NOME varchar(150), add ID_CIDADE integer, add constraint PK_PESSOA primary key (ID), add constraint FK_PESSOA_CIDADE foreign key (ID_CIDADE) references CIDADE (ID)', lReturnValue);
  {$ENDIF}
end;

procedure TestDropTableCommandTextGenerator.SetUp;
begin
  FDropTableCommandTextGenerator := TDropTableCommandTextGenerator.Create;
end;

procedure TestDropTableCommandTextGenerator.TearDown;
begin
  FDropTableCommandTextGenerator.Free;
  FDropTableCommandTextGenerator := nil;
end;

procedure TestDropTableCommandTextGenerator.TestGenerateText;
var
  lCommand: TDropTableCommand;
  lReturnValue: string;
begin
  lCommand := TDropTableCommand.Create('PESSOA');
  lReturnValue := FDropTableCommandTextGenerator.GenerateText(lCommand);
  FreeAndNil(lCommand);
  CheckEqualsString('drop table PESSOA', lReturnValue);
end;

procedure TestUpdateCommandTextGenerator.SetUp;
begin
  FUpdateCommandTextGenerator := TUpdateCommandTextGenerator.Create;
end;

procedure TestUpdateCommandTextGenerator.TearDown;
begin
  FUpdateCommandTextGenerator.Free;
  FUpdateCommandTextGenerator := nil;
end;

procedure TestUpdateCommandTextGenerator.TestGenerateText;
var
  lCommand: TUpdateCommand;
  lReturnValue: string;
begin
  lCommand := TUpdateCommand.Create;
  lCommand.TableName('PESSOA').
           FieldValue<String>('APELIDO', 'Alexandro').
           Where(TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAs<Integer>(1)));
  lReturnValue := FUpdateCommandTextGenerator.GenerateText(lCommand);
  FreeAndNil(lCommand);
  CheckEqualsString('update PESSOA set APELIDO = :APELIDO where (ID = 1)', lReturnValue);
end;

procedure TestInsertCommandTextGenerator.SetUp;
begin
  FInsertCommandTextGenerator := TInsertCommandTextGenerator.Create;
end;

procedure TestInsertCommandTextGenerator.TearDown;
begin
  FInsertCommandTextGenerator.Free;
  FInsertCommandTextGenerator := nil;
end;

procedure TestInsertCommandTextGenerator.TestGenerateTextWithSelect;
var
  lCommand: TInsertCommand;
  lReturnValue: string;
  lSelect: TSelectClause;
begin
  lCommand := TInsertCommand.Create;
  lSelect := TSelectClause.CreateFromTable('FORNECEDORES');
  try
    lSelect.Field('ID').Field('NOME').Field('APELIDO');
    lCommand.IntoTable('PESSOA').Field('ID').Field('NOME').Field('APELIDO').Select(lSelect);
    lReturnValue := FInsertCommandTextGenerator.GenerateText(lCommand);
    CheckEqualsString('insert into PESSOA (ID, NOME, APELIDO) (select ID, NOME, APELIDO from FORNECEDORES' {$IFDEF MSSQL} + ' with (nolock)' {$ENDIF} + ')', lReturnValue);
  finally
    FreeAndNil(lCommand);
  end;
end;

procedure TestInsertCommandTextGenerator.TestGenerateTextWithValues;
var
  lCommand: TInsertCommand;
  lReturnValue: string;
begin
  lCommand := TInsertCommand.Create;
  try
    lCommand.IntoTable('PESSOA').
             FieldValue<Integer>('ID', 1).
             FieldValue<String>('NOME', 'Alexandro').
             FieldValue<String>('APELIDO', 'Alexandro');
    lReturnValue := FInsertCommandTextGenerator.GenerateText(lCommand);
    CheckEqualsString('insert into PESSOA (ID, NOME, APELIDO) values (:ID, :NOME, :APELIDO)', lReturnValue);
  finally
    FreeAndNil(lCommand);
  end;
end;

procedure TestDeleteCommandTextGenerator.SetUp;
begin
  FDeleteCommandTextGenerator := TDeleteCommandTextGenerator.Create;
end;

procedure TestDeleteCommandTextGenerator.TearDown;
begin
  FDeleteCommandTextGenerator.Free;
  FDeleteCommandTextGenerator := nil;
end;

procedure TestDeleteCommandTextGenerator.TestGenerateText;
var
  lCommand: TDeleteCommand;
  lReturnValue: string;
begin
  lCommand := TDeleteCommand.Create;
  try
    lCommand.From('PESSOA').
             Where(TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAs<Integer>(1)));
    lReturnValue := FDeleteCommandTextGenerator.GenerateText(lCommand);
    CheckEqualsString('delete from PESSOA where (ID = 1)', lReturnValue);
  finally
    FreeAndNil(lCommand);
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestAddOrAlterFieldCommandTextGenerator.Suite);
  RegisterTest(TestDropFieldCommandTextGenerator.Suite);
  RegisterTest(TestAddConstraintCommandTextGenerator.Suite);
  RegisterTest(TestDropConstraintCommandTextGenerator.Suite);
  RegisterTest(TestCreateTableCommandTextGenerator.Suite);
  RegisterTest(TestAlterTableCommandTextGenerator.Suite);
  RegisterTest(TestDropTableCommandTextGenerator.Suite);
  RegisterTest(TestUpdateCommandTextGenerator.Suite);
  RegisterTest(TestInsertCommandTextGenerator.Suite);
  RegisterTest(TestDeleteCommandTextGenerator.Suite);

end.
