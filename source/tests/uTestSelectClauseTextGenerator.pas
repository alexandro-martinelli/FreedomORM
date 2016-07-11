unit uTestSelectClauseTextGenerator;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.TextGenerator.CustomSelectTextGenerator,
  AM.Freedom.TextGenerator.CustomClauseTextGenerator,
  AM.Freedom.SQLMappers.SelectClause,
  AM.Freedom.DefaultsClassRegister;

type
  TestTSelectClauseTextGenerator = class(TTestCase)
  strict private
    FSelectClauseTextGenerator: TCustomSelectTextGenerator;
    FSelect: TSelectClause;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGenerateTextManual;
    procedure TestGenerateTextManualWithLimitRows;
    procedure TestGenerateTextFactory;
    procedure TestGenerateTextFactoryWithLimitRows;
    procedure TestGenerateTextWithFromClass;
    procedure TestGenerateTextWithJoinClass;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLMappers.OrderByClause,
  AM.Freedom.SQLMappers.JoinClause,
  AM.Freedom.SQLMappers.MSSQLMapper,
  uObjectsTests;

procedure TestTSelectClauseTextGenerator.SetUp;
begin
  FSelectClauseTextGenerator := TCustomSelectTextGenerator.Create;
end;

procedure TestTSelectClauseTextGenerator.TearDown;
begin
  FreeAndNil(FSelectClauseTextGenerator);
  FreeAndNil(FSelect);
end;

procedure TestTSelectClauseTextGenerator.TestGenerateTextFactory;
var
  lReturnValue: string;
begin
  FSelect := TSelectClause.CreateFromTable('CLIENTES', 'CLI').
          Field('ID', 'CLI').
          Field('NOME', 'CLI', 'NOME_CLIENTE').
          Field('SOBRENOME', 'CLI').
          Field('SEXO', 'CLI').
          Field('NOME', 'CID', 'CIDADE').
          JoinTable('CIDADES', [TCriteria.CreateAsEqual(TFieldArgument.Create('ID', 'CID'), TFieldArgument.Create('ID_CIDADE', 'CLI'))], 'CI').
          Where(TCriteria.CreateAsLessThanOrEqualTo(TFieldArgument.Create('ID', 'CLI'), TValueArgument.CreateAs<Integer>(50))).
          OrderBy('NOME', 1, 'CLI');
  lReturnValue := FSelectClauseTextGenerator.GenerateText(FSelect);
  CheckEqualsString('select CLI.ID, CLI.NOME as NOME_CLIENTE, CLI.SOBRENOME, CLI.SEXO, CID.NOME as CIDADE' +
      ' from CLIENTES CLI' {$IFDEF MSSQL} + ' with (nolock)' {$ENDIF} +
      ' join CIDADES CI on (CID.ID = CLI.ID_CIDADE)' +
      ' where (CLI.ID <= 50) order by CLI.NOME', lReturnValue);
end;

procedure TestTSelectClauseTextGenerator.TestGenerateTextFactoryWithLimitRows;
var
  lReturnValue: string;
begin
  FSelect := TSelectClause.CreateFromTable('CLIENTES', 'CLI');
  FSelect.WhereClause.LimitRows := 50;
  FSelect.Field('ID', 'CLI').
          Field('NOME', 'CLI', 'NOME_CLIENTE').
          Field('SOBRENOME', 'CLI').
          Field('SEXO', 'CLI').
          Field('NOME', 'CID', 'CIDADE').
          JoinTable('CIDADES', [TCriteria.CreateAsEqual(TFieldArgument.Create('ID', 'CID'), TFieldArgument.Create('ID_CIDADE', 'CLI'))], 'CI').
          Where(TCriteria.CreateAsLessThanOrEqualTo(TFieldArgument.Create('ID', 'CLI'), TValueArgument.CreateAs<Integer>(50))).
          OrderBy('NOME', 1, 'CLI');
  lReturnValue := FSelectClauseTextGenerator.GenerateText(FSelect);
  CheckEqualsString('select' + {$IFDEF MSSQL} ' top 50' + {$ELSEIF Defined(FIREBIRD)} ' first 50' + {$IFEND}
      ' CLI.ID, CLI.NOME as NOME_CLIENTE, CLI.SOBRENOME, CLI.SEXO, CID.NOME as CIDADE' +
      ' from CLIENTES CLI' {$IFDEF MSSQL} + ' with (nolock)' {$ENDIF} +
      ' join CIDADES CI on (CID.ID = CLI.ID_CIDADE)' +
      ' where (CLI.ID <= 50) order by CLI.NOME' {$IFDEF POSTGRE} + ' limit 50' {$ENDIF}, lReturnValue);
end;

procedure TestTSelectClauseTextGenerator.TestGenerateTextManual;
var
  lReturnValue: string;
begin
  FSelect := TSelectClause.Create;
  FSelect.FieldList.AddField('ID', 'CLI');
  FSelect.FieldList.AddField('NOME', 'CLI', 'NOME_CLIENTE');
  FSelect.FieldList.AddField('SOBRENOME', 'CLI');
  FSelect.FieldList.AddField('SEXO', 'CLI');
  FSelect.FieldList.AddField('NOME', 'CID', 'CIDADE');
  FSelect.FromClause.Argument := TTableArgument.Create('CLIENTES', 'CLI');
  FSelect.JoinClause.Add(TJoin.Create(TTableArgument.Create('CIDADES', 'CI'), [TCriteria.CreateAsEqual(TFieldArgument.Create('ID', 'CID'), TFieldArgument.Create('ID_CIDADE', 'CLI'))]));
  FSelect.WhereClause.ListCriterias.Add(TCriteria.CreateAsLessThanOrEqualTo(TFieldArgument.Create('ID', 'CLI'), TValueArgument.CreateAs<Integer>(50)));
  FSelect.OrderByClause.Add(TOrderBy.Create('NOME', 1, 'CLI'));
  lReturnValue := FSelectClauseTextGenerator.GenerateText(FSelect);
  CheckEqualsString('select CLI.ID, CLI.NOME as NOME_CLIENTE, CLI.SOBRENOME, CLI.SEXO, CID.NOME as CIDADE' +
      ' from CLIENTES CLI' + {$IFDEF MSSQL}' with (nolock)' + {$IFEND}
      ' join CIDADES CI on (CID.ID = CLI.ID_CIDADE)' +
      ' where (CLI.ID <= 50) order by CLI.NOME', lReturnValue);
end;

procedure TestTSelectClauseTextGenerator.TestGenerateTextManualWithLimitRows;
var
  lReturnValue: string;
begin
  FSelect := TSelectClause.Create;
  FSelect.FieldList.AddField('ID', 'CLI');
  FSelect.FieldList.AddField('NOME', 'CLI', 'NOME_CLIENTE');
  FSelect.FieldList.AddField('SOBRENOME', 'CLI');
  FSelect.FieldList.AddField('SEXO', 'CLI');
  FSelect.FieldList.AddField('NOME', 'CID', 'CIDADE');
  FSelect.FromClause.Argument := TTableArgument.Create('CLIENTES', 'CLI');
  FSelect.JoinClause.Add(TJoin.Create(TTableArgument.Create('CIDADES', 'CI'), [TCriteria.CreateAsEqual(TFieldArgument.Create('ID', 'CID'), TFieldArgument.Create('ID_CIDADE', 'CLI'))]));
  FSelect.WhereClause.AddCriteria(TCriteria.CreateAsLessThanOrEqualTo(TFieldArgument.Create('ID', 'CLI'), TValueArgument.CreateAs<Integer>(50)));
  FSelect.WhereClause.LimitRows := 100;
  FSelect.OrderByClause.Add(TOrderBy.Create('NOME', 1, 'CLI'));
  lReturnValue := FSelectClauseTextGenerator.GenerateText(FSelect);
  CheckEqualsString('select' + {$IFDEF MSSQL} ' top 100' + {$ELSEIF Defined(FIREBIRD)} ' first 100' + {$IFEND}
      ' CLI.ID, CLI.NOME as NOME_CLIENTE, CLI.SOBRENOME, CLI.SEXO, CID.NOME as CIDADE' +
      ' from CLIENTES CLI' {$IFDEF MSSQL} + ' with (nolock)' {$ENDIF} +
      ' join CIDADES CI on (CID.ID = CLI.ID_CIDADE)' +
      ' where (CLI.ID <= 50) order by CLI.NOME'{$IFDEF POSTGRE} + ' limit 100' {$ENDIF}, lReturnValue);
end;

procedure TestTSelectClauseTextGenerator.TestGenerateTextWithFromClass;
var
  lReturnValue: string;
begin
  FSelect := TSelectClause.CreateFrom<TProduto>;
  FSelect.FieldList.AddField('ID', 'CLI');
  FSelect.FieldList.AddField('NOME', 'CLI', 'NOME_CLIENTE');
  FSelect.FieldList.AddField('SOBRENOME', 'CLI');
  FSelect.FieldList.AddField('SEXO', 'CLI');
  FSelect.FieldList.AddField('NOME', 'CID', 'CIDADE');
  FSelect.JoinClause.Add(TJoin.Create(TTableArgument.Create('CIDADES', 'CI'), [TCriteria.CreateAsEqual(TFieldArgument.Create('ID', 'CID'), TFieldArgument.Create('ID_CIDADE', 'CLI'))]));
  FSelect.WhereClause.AddCriteria(TCriteria.CreateAsLessThanOrEqualTo(TFieldArgument.Create('ID', 'CLI'), TValueArgument.CreateAs<Integer>(50)));
  FSelect.WhereClause.LimitRows := 100;
  FSelect.OrderByClause.Add(TOrderBy.Create('NOME', 1, 'CLI'));
  lReturnValue := FSelectClauseTextGenerator.GenerateText(FSelect);
  CheckEqualsString('select' + {$IFDEF MSSQL} ' top 100'  + {$ELSEIF Defined(FIREBIRD)} ' first 100' + {$IFEND}
      ' CLI.ID, CLI.NOME as NOME_CLIENTE, CLI.SOBRENOME, CLI.SEXO, CID.NOME as CIDADE' +
      ' from ' + {$IFDEF MSSQL} 'DBO.' + {$ELSEIF Define POSTGRE} 'PUBLIC.' + {$IFEND} 'PRODUTOS P' {$IFDEF MSSQL} + ' with (nolock)' {$ENDIF} +
      ' join CIDADES CI on (CID.ID = CLI.ID_CIDADE)' +
      ' where (CLI.ID <= 50) order by CLI.NOME' {$IFDEF POSTGRE} + ' limit 100' {$ENDIF}, lReturnValue);
end;

procedure TestTSelectClauseTextGenerator.TestGenerateTextWithJoinClass;
var
  lReturnValue: string;
begin
  FSelect := TSelectClause.Create;
  FSelect.FieldList.AddField('ID', 'CLI');
  FSelect.FieldList.AddField('NOME', 'CLI', 'NOME_CLIENTE');
  FSelect.FieldList.AddField('SOBRENOME', 'CLI');
  FSelect.FieldList.AddField('SEXO', 'CLI');
  FSelect.FieldList.AddField('NOME', 'CID', 'CIDADE');
  FSelect.FromClause.Argument := TTableArgument.Create('CLIENTES', 'CLI');
  FSelect.Join<TProduto>([TCriteria.CreateAsEqual(TFieldArgument.Create('ID', 'CID'), TFieldArgument.Create('ID_CIDADE', 'CLI'))]);
  FSelect.WhereClause.AddCriteria(TCriteria.CreateAsLessThanOrEqualTo(TFieldArgument.Create('ID', 'CLI'), TValueArgument.CreateAs<Integer>(50)));
  FSelect.WhereClause.LimitRows := 100;
  FSelect.OrderByClause.Add(TOrderBy.Create('NOME', 1, 'CLI'));
  lReturnValue := FSelectClauseTextGenerator.GenerateText(FSelect);
  CheckEqualsString('select' + {$IFDEF MSSQL} ' top 100' + {$ELSEIF Defined(FIREBIRD)} ' first 100' + {$IFEND}
      ' CLI.ID, CLI.NOME as NOME_CLIENTE, CLI.SOBRENOME, CLI.SEXO, CID.NOME as CIDADE' +
      ' from CLIENTES CLI' {$IFDEF MSSQL} + ' with (nolock)' {$ENDIF} +
      ' join PRODUTOS P on (CID.ID = CLI.ID_CIDADE)' +
      ' where (CLI.ID <= 50) order by CLI.NOME' {$IFDEF POSTGRE}  + ' limit 100' {$ENDIF}, lReturnValue);
end;

initialization
  RegisterTest(TestTSelectClauseTextGenerator.Suite);

end.



