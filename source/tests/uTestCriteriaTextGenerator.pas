unit uTestCriteriaTextGenerator;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.TextGenerator.CriteriaTextGenerator,
  AM.Freedom.SQLMappers.SelectClause,
  AM.Freedom.SQLMappers.MSSQLMapper;

type
  TestTCriteriaTextGenerator = class(TTestCase)
  strict private
    FCriteriaTextGenerator: TCriteriaTextGenerator;
    function GetDateFormatForTest: String;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestString;
    procedure TestInteger;
    procedure TestDate;
    procedure TestTime;
    procedure TestDateTime;
    procedure TestDouble;
    procedure TestCurrency;
    procedure TestBoolean;

    procedure TestLiterals;
    procedure TestCharCases;

    procedure TestDifferent;
    procedure TestGreaterThan;
    procedure TestLessThan;
    procedure TestGreaterThanOrEqualTo;
    procedure TestLessThanOrEqualTo;
    procedure TestNull;
    procedure TestNotNull;
    procedure TestContaining;
    procedure TestStartingWith;
    procedure TestSimpleLikeLeft;
    procedure TestSimpleLikeMidle;
    procedure TestSimpleLikeRigth;

    procedure TestComplexLikeLeft;
    procedure TestComplexLikeMidle;
    procedure TestComplexLikeRigth;

    procedure TestBetween;
    procedure TestIn;
    procedure TestExists;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.EnumerationTypes, AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLMappers.Expressions;

function TestTCriteriaTextGenerator.GetDateFormatForTest: String;
begin
  {$IFDEF MSSQL}
  Result := 'dd-MM-yyyy';
  {$ENDIF}
  {$IFDEF POSTGRE}
  Result := 'MM/dd/yyyy';
  {$ENDIF}
  {$IFDEF FIREBIRD}
  Result := 'dd.MM.yyyy';
  {$ENDIF}
end;

procedure TestTCriteriaTextGenerator.SetUp;
begin
  FCriteriaTextGenerator := TCriteriaTextGenerator.Create;
end;

procedure TestTCriteriaTextGenerator.TearDown;
begin
  FCriteriaTextGenerator.Free;
  FCriteriaTextGenerator := nil;
end;

procedure TestTCriteriaTextGenerator.TestBetween;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsBetween(TFieldArgument.Create('CODIGO'), TValueArgument.CreateAsInteger(100),
      TValueArgument.CreateAsInteger(1000));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(CODIGO between 100 and 1000)', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestBoolean;
var
  lReturnValue: string;
  lCriteria: TCriteria;
  lBoolean: Boolean;
begin
  lBoolean := True;
  lCriteria := TCriteria.CreateAsEqual(TFieldArgument.Create('ATIVO'), TValueArgument.CreateAsBoolean(lBoolean));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  {$IFDEF MSSQL}
  CheckEqualsString('(ATIVO = 1)', lReturnValue);
  {$ELSEIF define(Firebird)}
  CheckEqualsString('(ATIVO = TRUE)', lReturnValue);
  {$ELSEIF define(Postgre)}
  CheckEqualsString('(ATIVO = ''f'')', lReturnValue);
  {$IFEND}
  FreeAndNil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestCharCases;
var
  ReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsEqual(TUpper.Create(TFieldArgument.Create('DATANASC', '', '')),
    TUpper.Create(TLiteralArgument.Create('CURRENT_TIMESTAMP')));
  ReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(upper(DATANASC) = upper(CURRENT_TIMESTAMP))', ReturnValue);
  FreeAndnil(lCriteria);

  lCriteria := TCriteria.CreateAsEqual(TLower.Create(TFieldArgument.Create('DATANASC', '', '')),
    TLower.Create(TLiteralArgument.Create('CURRENT_TIMESTAMP')));
  ReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(lower(DATANASC) = lower(CURRENT_TIMESTAMP))', ReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestComplexLikeLeft;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsLikeLeft(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro Martinelli'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  {$IFDEF MSSQL}
  CheckEqualsString('(NOME like (''%Alexandro%Martinelli''))', lReturnValue);
  {$ELSE}
  CheckEqualsString('(NOME like (''%Alexandro Martinelli''))', lReturnValue);
  {$IFEND}
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestComplexLikeMidle;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsLikeMidle(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro Martinelli'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  {$IFDEF MSSQL}
  CheckEqualsString('(NOME like (''%Alexandro%Martinelli%''))', lReturnValue);
  {$ELSE}
  CheckEqualsString('(NOME like (''%Alexandro Martinelli%''))', lReturnValue);
  {$IFEND}
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestComplexLikeRigth;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsLikeRigth(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro Martinelli'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  {$IFDEF MSSQL}
  CheckEqualsString('(NOME like (''Alexandro%Martinelli%''))', lReturnValue);
  {$ELSE}
  CheckEqualsString('(NOME like (''Alexandro Martinelli%''))', lReturnValue);
  {$IFEND}
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestContaining;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsContaining(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(NOME CONTAINING (''Alexandro''))', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestCurrency;
var
  lReturnValue: string;
  lCriteria: TCriteria;
  lValor: Double;
begin
  lValor := 862.69;
  lCriteria := TCriteria.CreateAsEqual(TFieldArgument.Create('VALOR'), TValueArgument.CreateAsCurrency(lValor));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(VALOR = 862.69)', lReturnValue);
  FreeAndNil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestDate;
var
  lReturnValue: string;
  lCriteria: TCriteria;
  lNow: TDate;
  lFormat: String;
begin
  lNow := Date;
  lCriteria := TCriteria.CreateAsEqual(TFieldArgument.Create('DATANASC'), TValueArgument.CreateAsDate(lNow));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  lFormat := GetDateFormatForTest;
  CheckEqualsString('(DATANASC = ' + QuotedStr(FormatDateTime(lFormat, lNow)) + ')' , lReturnValue);
  FreeAndNil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestDateTime;
var
  lReturnValue: string;
  lCriteria: TCriteria;
  lNow: TDateTime;
begin
  lNow := Now;
  lCriteria := TCriteria.CreateAsEqual(TFieldArgument.Create('DATANASC'), TValueArgument.CreateAsDateTime(lNow));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(DATANASC = ' + QuotedStr(FormatDateTime(GetDateFormatForTest + ' HH:nn:ss', lNow)) + ')' , lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestDifferent;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsDifferent(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(NOME <> ''Alexandro'')', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestDouble;
var
  lReturnValue: string;
  lCriteria: TCriteria;
  lValor: Double;
begin
  lValor := 562.69;
  lCriteria := TCriteria.CreateAsEqual(TFieldArgument.Create('VALOR'), TValueArgument.CreateAsDouble(lValor));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(VALOR = 562.69)', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestExists;
var
  lSelect: TSelectClause;
  lCriteria: TCriteria;
  lReturnValue: string;
begin
  lSelect := TSelectClause.CreateFromTable('CLIENTES', 'CLI');
  lSelect.Field('ID', 'CLI').
          Field('NOME', 'CLI', 'NOME_CLIENTE').
          Field('SOBRENOME', 'CLI').
          Field('SEXO', 'CLI').
          Field('NOME', 'CID', 'CIDADE').
          JoinTable('CIDADES', [TCriteria.CreateAsEqual(TFieldArgument.Create('ID', 'CID'), TFieldArgument.Create('ID_CIDADE', 'CLI'))], 'CI').
          Where(TCriteria.CreateAsLessThanOrEqualTo(TFieldArgument.Create('ID', 'CLI'), TValueArgument.CreateAs<Integer>(50))).
          OrderBy('NOME', 1, 'CLI');
  lCriteria := TCriteria.CreateAsExists(lSelect);
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(exists ((select CLI.ID, CLI.NOME as NOME_CLIENTE, CLI.SOBRENOME, CLI.SEXO, CID.NOME as CIDADE' +
      ' from CLIENTES CLI'{$IFDEF MSSQL} + ' with (nolock)'{$ENDIF} +
      ' join CIDADES CI on (CID.ID = CLI.ID_CIDADE)' +
      ' where (CLI.ID <= 50) order by CLI.NOME)))', lReturnValue);
  FreeAndNil(lCriteria);

end;

procedure TestTCriteriaTextGenerator.TestGreaterThan;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsGreaterThan(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(NOME > ''Alexandro'')', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestGreaterThanOrEqualTo;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsGreaterThanOrEqualTo(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(NOME >= ''Alexandro'')', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestIn;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsIn(TFieldArgument.Create('NOME'), [TValueArgument.CreateAsString('Alexandro'),
      TValueArgument.CreateAsInteger(10),
      TCoalesce.Create(TFieldArgument.Create('CODIGO'), [TValueArgument.CreateAsInteger(10)])]);
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(NOME in (''Alexandro'', 10, coalesce(CODIGO, 10)))', lReturnValue);
  FreeAndNil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestInteger;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsEqual(TFieldArgument.Create('ID'), TValueArgument.CreateAsInteger(1));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(ID = 1)', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestLessThan;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsLessThan(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(NOME < ''Alexandro'')', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestLessThanOrEqualTo;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsLessThanOrEqualTo(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(NOME <= ''Alexandro'')', lReturnValue);
  FreeAndnil(lCriteria);

end;

procedure TestTCriteriaTextGenerator.TestSimpleLikeLeft;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsLikeLeft(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(NOME like (''%Alexandro''))', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestSimpleLikeMidle;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsLikeMidle(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(NOME like (''%Alexandro%''))', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestSimpleLikeRigth;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsLikeRigth(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(NOME like (''Alexandro%''))', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestLiterals;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsEqual(TFieldArgument.Create('DATANASC'), TLiteralArgument.Create('CURRENT_TIMESTAMP'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(DATANASC = CURRENT_TIMESTAMP)', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestNotNull;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsNotNull(TFieldArgument.Create('NOME'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(NOME is not null)', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestNull;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsNull(TFieldArgument.Create('NOME'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(NOME is null)', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestStartingWith;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsStartingWith(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  {$IFDEF FIREBIRD}
    CheckEqualsString('(NOME starting with (''Alexandro''))', lReturnValue);
  {$ENDIF}
  FreeAndNil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestString;
var
  lReturnValue: string;
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria.CreateAsEqual(TFieldArgument.Create('NOME'), TValueArgument.CreateAsString('Alexandro'));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(NOME = ''Alexandro'')', lReturnValue);
  FreeAndnil(lCriteria);
end;

procedure TestTCriteriaTextGenerator.TestTime;
var
  lReturnValue: string;
  lCriteria: TCriteria;
  lNow: TTime;
begin
  lNow := Time;
  lCriteria := TCriteria.CreateAsEqual(TFieldArgument.Create('DATANASC'), TValueArgument.CreateAsTime(lNow));
  lReturnValue := FCriteriaTextGenerator.GenerateText(lCriteria);
  CheckEqualsString('(DATANASC = ' + QuotedStr(TimeToStr(lNow)) + ')', lReturnValue);
  FreeAndnil(lCriteria);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTCriteriaTextGenerator.Suite);
end.

