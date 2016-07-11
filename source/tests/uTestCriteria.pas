unit uTestCriteria;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  System.SysUtils,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.GroupCriteria.ValueCriteria, AM.Freedom.SQLMappers.SelectClause;

type
  TestTCriteria = class(TTestCase)
  strict private
    FCriteria: TCriteria;
  public
    procedure TearDown; override;
  published
    procedure TestCreateAsEqual;
    procedure TestCreateAsLikeLeft;
    procedure TestCreateAsLikeMidle;
    procedure TestCreateAsLikeRigth;
    procedure TestCreateAsIn;
    procedure TestCreateAsBetween;
    procedure TestCreateAsExists;



  end;

implementation

uses
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria.Comparators,
  AM.Freedom.SQLMappers.Arguments, AM.Freedom.SQLMappers.Expressions;

procedure TestTCriteria.TearDown;
begin
  FreeAndNil(FCriteria);
end;

procedure TestTCriteria.TestCreateAsBetween;
var
  lNow: TDate;
begin
  lNow := Now;
  FCriteria := TCriteria.CreateAsBetween(TFieldArgument.Create('DATA', 'CLI'),
      TValueArgument.CreateAs<TDate>(lNow - 30), TValueArgument.CreateAs<TDate>(lNow));
  CheckEqualsString(TFieldArgument(FCriteria.LeftArgument).Name, 'DATA');
  CheckEqualsString(TFieldArgument(FCriteria.LeftArgument).TableAlias, 'CLI');
  CheckEquals(TValueArgument(TBetweenArgument(FCriteria.RigthArgument).BetweenArgument).AsDate, lNow - 30);
  CheckEquals(TValueArgument(TBetweenArgument(FCriteria.RigthArgument).AndArgument).AsDate, lNow);
  CheckEquals(FCriteria.Comparator.ClassType, TBetween);
end;

procedure TestTCriteria.TestCreateAsEqual;
begin
  FCriteria := TCriteria.CreateAsEqual(TFieldArgument.Create('NOME', 'CLI'), TLiteralArgument.Create('Alexandro'));
  CheckEqualsString(TFieldArgument(TCriteria(FCriteria).LeftArgument).Name, 'NOME');
  CheckEqualsString(TFieldArgument(TCriteria(FCriteria).LeftArgument).TableAlias, 'CLI');
  CheckEquals(FCriteria.Comparator.ClassType, TEqual);
end;


procedure TestTCriteria.TestCreateAsExists;
begin
  FCriteria := TCriteria.CreateAsExists(TSelectClause.Create);
  CheckEquals(FCriteria.Comparator.ClassType, TExists);
  CheckEquals(FCriteria.RigthArgument.ClassType, TSelectClause);
end;

procedure TestTCriteria.TestCreateAsIn;
begin
  FCriteria := TCriteria.CreateAsIn(TFieldArgument.Create('NOME', 'CLI'),
      [TLiteralArgument.Create('Alexandro')]);
  CheckEqualsString(TFieldArgument(FCriteria.LeftArgument).Name, 'NOME');
  CheckEqualsString(TFieldArgument(FCriteria.LeftArgument).TableAlias, 'CLI');
  CheckEquals(FCriteria.Comparator.ClassType, TIn);
end;

procedure TestTCriteria.TestCreateAsLikeLeft;
begin
  FCriteria := TCriteria.CreateAsLikeLeft(TFieldArgument.Create('NOME', 'CLI'),
      TLiteralArgument.Create('Alexandro'));
  CheckEqualsString(TFieldArgument(FCriteria.LeftArgument).Name, 'NOME');
  CheckEqualsString(TFieldArgument(FCriteria.LeftArgument).TableAlias, 'CLI');
  CheckEquals(FCriteria.Comparator.ClassType, TLikeLeft);
end;

procedure TestTCriteria.TestCreateAsLikeMidle;
begin
  FCriteria := TCriteria.CreateAsLikeMidle(TFieldArgument.Create('NOME', 'CLI'),
    TLiteralArgument.Create('Alexandro'));
  CheckEqualsString(TFieldArgument(TCriteria(FCriteria).LeftArgument).Name, 'NOME');
  CheckEqualsString(TFieldArgument(TCriteria(FCriteria).LeftArgument).TableAlias, 'CLI');
  CheckEquals(FCriteria.Comparator.ClassType, TLikeMidle);
end;

procedure TestTCriteria.TestCreateAsLikeRigth;
begin
  FCriteria := TCriteria.CreateAsLikeRigth(TFieldArgument.Create('NOME', 'CLI'),
    TLiteralArgument.Create('Alexandro'));
  CheckEqualsString(TFieldArgument(TCriteria(FCriteria).LeftArgument).Name, 'NOME');
  CheckEqualsString(TFieldArgument(TCriteria(FCriteria).LeftArgument).TableAlias, 'CLI');
  CheckEquals(FCriteria.Comparator.ClassType, TLikeRigth);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTCriteria.Suite);
end.

