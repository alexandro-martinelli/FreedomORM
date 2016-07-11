unit uTestComparators;

{$I FreedomORM.inc}

interface

uses
  TestFramework,
  AM.Freedom.GroupCriteria.Comparators;

type
  TestTComparators = class(TTestCase)
  strict private
    FComparator: TCustomComparator;
  strict protected
    function GetComparatorClass: TComparatorClass; virtual; abstract;
    function GetComparator: TCustomComparator;
  public
    procedure SetUp; override; final;
    procedure TearDown; override; final;
  published
    procedure TestComparator; virtual; abstract;
    procedure TestCompratorType; virtual; abstract;
    procedure TesteAllowsSQLAfter; virtual;
  end;

  TestTEqual = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
  end;

  TestTDifferent = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
  end;

  TestTGreaterThan = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
  end;

  TestTLessThan = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
  end;

  TestTGreaterThanOrEqualTo = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
  end;

  TestTLessThanOrEqualTo = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
  end;

  TestTNull = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
    procedure TesteAllowsSQLAfter; override;
  end;

  TestTContaining = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
  end;

  TestTStartingWith = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
  end;

  TestTLikeLeft = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
  end;

  TestTLikeMidle = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
  end;

  TestTLikeRigth = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
  end;

  TestTBetween = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
  end;

  TestTIn = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
  end;

  TestTExists = class(TestTComparators)
  strict protected
    function GetComparatorClass: TComparatorClass; override;
  published
    procedure TestComparator; override;
    procedure TestCompratorType; override;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Helper.ComparatorType;

{ TestTComparators }

function TestTComparators.GetComparator: TCustomComparator;
begin
  Result := FComparator;
end;

procedure TestTComparators.SetUp;
begin
  inherited;
  FComparator := GetComparatorClass.Create;
end;

procedure TestTComparators.TearDown;
begin
  inherited;
  FreeAndNil(FComparator);
end;

procedure TestTComparators.TesteAllowsSQLAfter;
begin
  Check(FComparator.AllowsSQLAfter)
end;

{ TestTEqual }

function TestTEqual.GetComparatorClass: TComparatorClass;
begin
  Result := TEqual;
end;

procedure TestTEqual.TestComparator;
begin
  CheckEqualsString(cpEqual.ToString, GetComparator.Comparator);
end;

procedure TestTEqual.TestCompratorType;
begin
  Check(cpEqual = GetComparator.ComparatorType);
end;

{ TestTDifferent }

function TestTDifferent.GetComparatorClass: TComparatorClass;
begin
  Result := TDifferent;
end;

procedure TestTDifferent.TestComparator;
begin
  CheckEqualsString(cpDifferent.ToString, GetComparator.Comparator);
end;

procedure TestTDifferent.TestCompratorType;
begin
  Check(cpDifferent = GetComparator.ComparatorType);
end;

{ TestTGreaterThan }

function TestTGreaterThan.GetComparatorClass: TComparatorClass;
begin
  Result := TGreaterThan;
end;

procedure TestTGreaterThan.TestComparator;
begin
  CheckEqualsString(cpGreaterThan.ToString, GetComparator.Comparator);
end;

procedure TestTGreaterThan.TestCompratorType;
begin
  Check(cpGreaterThan = GetComparator.ComparatorType);
end;

{ TestTLessThan }

function TestTLessThan.GetComparatorClass: TComparatorClass;
begin
  Result := TLessThan;
end;

procedure TestTLessThan.TestComparator;
begin
  CheckEqualsString(cpLessThan.ToString, GetComparator.Comparator);
end;

procedure TestTLessThan.TestCompratorType;
begin
  Check(cpLessThan = GetComparator.ComparatorType);
end;

{ TestTGreaterThanOrEqualTo }

function TestTGreaterThanOrEqualTo.GetComparatorClass: TComparatorClass;
begin
  Result := TGreaterThanOrEqualTo;
end;

procedure TestTGreaterThanOrEqualTo.TestComparator;
begin
  CheckEqualsString(cpGreaterThanOrEqualTo.ToString, GetComparator.Comparator);
end;

procedure TestTGreaterThanOrEqualTo.TestCompratorType;
begin
  Check(cpGreaterThanOrEqualTo = GetComparator.ComparatorType);
end;

{ TestTLessThanOrEqualTo }

function TestTLessThanOrEqualTo.GetComparatorClass: TComparatorClass;
begin
  Result := TLessThanOrEqualTo;
end;

procedure TestTLessThanOrEqualTo.TestComparator;
begin
  CheckEqualsString(cpLessThanOrEqualTo.ToString, GetComparator.Comparator);
end;

procedure TestTLessThanOrEqualTo.TestCompratorType;
begin
  Check(cpLessThanOrEqualTo = GetComparator.ComparatorType);
end;

{ TestTNull }

function TestTNull.GetComparatorClass: TComparatorClass;
begin
    Result := TNull;
end;

procedure TestTNull.TestComparator;
begin
  CheckEqualsString(cpNull.ToString, GetComparator.Comparator);
end;

procedure TestTNull.TestCompratorType;
begin
  Check(cpNull = GetComparator.ComparatorType);
end;

procedure TestTNull.TesteAllowsSQLAfter;
begin
  Check(not GetComparator.AllowsSQLAfter);
end;

{ TestTContaining }

function TestTContaining.GetComparatorClass: TComparatorClass;
begin
  Result := TContaining;
end;

procedure TestTContaining.TestComparator;
begin
  CheckEqualsString(cpContaining.ToString, GetComparator.Comparator);
end;

procedure TestTContaining.TestCompratorType;
begin
  Check(cpContaining = GetComparator.ComparatorType);
end;

{ TestTStartingWith }

function TestTStartingWith.GetComparatorClass: TComparatorClass;
begin
  Result := TStartingWith;
end;

procedure TestTStartingWith.TestComparator;
begin
  CheckEqualsString(cpStartingWith.ToString, GetComparator.Comparator);
end;

procedure TestTStartingWith.TestCompratorType;
begin
  Check(cpStartingWith = GetComparator.ComparatorType);
end;

{ TestTLikeLeft }

function TestTLikeLeft.GetComparatorClass: TComparatorClass;
begin
  Result := TLikeLeft;
end;

procedure TestTLikeLeft.TestComparator;
begin
  CheckEqualsString(cpLikeLeft.ToString, GetComparator.Comparator);
end;

procedure TestTLikeLeft.TestCompratorType;
begin
  Check(cpLikeLeft = GetComparator.ComparatorType);
end;

{ TestTLikeMidle }

function TestTLikeMidle.GetComparatorClass: TComparatorClass;
begin
  Result := TLikeMidle;
end;

procedure TestTLikeMidle.TestComparator;
begin
  CheckEqualsString(cpLikeMidle.ToString, GetComparator.Comparator);
end;

procedure TestTLikeMidle.TestCompratorType;
begin
  Check(cpLikeMidle = GetComparator.ComparatorType);
end;

{ TestTLikeRigth }

function TestTLikeRigth.GetComparatorClass: TComparatorClass;
begin
  Result := TLikeRigth;
end;

procedure TestTLikeRigth.TestComparator;
begin
  CheckEqualsString(cpLikeRigth.ToString, GetComparator.Comparator);
end;

procedure TestTLikeRigth.TestCompratorType;
begin
  Check(cpLikeRigth = GetComparator.ComparatorType);
end;

{ TestTBetween }

function TestTBetween.GetComparatorClass: TComparatorClass;
begin
  Result := TBetween;
end;

procedure TestTBetween.TestComparator;
begin
  CheckEqualsString(cpBetween.ToString, GetComparator.Comparator);
end;

procedure TestTBetween.TestCompratorType;
begin
  Check(cpBetween = GetComparator.ComparatorType);
end;

{ TestTIn }

function TestTIn.GetComparatorClass: TComparatorClass;
begin
  Result := TIn;
end;

procedure TestTIn.TestComparator;
begin
  CheckEqualsString(cpIn.ToString, GetComparator.Comparator);
end;

procedure TestTIn.TestCompratorType;
begin
  Check(cpIn = GetComparator.ComparatorType);
end;

{ TestTExists }

function TestTExists.GetComparatorClass: TComparatorClass;
begin
  Result := TExists;
end;

procedure TestTExists.TestComparator;
begin
  CheckEqualsString(cpExists.ToString, GetComparator.Comparator);
end;

procedure TestTExists.TestCompratorType;
begin
  Check(cpExists = GetComparator.ComparatorType);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTEqual.Suite);
  RegisterTest(TestTDifferent.Suite);
  RegisterTest(TestTGreaterThan.Suite);
  RegisterTest(TestTLessThan.Suite);
  RegisterTest(TestTGreaterThanOrEqualTo.Suite);
  RegisterTest(TestTLessThanOrEqualTo.Suite);
  RegisterTest(TestTNull.Suite);
  RegisterTest(TestTContaining.Suite);
  RegisterTest(TestTStartingWith.Suite);
  RegisterTest(TestTLikeLeft.Suite);
  RegisterTest(TestTLikeMidle.Suite);
  RegisterTest(TestTLikeRigth.Suite);
  RegisterTest(TestTBetween.Suite);
  RegisterTest(TestTIn.Suite);
  RegisterTest(TestTExists.Suite);
end.

