unit uTestFromClause;

{$I FreedomORM.inc}

interface

uses
  TestFramework, AM.Freedom.SQLMappers.FromClause,
  AM.Freedom.SQLMappers.SQLClause, AM.Freedom.SQLMapper.CustomArgument;

type
  TestTFromClause = class(TTestCase)
  strict private
    FFromClause: TFromClause;
  public
    procedure TearDown; override;
  published
    procedure TestProperties;
    procedure TestInvalidArgument;
  end;

implementation

uses
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.Exceptions, System.SysUtils;

procedure TestTFromClause.TearDown;
begin
  FFromClause.Free;
  FFromClause := nil;
end;

procedure TestTFromClause.TestInvalidArgument;
var
  lFieldArgument: TFieldArgument;
begin
  ExpectedException := EInvalidArgument;
  lFieldArgument := TFieldArgument.Create('NOME');
  try
    FFromClause := TFromClause.Create(lFieldArgument);
  finally
    FreeAndNil(lFieldArgument);
  end;
end;

procedure TestTFromClause.TestProperties;
begin
  ExpectedException := nil;
  FFromClause := TFromClause.Create(TTableArgument.Create('CLIENTES', 'CLI'));
  CheckEquals(TTableArgument, FFromClause.Argument.ClassType);
end;

initialization

// Register any test cases with the test runner
RegisterTest(TestTFromClause.Suite);

end.
