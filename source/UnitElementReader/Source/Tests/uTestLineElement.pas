unit uTestLineElement;

interface

uses
  TestFramework,
  System.Classes,
  UnitReader.Elements,
  System.Generics.Collections,
  UnitReader.EnumerationTypes,
  System.SysUtils;

type
  TestTLineElement = class(TTestCase)
  strict private
    FLine: TLineElement;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
    procedure TestAssign;
  end;

implementation

procedure TestTLineElement.SetUp;
begin
  FLine := TLineElement.Create;
end;

procedure TestTLineElement.TearDown;
begin
  FLine.Free;
  FLine := nil;
end;

procedure TestTLineElement.TestAssign;
var
  lSource: TLineElement;
begin
  FLine.TokenType := tkComment;
  FLine.LineNumber := 20;
  FLine.Text := '// if (lIndex > 0) then';
  lSource := TLineElement.Create;
  try
    lSource.Assign(FLine);
    CheckTrue(lSource.TokenType = tkComment);
    CheckEquals(20, lSource.LineNumber);
    CheckEqualsString('// if (lIndex > 0) then', lSource.Text);
    CheckTrue(lSource.ElementType = etLine);
  finally
    lSource.Free;
  end;
end;

procedure TestTLineElement.TestProperties;
begin
  FLine.TokenType := tkComment;
  FLine.LineNumber := 20;
  FLine.Text := '// if (lIndex > 0) then';
  CheckTrue(FLine.TokenType = tkComment);
  CheckEquals(20, FLine.LineNumber);
  CheckEqualsString('// if (lIndex > 0) then', FLine.Text);
  CheckTrue(FLine.ElementType = etLine);
end;

initialization
  RegisterTest(TestTLineElement.Suite);

end.
