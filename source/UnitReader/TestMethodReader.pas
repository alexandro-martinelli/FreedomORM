unit TestMethodReader;

interface

uses
  TestFramework,
  AM.UnitReader.Readers.CustomReader,
  AM.UnitReader.Members.MethodMember,
  AM.UnitReader.Enumerations,
  AM.UnitReader.Readers.MethodReader,
  AM.UnitReader.Members.CustomMember,
  AM.UnitReader.Members.ArgumentMember,
  AM.UnitReader.Helper.MethodType;

type
  TestTMethodReader = class(TTestCase)
  strict private
    FMethodMember: TMethodMember;
  protected
    procedure TearDown; override;
  published
    procedure TestProcedureWithoutParameters;
    procedure TestProcedureWithOneParameter;
    procedure TestProcedureWithTwoParameters;
    procedure TestProcedureWithTreeParameters;

    procedure TestClassProcedureWithoutParameters;
    procedure TestClassProcedureWithOneParameter;
    procedure TestClassProcedureWithTwoParameters;
    procedure TestClassProcedureWithTreeParameters;

    procedure TestFunctionWithTreeParameters;
    procedure TestClassFunctionWithTreeParameters;

    procedure TestConstructorWithTreeParameters;
    procedure TestClassConstructorWithTreeParameters;

    procedure TestDestructor;
    procedure TestClassDestructor;
  end;

implementation

procedure TestTMethodReader.TearDown;
begin
  inherited;
  FMethodMember.Free;
end;

procedure TestTMethodReader.TestClassConstructorWithTreeParameters;
var
  lMethodStr: string;
begin
  lMethodStr := 'class constructor Create(pStr, pSecondStr: String; pPosIni: Integer);';
  FMethodMember := TMethodReader.MethodFromStr(lMethodStr, vsPublished);
  CheckEqualsString('Create', FMethodMember.Name);
  CheckEquals(3, FMethodMember.Arguments.Count);
  CheckEqualsString('pStr', FMethodMember.Arguments.Items[0].Name);
  CheckEqualsString('pSecondStr', FMethodMember.Arguments.Items[1].Name);
  CheckEqualsString('pPosIni', FMethodMember.Arguments.Items[2].Name);
  CheckTrue(FMethodMember.IsClassMethod);
  CheckTrue(mtConstructor = FMethodMember.MethodType);
end;

procedure TestTMethodReader.TestClassDestructor;
var
  lMethodStr: string;
begin
  lMethodStr := 'class destructor Destroy;';
  FMethodMember := TMethodReader.MethodFromStr(lMethodStr, vsPublished);
  CheckEqualsString('Destroy', FMethodMember.Name);
  CheckEquals(0, FMethodMember.Arguments.Count);
  CheckTrue(FMethodMember.IsClassMethod);
  CheckTrue(mtDestructor = FMethodMember.MethodType);
end;

procedure TestTMethodReader.TestClassFunctionWithTreeParameters;
var
  lMethodStr: string;
begin
  lMethodStr := 'class function DoRead(pStr, pSecondStr: String; pPosIni: Integer): String;';
  FMethodMember := TMethodReader.MethodFromStr(lMethodStr, vsPublished);
  CheckEqualsString('DoRead', FMethodMember.Name);
  CheckEquals(3, FMethodMember.Arguments.Count);
  CheckEqualsString('pStr', FMethodMember.Arguments.Items[0].Name);
  CheckEqualsString('pSecondStr', FMethodMember.Arguments.Items[1].Name);

  CheckEqualsString('pPosIni', FMethodMember.Arguments.Items[2].Name);
  CheckTrue(FMethodMember.IsClassMethod);
  CheckTrue(mtFunction = FMethodMember.MethodType);

end;

procedure TestTMethodReader.TestClassProcedureWithOneParameter;
var
  lMethodStr: string;
begin
  lMethodStr := 'class procedure DoRead(pStr: String);';
  FMethodMember := TMethodReader.MethodFromStr(lMethodStr, vsPublished);
  CheckEqualsString('DoRead', FMethodMember.Name);
  CheckEquals(1, FMethodMember.Arguments.Count);
  CheckEqualsString('pStr', FMethodMember.Arguments.Items[0].Name);
  CheckTrue(FMethodMember.IsClassMethod);
  CheckTrue(mtProcedure = FMethodMember.MethodType);
end;

procedure TestTMethodReader.TestClassProcedureWithoutParameters;
var
  lMethodStr: string;
begin
  lMethodStr := 'class procedure DoRead(pStr: String);';
  FMethodMember := TMethodReader.MethodFromStr(lMethodStr, vsPublished);
  CheckEqualsString('DoRead', FMethodMember.Name);
  CheckEquals(1, FMethodMember.Arguments.Count);
  CheckEqualsString('pStr', FMethodMember.Arguments.Items[0].Name);
  CheckTrue(FMethodMember.IsClassMethod);
  CheckTrue(mtProcedure = FMethodMember.MethodType);
end;

procedure TestTMethodReader.TestClassProcedureWithTreeParameters;
var
  lMethodStr: string;
begin
  lMethodStr := 'class procedure DoRead(pStr, pSecondStr: String; pPosIni: Integer);';
  FMethodMember := TMethodReader.MethodFromStr(lMethodStr, vsPublished);
  CheckEqualsString('DoRead', FMethodMember.Name);
  CheckEquals(3, FMethodMember.Arguments.Count);
  CheckEqualsString('pStr', FMethodMember.Arguments.Items[0].Name);
  CheckEqualsString('pSecondStr', FMethodMember.Arguments.Items[1].Name);

  CheckEqualsString('pPosIni', FMethodMember.Arguments.Items[2].Name);
  CheckTrue(FMethodMember.IsClassMethod);
  CheckTrue(mtProcedure = FMethodMember.MethodType);
end;

procedure TestTMethodReader.TestClassProcedureWithTwoParameters;
var
  lMethodStr: string;
begin
  lMethodStr := 'class procedure DoRead(pStr: String; pPosIni: Integer);';
  FMethodMember := TMethodReader.MethodFromStr(lMethodStr, vsPublished);
  CheckEqualsString('DoRead', FMethodMember.Name);
  CheckEquals(2, FMethodMember.Arguments.Count);
  CheckEqualsString('pStr', FMethodMember.Arguments.Items[0].Name);
  CheckEqualsString('pPosIni', FMethodMember.Arguments.Items[1].Name);
  CheckTrue(FMethodMember.IsClassMethod);
  CheckTrue(mtProcedure = FMethodMember.MethodType);
end;

procedure TestTMethodReader.TestConstructorWithTreeParameters;
var
  lMethodStr: string;
begin
  lMethodStr := 'constructor Create(pStr, pSecondStr: String; pPosIni: Integer);';
  FMethodMember := TMethodReader.MethodFromStr(lMethodStr, vsPublished);
  CheckEqualsString('Create', FMethodMember.Name);
  CheckEquals(3, FMethodMember.Arguments.Count);
  CheckEqualsString('pStr', FMethodMember.Arguments.Items[0].Name);
  CheckEqualsString('pSecondStr', FMethodMember.Arguments.Items[1].Name);
  CheckEqualsString('pPosIni', FMethodMember.Arguments.Items[2].Name);
  CheckFalse(FMethodMember.IsClassMethod);
  CheckTrue(mtConstructor = FMethodMember.MethodType);
end;

procedure TestTMethodReader.TestDestructor;
var
  lMethodStr: string;
begin
  lMethodStr := 'destructor Destroy;';
  FMethodMember := TMethodReader.MethodFromStr(lMethodStr, vsPublished);
  CheckEqualsString('Destroy', FMethodMember.Name);
  CheckEquals(0, FMethodMember.Arguments.Count);
  CheckFalse(FMethodMember.IsClassMethod);
  CheckTrue(mtDestructor = FMethodMember.MethodType);
end;

procedure TestTMethodReader.TestFunctionWithTreeParameters;
var
  lMethodStr: string;
begin
  lMethodStr := 'function DoRead(pStr, pSecondStr: String; pPosIni: Integer): String;';
  FMethodMember := TMethodReader.MethodFromStr(lMethodStr, vsPublished);
  CheckEqualsString('DoRead', FMethodMember.Name);
  CheckEquals(3, FMethodMember.Arguments.Count);
  CheckEqualsString('pStr', FMethodMember.Arguments.Items[0].Name);
  CheckEqualsString('pSecondStr', FMethodMember.Arguments.Items[1].Name);
  CheckEqualsString('pPosIni', FMethodMember.Arguments.Items[2].Name);
  CheckFalse(FMethodMember.IsClassMethod);
  CheckTrue(mtFunction = FMethodMember.MethodType);
end;

procedure TestTMethodReader.TestProcedureWithOneParameter;
var
  lMethodStr: string;
begin
  lMethodStr := 'procedure DoRead(pStr: String);';
  FMethodMember := TMethodReader.MethodFromStr(lMethodStr, vsPublished);
  CheckEqualsString('DoRead', FMethodMember.Name);
  CheckEquals(1, FMethodMember.Arguments.Count);
  CheckEqualsString('pStr', FMethodMember.Arguments.Items[0].Name);
  CheckFalse(FMethodMember.IsClassMethod);
  CheckTrue(mtProcedure = FMethodMember.MethodType);
end;

procedure TestTMethodReader.TestProcedureWithoutParameters;
var
  lMethodStr: string;
begin
  lMethodStr := 'procedure DoRead;';
  FMethodMember := TMethodReader.MethodFromStr(lMethodStr, vsPublished);
  CheckEqualsString('DoRead', FMethodMember.Name);
  CheckEquals(0, FMethodMember.Arguments.Count);
  CheckFalse(FMethodMember.IsClassMethod);
  CheckTrue(mtProcedure = FMethodMember.MethodType);
end;

procedure TestTMethodReader.TestProcedureWithTreeParameters;
var
  lMethodStr: string;
begin
  lMethodStr := 'procedure DoRead(pStr, pSecondStr: String; pPosIni: Integer);';
  FMethodMember := TMethodReader.MethodFromStr(lMethodStr, vsPublished);
  CheckEqualsString('DoRead', FMethodMember.Name);
  CheckEquals(3, FMethodMember.Arguments.Count);
  CheckEqualsString('pStr', FMethodMember.Arguments.Items[0].Name);
  CheckEqualsString('pSecondStr', FMethodMember.Arguments.Items[1].Name);
  CheckEqualsString('pPosIni', FMethodMember.Arguments.Items[2].Name);
  CheckFalse(FMethodMember.IsClassMethod);
  CheckTrue(mtProcedure = FMethodMember.MethodType);
end;

procedure TestTMethodReader.TestProcedureWithTwoParameters;
var
  lMethodStr: string;
begin
  lMethodStr := 'procedure DoRead(pStr: String; pPosIni: Integer);';
  FMethodMember := TMethodReader.MethodFromStr(lMethodStr, vsPublished);
  CheckEqualsString('DoRead', FMethodMember.Name);
  CheckEquals(2, FMethodMember.Arguments.Count);
  CheckEqualsString('pStr', FMethodMember.Arguments.Items[0].Name);
  CheckEqualsString('pPosIni', FMethodMember.Arguments.Items[1].Name);
  CheckFalse(FMethodMember.IsClassMethod);
  CheckTrue(mtProcedure = FMethodMember.MethodType);
end;

initialization

// Register any test cases with the test runner
RegisterTest(TestTMethodReader.Suite);

end.
