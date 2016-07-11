unit TestStrUtilsClass;

interface

uses
  TestFramework, AM.UnitReader.Utils.StrUtils;

type
  TestTStrUtils = class(TTestCase)
  published
    procedure TestContainsText;
    procedure TestReplaceText;
    procedure TestRemoveSpaces;
    procedure TestRemoveLineBreaks;
  end;

implementation

procedure TestTStrUtils.TestContainsText;
var
  ReturnValue: Boolean;
  lSubText: string;
  lText: string;
begin
  lText := 'FConstructorType: TConstructorType';
  lSubText := 'const';
  ReturnValue := TStrUtils.ContainsText(lText, lSubText);
  CheckFalse(ReturnValue);

  lSubText := 'constructor';
  ReturnValue := TStrUtils.ContainsText(lText, lSubText);
  CheckFalse(ReturnValue);

  lSubText := 'type';
  ReturnValue := TStrUtils.ContainsText(lText, lSubText);
  CheckFalse(ReturnValue);

  lText := 'strict private const strict protected const';
  lSubText := 'const';
  ReturnValue := TStrUtils.ContainsText(lText, lSubText);
  CheckTrue(ReturnValue);
end;

procedure TestTStrUtils.TestReplaceText;
var
  lToText, lFromText, lText, lReturnValue: string;
begin
  lToText := sLineBreak + 'const' + sLineBreak;
  lFromText := 'const';
  lText := 'FConstructorType: TConstructorType';
  lReturnValue := TStrUtils.ReplaceText(lText, lFromText, lToText);
  CheckEqualsString(lText, lReturnValue);

  lText := 'Strict private const' + sLineBreak + 'cDel = ''Nothing''' + sLineBreak + 'strict protected const';
  lReturnValue := TStrUtils.ReplaceText(lText, lFromText, lToText);
  CheckNotEqualsString(lText, lReturnValue);
end;

procedure TestTStrUtils.TestRemoveSpaces;
var
  lReturnValue: string;
  lString: string;
begin
  lString := '  strict private  ';
  lReturnValue := TStrUtils.RemoveSpaces(lString);
  CheckEqualsString('strict private', lReturnValue);

  lString := '  ';
  lReturnValue := TStrUtils.RemoveSpaces(lString);
  CheckEqualsString('', lReturnValue);
end;

procedure TestTStrUtils.TestRemoveLineBreaks;
var
  lReturnValue: string;
  lString: string;
begin
  lString := SLineBreak + sLineBreak + 'strict';
  lReturnValue := TStrUtils.RemoveLineBreaks(lString);
  CheckEqualsString(sLIneBreak + 'strict', lReturnValue);

  lString := sLineBreak;
  lReturnValue := TStrUtils.RemoveSpaces(lString);
  CheckEqualsString(sLineBreak, lReturnValue);

  lString := sLineBreak + sLineBreak;
  lReturnValue := TStrUtils.RemoveLineBreaks(lString);
  CheckEqualsString(sLineBreak, lReturnValue);

  lString := sLineBreak + sLineBreak + sLineBreak;
  lReturnValue := TStrUtils.RemoveLineBreaks(lString);
  CheckEqualsString(sLineBreak, lReturnValue);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTStrUtils.Suite);
end.

