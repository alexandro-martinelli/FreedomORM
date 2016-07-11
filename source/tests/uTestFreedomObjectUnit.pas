unit uTestFreedomObjectUnit;

interface

uses
  TestFramework,
  AM.Freedom.FreedomObjectDescriptor,
  AM.Freedom.frmNewUnit;

type

  TestTNewFreedomObjectUnit = class(TTestCase)
  published
    procedure TestCreateNewUnit;
  end;

implementation

procedure TestTNewFreedomObjectUnit.TestCreateNewUnit;
var
  ReturnValue: TFreedomUnitDescriptor;
begin
  ReturnValue := TfrmNewUnit.CreateNewUnit;
  if (Assigned(ReturnValue)) then
  begin
    ReturnValue.Free;
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTNewFreedomObjectUnit.Suite);
end.

