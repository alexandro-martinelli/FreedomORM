unit uTestFreedomObjectDataSet;

interface

uses
  TestFramework,
  ufrmTestFreedomObjectDataSet;

type
  TestTfrmTestFreedomObjectDataSet = class(TTestCase)
  published
    procedure TestDoTest;
  end;

implementation


procedure TestTfrmTestFreedomObjectDataSet.TestDoTest;
begin
  TfrmTestFreedomObjectDataSet.DoTest;
end;

initialization
  RegisterTest(TestTfrmTestFreedomObjectDataSet.Suite);

end.
