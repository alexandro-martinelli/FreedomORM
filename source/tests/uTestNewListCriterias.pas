unit uTestNewListCriterias;

interface

uses
  TestFramework,
  AM.Freedom.frmBase,
  System.Variants,
  AM.Freedom.frmNewListCriteria,
  Vcl.Dialogs,
  Vcl.ImgList,
  Winapi.Windows,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.Controls,
  AM.Freedom.GroupCriteriaDescriptor,
  System.Classes,
  System.SysUtils,
  Winapi.Messages,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  TestTfrmNewListCriteria = class(TTestCase)
  published
    procedure TestNewListCriteria;
  end;

implementation


procedure TestTfrmNewListCriteria.TestNewListCriteria;
var
  ReturnValue: TStrings;
begin
  ReturnValue := TfrmNewListCriteria.NewListCriteria;
  ReturnValue.Free;

  // TODO: Validate method results
end;

initialization

// Register any test cases with the test runner
RegisterTest(TestTfrmNewListCriteria.Suite);

end.
