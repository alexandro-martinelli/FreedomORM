unit uTestfrmNewDBObjectEditor;

interface

uses
  TestFramework,
  AM.Freedom.frmNewDBObjectEditor,
  AM.Freedom.FreedomObjectDescriptor;

type
  TestfrmNewDBObjectEditor = class(TTestCase)
  published
    procedure TestCreateNewDBUnit;
  end;

implementation

uses
  System.SysUtils;

procedure TestfrmNewDBObjectEditor.TestCreateNewDBUnit;
var
  ReturnValue: TFreedomProjectDescriptor;
begin
  ReturnValue := TfrmNewDBObjectEditor.CreateNewDBUnit;
  FreeAndNil(ReturnValue);
end;

initialization
  RegisterTest(TestfrmNewDBObjectEditor.Suite);

end.

