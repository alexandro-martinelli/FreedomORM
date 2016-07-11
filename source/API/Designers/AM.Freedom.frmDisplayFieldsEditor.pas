unit AM.Freedom.frmDisplayFieldsEditor;

interface

uses
  Data.DB,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  AM.Freedom.frmBase,
  Vcl.StdCtrls,
  Vcl.ImgList;

type
  TfrmDisplayFieldsEditor = class(TfrmBase)
    lblClassName: TLabel;
    lblName: TLabel;
    lblDisplayLabel: TLabel;
    lblDisplayFormat: TLabel;
    edtClassName: TEdit;
    edtName: TEdit;
    cbxDisplayLabel: TComboBox;
    cbxDisplayFormat: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
  private
    FField: TField;
    procedure AssignField(pField: TField);
  public
    class procedure EditDisplay(pField: TField);
  end;

implementation

{$R *.dfm}

{ TfrmDisplayFieldsEditor }

procedure TfrmDisplayFieldsEditor.AssignField(pField: TField);
begin
  FField := pField;
  edtClassName.Text := pField.ClassName;
  edtName.Text := pField.Name;
  cbxDisplayLabel.Text := pField.DisplayLabel;
  cbxDisplayFormat.Enabled := pField.InheritsFrom(TNumericField);
  lblDisplayFormat.Enabled := cbxDisplayFormat.Enabled;
  if (cbxDisplayFormat.Enabled) then
  begin
    cbxDisplayFormat.Text := TNumericField(pField).DisplayFormat;
  end;
end;

procedure TfrmDisplayFieldsEditor.btnOKClick(Sender: TObject);
begin
  FField.DisplayLabel := cbxDisplayLabel.Text;
  if (FField.DisplayLabel = '') then
  begin
    FField.DisplayLabel := FField.FieldName;
  end;
  if (cbxDisplayFormat.Enabled) then
  begin
    TNumericField(FField).DisplayFormat := cbxDisplayFormat.Text;
  end;
  ModalResult := mrOK;
end;

class procedure TfrmDisplayFieldsEditor.EditDisplay(pField: TField);
var
  lfrmDisplayFieldsEditor: TfrmDisplayFieldsEditor;
begin
  lfrmDisplayFieldsEditor := TfrmDisplayFieldsEditor.Create(nil);
  try
    lfrmDisplayFieldsEditor.AssignField(pField);
    lfrmDisplayFieldsEditor.ShowModal;
  finally
    lfrmDisplayFieldsEditor.Free;
  end;
end;

end.
