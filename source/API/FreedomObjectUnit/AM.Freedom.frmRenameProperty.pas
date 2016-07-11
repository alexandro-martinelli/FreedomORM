unit AM.Freedom.frmRenameProperty;

interface

uses
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
  Vcl.ImgList,
  Vcl.StdCtrls,
  AM.Freedom.FreedomObjectDescriptor;

type
  TfrmRenameProperty = class(TfrmBase)
    lblOldName: TLabel;
    lblNewName: TLabel;
    edtOldName: TEdit;
    edtNewName: TEdit;
    btnOK: TButton;
    BtnCancel: TButton;
    procedure edtNewNameChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  strict private
    FDescriptor: TFieldPropertyDescriptor;
    procedure SetDescriptor(pDescriptor: TFieldPropertyDescriptor);
  public
    class procedure Rename(pDescriptor: TFieldPropertyDescriptor);
  end;

implementation

{$R *.dfm}

uses
  System.StrUtils;

{ TfrmRemaneProperty }

procedure TfrmRenameProperty.btnOKClick(Sender: TObject);
begin
  inherited;
  edtNewName.Text := ReplaceText(edtNewName.Text, ' ', '_');
end;

procedure TfrmRenameProperty.edtNewNameChange(Sender: TObject);
begin
  inherited;
  btnOK.Enabled := (edtNewName.Text <> '') and (edtOldName.Text <> edtNewName.Text)
end;

procedure TfrmRenameProperty.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if (ModalResult = mrOK) then
  begin
    FDescriptor.PropertyMapper.Name := edtNewName.Text;
  end;
end;

class procedure TfrmRenameProperty.Rename(pDescriptor: TFieldPropertyDescriptor);
var
  lfrmRenameProperty: TfrmRenameProperty;
begin
  lfrmRenameProperty := TfrmRenameProperty.Create(nil);
  try
    lfrmRenameProperty.SetDescriptor(pDescriptor);
    lfrmRenameProperty.ShowModal;
  finally
    lfrmRenameProperty.Free;
  end;
end;

procedure TfrmRenameProperty.SetDescriptor(pDescriptor: TFieldPropertyDescriptor);
begin
  FDescriptor := pDescriptor;
  Caption := Format('Rename property "%s"', [pDescriptor.PropertyMapper.Name]);
  edtNewName.Text := pDescriptor.PropertyMapper.Name;
  edtOldName.Text := pDescriptor.PropertyMapper.Name;
end;

end.
