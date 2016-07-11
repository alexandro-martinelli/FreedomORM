unit AM.Freedom.frmOptions;

interface

uses
  {$IFNDEF ORMTEST}
  ToolsAPI,
  {$IFEND}
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  System.IniFiles,
  AM.Freedom.frmBase,
  Vcl.ImgList;

type
  TfrmOptions = class(TfrmBase)
    lblMappingCharCase: TLabel;
    lblSchemaCharCase: TLabel;
    cbxMappingCharCase: TComboBox;
    cbxSchemaCharCase: TComboBox;
    FlowPanel1: TFlowPanel;
    btnOK: TButton;
    btnCancel: TButton;
    lblBaseClassName: TLabel;
    edtBaseClassName: TEdit;
    lblListBaseClassName: TLabel;
    edtListBaseClassName: TEdit;
    Label1: TLabel;
    edtClassPrefix: TEdit;
  public
    class procedure ShowOptions;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TfrmObjectUnitOptions }

constructor TfrmOptions.Create(AOwner: TComponent);
begin
  inherited;
  cbxMappingCharCase.ItemIndex := IniFile.ReadInteger('Options', 'MappingCharCase', 1);
  cbxSchemaCharCase.ItemIndex := IniFile.ReadInteger('Options', 'SchemaCharCase', 2);
  edtBaseClassName.Text := IniFile.ReadString('Options', 'DefaultBaseClassName', 'TFreedomObject');
  edtListBaseClassName.Text := IniFile.ReadString('Options', 'DefaultListBaseClassName', 'TFreedomObjectList');
  edtClassPrefix.Text := IniFile.ReadString('Options', 'ClassPrefix', '');
end;

destructor TfrmOptions.Destroy;
begin
  if (ModalResult = mrOk) then
  begin
    IniFile.WriteInteger('Options', 'MappingCharCase', cbxMappingCharCase.ItemIndex);
    IniFile.WriteInteger('Options', 'SchemaCharCase', cbxSchemaCharCase.ItemIndex);
    IniFile.WriteString('Options', 'DefaultBaseClassName', edtBaseClassName.Text);
    IniFile.WriteString('Options', 'DefaultListBaseClassName', edtListBaseClassName.Text);
    IniFile.WriteString('Options', 'ClassPrefix', edtClassPrefix.Text);
  end;
  inherited;
end;

class procedure TfrmOptions.ShowOptions;
var
  frmObjectUnitOptions: TfrmOptions;
begin
  frmObjectUnitOptions := TfrmOptions.Create(nil);
  try
    frmObjectUnitOptions.ShowModal;
  finally
    frmObjectUnitOptions.Free;
  end;
end;

end.
