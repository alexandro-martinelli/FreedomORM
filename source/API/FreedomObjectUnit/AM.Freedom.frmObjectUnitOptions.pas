unit AM.Freedom.frmObjectUnitOptions;

interface

uses
  {$IFNDEF TEST}
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
  AM.Freedom.frmBaseObjectUnit;

type
  TfrmObjectUnitOptions = class(TfrmBaseObjectUnit)
    lblMappingCharCase: TLabel;
    lblSchemaCharCase: TLabel;
    cbxMappingCharCase: TComboBox;
    cbxSchemaCharCase: TComboBox;
    FlowPanel1: TFlowPanel;
    btnOK: TButton;
    btnCancel: TButton;
  public
    class procedure ShowOptions;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TfrmObjectUnitOptions }

constructor TfrmObjectUnitOptions.Create(AOwner: TComponent);
begin
  inherited;
  cbxMappingCharCase.ItemIndex := IniFile.ReadInteger('Options', 'MappingCharCase', 1);
  cbxSchemaCharCase.ItemIndex := IniFile.ReadInteger('Options', 'SchemaCharCase', 2);
end;

destructor TfrmObjectUnitOptions.Destroy;
begin
  if (ModalResult = mrOk) then
  begin
    IniFile.ReadInteger('Options', 'MappingCharCase', cbxMappingCharCase.ItemIndex);
    IniFile.ReadInteger('Options', 'SchemaCharCase', cbxSchemaCharCase.ItemIndex);
  end;
  inherited;
end;

class procedure TfrmObjectUnitOptions.ShowOptions;
var
  frmObjectUnitOptions: TfrmObjectUnitOptions;
begin
  frmObjectUnitOptions := TfrmObjectUnitOptions.Create(nil);
  try
    frmObjectUnitOptions.ShowModal;
  finally
    frmObjectUnitOptions.Free;
  end;
end;

end.
