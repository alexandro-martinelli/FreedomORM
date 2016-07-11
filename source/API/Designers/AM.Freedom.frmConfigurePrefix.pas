unit AM.Freedom.frmConfigurePrefix;

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
  Vcl.StdCtrls,
  Vcl.ImgList;

type
  TfrmConfigurePrefix = class(TfrmBase)
    lblClassName: TLabel;
    cbxClassName: TComboBox;
    Label2: TLabel;
    edtPrefixName: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
  private
    function ValidateEdits: Boolean;
    function GetPrefixName: string;
    procedure DoConfigure(pClass: TClass);
    function GetSelectedClassName: string;
  public
    class function ConfigurePrefix(pClass: TClass): string;
    property PrefixName: string read GetPrefixName;
    property SelectedClassName: string read GetSelectedClassName;
  end;

implementation

{$R *.dfm}

uses
  AM.Freedom.ComponentRenamerConfig;

procedure TfrmConfigurePrefix.btnOKClick(Sender: TObject);
begin
  if (ValidateEdits) then
  begin
    TComponentRenamerConfig.AddClassPrefix(cbxClassName.Text, edtPrefixName.Text);
    ModalResult := mrOK;
  end;
end;

class function TfrmConfigurePrefix.ConfigurePrefix(pClass: TClass): string;
var
  lfrmConfigurePrefix: TfrmConfigurePrefix;
begin
  lfrmConfigurePrefix := TfrmConfigurePrefix.Create(nil);
  try
    lfrmConfigurePrefix.DoConfigure(pClass);
    Result := '';
    if (lfrmConfigurePrefix.ShowModal = mrOK) then
    begin
      Result := lfrmConfigurePrefix.PrefixName;
    end;
    if (Result = '') then
    begin
      if (Application.MessageBox(PWideChar('you would like to add this class to the ignore list classes?'), PWideChar(''), MB_ICONQUESTION + MB_YESNO + MB_DEFBUTTON1) = ID_YES) then
      begin
        TComponentRenamerConfig.AddIgnoredClass(lfrmConfigurePrefix.SelectedClassName);
      end;
    end;
  finally
    lfrmConfigurePrefix.Free;
  end;
end;

procedure TfrmConfigurePrefix.DoConfigure(pClass: TClass);
var
  lClass: TClass;
begin
  lClass := pClass;
  cbxClassName.Clear;
  while Assigned(lClass) do
  begin
    cbxClassName.Items.Add(lClass.ClassName);
    lClass := lClass.ClassParent;
    if (lClass.ClassName = 'TComponent') then
    begin
      Break;
    end;
  end;
  cbxClassName.ItemIndex := 0;
end;

function TfrmConfigurePrefix.GetPrefixName: string;
begin
  Result := edtPrefixName.Text;
end;

function TfrmConfigurePrefix.GetSelectedClassName: string;
begin
  Result := cbxClassName.Text;
end;

function TfrmConfigurePrefix.ValidateEdits: Boolean;
begin
  Result := Trim(edtPrefixName.Text) <> '';
  if (not Result) then
  begin
    ShowBalloHintForControl(edtPrefixName, 'The prefix name is required');
  end;
end;

end.
