unit AM.Freedom.frmReconfigurePrefix;

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
  TfrmReconfigurePrefix = class(TfrmBase)
    btnOK: TButton;
    btnCancel: TButton;
    lblClassName: TLabel;
    lblPrefix: TLabel;
    edtClassName: TEdit;
    edtPrefix: TEdit;
    chkIgnoreClassType: TCheckBox;
    procedure btnOKClick(Sender: TObject);
  private
    function GetClassNameRename: string;
    procedure SetClassNameRename(const Value: string);
    function GetPrefixName: string;
    procedure SetPrefixName(const Value: string);
    function GetIgnoreClassType: Boolean;
    procedure SetIgnoreClassType(const Value: Boolean);
    function ValidadeEdits: Boolean;
  public
    property ClassNameRename: string read GetClassNameRename write SetClassNameRename;
    property PrefixName: string read GetPrefixName write SetPrefixName;
    property IgnoreClassType: Boolean read GetIgnoreClassType write SetIgnoreClassType;
  end;

implementation

{$R *.dfm}

procedure TfrmReconfigurePrefix.btnOKClick(Sender: TObject);
begin
  inherited;
  if (ValidadeEdits) then
  begin
    ModalResult := mrOK;
  end;
end;

function TfrmReconfigurePrefix.GetClassNameRename: string;
begin
  Result := edtClassName.Text;
end;

function TfrmReconfigurePrefix.GetIgnoreClassType: Boolean;
begin
  Result := chkIgnoreClassType.Checked;
end;

function TfrmReconfigurePrefix.GetPrefixName: string;
begin
  Result := edtPrefix.Text;
end;

procedure TfrmReconfigurePrefix.SetClassNameRename(const Value: string);
begin
  edtClassName.Text := Value;
end;

procedure TfrmReconfigurePrefix.SetIgnoreClassType(const Value: Boolean);
begin
  chkIgnoreClassType.Checked := Value;
end;

procedure TfrmReconfigurePrefix.SetPrefixName(const Value: string);
begin
  edtPrefix.Text := Value;
end;

function TfrmReconfigurePrefix.ValidadeEdits: Boolean;
begin
  Result := False;
  if (Trim(edtClassName.Text) = '') then
  begin
    ShowBalloHintForControl(edtClassName, 'The class name is required');
  end
  else if (Trim(edtPrefix.Text) = '') then
  begin
    ShowBalloHintForControl(edtPrefix, 'The prefix name is required');
  end
  else
  begin
    Result := True;
  end;
end;

end.
