unit AM.Freedom.frmExtractConstant;

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
  Vcl.ImgList,
  Data.Bind.EngExt,
  Vcl.Bind.DBEngExt,
  System.Rtti,
  System.Bindings.Outputs,
  Vcl.Bind.Editors,
  Data.Bind.Components,
  AM.UnitReader.Enumerations;

type
  TExtractConstantResult = class
  private
    FConstantName: String;
    FDeclareOnClass: Boolean;
    FVisibilityScope: TVisibilityScope;
  public
    property ConstantName: String read FConstantName write FConstantName;
    property DeclareOnClass: Boolean read FDeclareOnClass write FDeclareOnClass;
    property VisibilityScope: TVisibilityScope read FVisibilityScope write FVisibilityScope;
  end;

  TfrmExtractConstant = class(TfrmBase)
    lblConstantName: TLabel;
    lblConstantValue: TLabel;
    edtConstantValue: TEdit;
    cbxConstantName: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    chkDeclareOnClass: TCheckBox;
    cbxVisibilityScope: TComboBox;
    lblVisibilityScope: TLabel;
    BindingsList1: TBindingsList;
    LinkControlToPropertyEnabled: TLinkControlToProperty;
    LinkControlToPropertyEnabled2: TLinkControlToProperty;
    procedure btnOKClick(Sender: TObject);
  private
    function GetConstantName: string;
    function ValidateEdits: Boolean;
    function GetDeclareOnClass: Boolean;
    function GetVisibilityScope: TVisibilityScope;
    procedure SetConstValue(pConstValue: String);
  public
    class function ExtractConstant(pConstValue: string): TExtractConstantResult;
    property ConstantName: string read GetConstantName;
    property VisibilityScope: TVisibilityScope read GetVisibilityScope;
    property DeclareOnClass: Boolean read GetDeclareOnClass;
  end;

implementation

{$R *.dfm}

{ TfrmExtractConstant }

procedure TfrmExtractConstant.btnOKClick(Sender: TObject);
begin
  if (ValidateEdits) then
  begin
    ModalResult := mrOK;
  end;
end;

class function TfrmExtractConstant.ExtractConstant(pConstValue: string): TExtractConstantResult;
var
  lfrmExtractConstant: TfrmExtractConstant;
begin
  lfrmExtractConstant := TfrmExtractConstant.Create(nil);
  Result := TExtractConstantResult.Create;
  try
    lfrmExtractConstant.SetConstValue(pConstValue);
    if lfrmExtractConstant.ShowModal = mrOK then
    begin
      Result.ConstantName := lfrmExtractConstant.ConstantName;
      Result.VisibilityScope := lfrmExtractConstant.VisibilityScope;
      Result.DeclareOnClass := lfrmExtractConstant.DeclareOnClass;
    end;
  finally
    lfrmExtractConstant.Free;
  end;
end;

function TfrmExtractConstant.GetConstantName: string;
begin
  Result := Trim(cbxConstantName.Text);
end;

function TfrmExtractConstant.GetDeclareOnClass: Boolean;
begin
  Result := chkDeclareOnClass.Checked;
end;

function TfrmExtractConstant.GetVisibilityScope: TVisibilityScope;
begin
  case cbxVisibilityScope.ItemIndex of
    0: Result := vsStrictPrivate;
    1: Result := vsPrivate;
    2: Result := vsStrictProtected;
    3: Result := vsProtected;
    4: Result := vsPublic;
    else
      Result := vsPublished;
  end;
end;

procedure TfrmExtractConstant.SetConstValue(pConstValue: String);
begin
  edtConstantValue.Text := pConstValue;
end;

function TfrmExtractConstant.ValidateEdits: Boolean;
begin
  Result := False;
  if (Trim(cbxConstantName.Text) = '') then
  begin
    ShowBalloHintForControl(cbxConstantName, 'The constant name is required.');
  end
  else
  begin
    Result := True;
  end;
end;

end.
