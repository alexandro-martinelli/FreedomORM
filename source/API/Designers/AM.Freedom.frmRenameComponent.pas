unit AM.Freedom.frmRenameComponent;

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
  AM.Freedom.RenameParams,
  AM.Freedom.ComponentRenamer.PropertiesChange;

type
  TfrmRenameComponent = class(TfrmBase)
    lblName: TLabel;
    lblNewName: TLabel;
    edtName: TEdit;
    edtNewName: TEdit;
    chkIgnoreClassType: TCheckBox;
    lblClassType: TLabel;
    edtClassType: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    lblCaption: TLabel;
    cbxCaption: TComboBox;
    procedure btnOKClick(Sender: TObject);
    procedure edtNewNameEnter(Sender: TObject);
    procedure edtNewNameChange(Sender: TObject);
  strict private
    function FormatCaptionWithNameCase(pCaption: String): String;
  private
    FAlreadEnter: Boolean;
    FSelStart: Integer;
    FSelLength: Integer;
    FNewCaption: String;
    FSameNameCaption: Boolean;
    function ValidateEdits: Boolean;
    function GetNewName: String;
    procedure DoRename(pRemaneParams: TRenameParams);
    class procedure AddIgnoredClass(pClassName: String);
    function GetIgnoreClassType: Boolean;
    function GetNewCaption: String;
  public
    class function RenameComponent(pRemaneParams: TRenameParams): TPropertiesChange;
    property NewName: String read GetNewName;
    property NewCaption: String read GetNewCaption;
    property IgnoreClassType: Boolean read GetIgnoreClassType;
  end;

implementation

{$R *.dfm}

uses
  AM.Freedom.ComponentRenamerConfig,
  System.StrUtils;

class procedure TfrmRenameComponent.AddIgnoredClass(pClassName: String);
begin
  TComponentRenamerConfig.AddIgnoredClass(pClassName);
end;

procedure TfrmRenameComponent.btnOKClick(Sender: TObject);
begin
  inherited;
  if (ValidateEdits) then
  begin
    ModalResult := mrOK;
  end;
end;

procedure TfrmRenameComponent.DoRename(pRemaneParams: TRenameParams);
begin
  edtClassType.Text := pRemaneParams.RenameClassName;
  edtName.Text := pRemaneParams.Name;
  edtNewName.Text := pRemaneParams.Name;
  if (not StartsStr(pRemaneParams.Prefix, pRemaneParams.Name)) then
  begin
    edtNewName.Text := pRemaneParams.Prefix + pRemaneParams.Name;
  end;
  FSelStart := Length(pRemaneParams.Prefix);
  FSelLength := Length(pRemaneParams.Name);
  lblCaption.Enabled := pRemaneParams.HasCaption;
  cbxCaption.Enabled := pRemaneParams.HasCaption;
  cbxCaption.Text := pRemaneParams.Caption;
  FNewCaption := pRemaneParams.Caption;
end;

procedure TfrmRenameComponent.edtNewNameChange(Sender: TObject);
var
  lCaption: String;
begin
  inherited;
  if cbxCaption.Enabled then
  begin
    lCaption := edtNewName.Text;
    if (FSelStart > 0) then
    begin
      lCaption := Copy(lCaption, FSelStart + 1, Length(lCaption));
      lCaption := FormatCaptionWithNameCase(lCaption)
    end;
    cbxCaption.Text := lCaption;
  end;
end;

function TfrmRenameComponent.FormatCaptionWithNameCase(pCaption: String): String;
var
  lIndex: Integer;
  lStrings: TStrings;
  lStringIndex: Integer;
  lString: String;
begin
  lStrings := TStringList.Create;
  try
    lStringIndex := -1;
    for lIndex := 1 to Length(pCaption) do
    begin
      if (pCaption[lIndex] = AnsiUpperCase(pCaption[lIndex])) then
      begin
        lStringIndex := lStrings.Add(pCaption[lIndex]);
      end
      else
      begin
        if (lStringIndex = -1) then
        begin
          lStringIndex := lStrings.Add(AnsiUpperCase(pCaption[lIndex]));
        end
        else
        begin
          lStrings.Strings[lStringIndex] := lStrings.Strings[lStringIndex] + pCaption[lIndex];
        end;
      end;
    end;
    Result := '';
    for lString in lStrings do
    begin
      Result := Result + ifthen(Result <> '', ' ') + ifthen(Result <> '', AnsiLowerCase(lString), lString);
    end;
    if (Trim(Result) = '') then
    begin
      Result := pCaption;
    end;
  finally
    lStrings.Free;
  end;
end;

procedure TfrmRenameComponent.edtNewNameEnter(Sender: TObject);
begin
  inherited;
  if (not FAlreadEnter) and (FSelStart > 0) then
  begin
    edtNewName.SelStart := FSelStart;
    edtNewName.SelLength := FSelLength;
    FAlreadEnter := True;
    FSameNameCaption := edtName.Text = cbxCaption.Text;
  end;
end;

function TfrmRenameComponent.GetIgnoreClassType: Boolean;
begin
  Result := chkIgnoreClassType.Checked;
end;

function TfrmRenameComponent.GetNewCaption: String;
begin
  Result := cbxCaption.Text;
  if (ModalResult <> mrOK) then
  begin
    Result := FNewCaption;
  end;
end;

function TfrmRenameComponent.GetNewName: String;
begin
  Result := edtName.Text;
  if (ModalResult = mrOK) and (not chkIgnoreClassType.Checked) then
  begin
    Result := edtNewName.Text;
  end;
end;

class function TfrmRenameComponent.RenameComponent(pRemaneParams: TRenameParams): TPropertiesChange;
var
  lForm: TfrmRenameComponent;
begin
  lForm := TfrmRenameComponent.Create(nil);
  try
    lForm.DoRename(pRemaneParams);
    lForm.ShowModal;
    Result := TPropertiesChange.Create;
    Result.Name := lForm.NewName;
    Result.Caption := lForm.NewCaption;
    if (lForm.IgnoreClassType) then
    begin
      AddIgnoredClass(pRemaneParams.RenameClassName);
    end;
  finally
    lForm.Free;
  end;
end;

function TfrmRenameComponent.ValidateEdits: Boolean;
begin
  Result := Trim(edtName.Text) <> '';
  if (not Result) then
  begin
    ShowBalloHintForControl(edtName, 'The new name is required');
  end;
end;

end.
