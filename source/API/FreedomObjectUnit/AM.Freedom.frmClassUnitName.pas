unit AM.Freedom.frmClassUnitName;

interface

uses
  System.Generics.Collections,
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
  Vcl.ExtCtrls;

type
  TfrmClassUnitName = class(TfrmBase)
    lblUnitName: TLabel;
    lblExplain: TLabel;
    cbxClassUnitName: TComboBox;
    lblExplain2: TLabel;
    FlowPanel1: TFlowPanel;
    btnFinish: TButton;
    btnCancel: TButton;
    procedure btnFinishClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  strict private
    class var FHistoryClassUnitNames: TDictionary<String, String>;
    class var FOwnerClass: TClass;
  private
    procedure SetClassName(pClassName: String);
    function GetClassUnitName: String;
  protected
    procedure DoInitializeHistory; override;
  public
    class function GetUnitNameForClassName(pClassName: String): String;
    class procedure InitializeClassHistory(pOwnerClass: TClass);
    class procedure FinalizeClassHistory(pOwnerClass: TClass);
    property ClassUnitName: String read GetClassUnitName;
  end;

implementation

{$R *.dfm}

{ TfrmClassUnitName }

procedure TfrmClassUnitName.btnFinishClick(Sender: TObject);
begin
  if (cbxClassUnitName.Text <> '') and (Length(cbxClassUnitName.Text) < 3) then
  begin
    ShowBalloHintForControl(cbxClassUnitName, 'The name of the unit must contain at least 3 characters!');
  end
  else
  begin
    ModalResult := mrOK;
  end;
end;

procedure TfrmClassUnitName.DoInitializeHistory;
var
  lUnitNames: TStrings;
  lIndex: Integer;
begin
  inherited;
  lUnitNames := TStringList.Create;
  try
    IniFile.ReadSectionValues('ClassToUnitName', lUnitNames);
    cbxClassUnitName.Items.Clear;
    for lIndex := 0 to lUnitNames.Count - 1 do
    begin
      if (lUnitNames.ValueFromIndex[lIndex] <> '') and (cbxClassUnitName.Items.IndexOf(lUnitNames.ValueFromIndex[lIndex]) < 0) then
      begin
        cbxClassUnitName.Items.Add(lUnitNames.ValueFromIndex[lIndex]);
      end;
    end;
  finally
    lUnitNames.Free;
  end;
end;

class procedure TfrmClassUnitName.FinalizeClassHistory(pOwnerClass: TClass);
begin
  if (FOwnerClass <> nil) and (FOwnerClass = pOwnerClass) then
  begin
    FHistoryClassUnitNames.Free;
    FOwnerClass := nil;
  end;
end;

procedure TfrmClassUnitName.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if (ModalResult <> mrOK) then
  begin
    cbxClassUnitName.Text := '';
  end;
end;

function TfrmClassUnitName.GetClassUnitName: String;
begin
  Result := cbxClassUnitName.Text;
end;

class function TfrmClassUnitName.GetUnitNameForClassName(pClassName: String): String;
var
  lfrmClassUnitName: TfrmClassUnitName;
begin
  if FHistoryClassUnitNames.ContainsKey(pClassName) then
  begin
    Result := FHistoryClassUnitNames.Items[pClassName];
  end
  else
  begin
    lfrmClassUnitName := TfrmClassUnitName.Create(nil);
    Result := '';
    try
      lfrmClassUnitName.SetClassName(pClassName);
      if lfrmClassUnitName.ShowModal = mrOK then
      begin
        Result := lfrmClassUnitName.ClassUnitName;
        FHistoryClassUnitNames.AddOrSetValue(pClassName, Result);
      end;
    finally
      FreeAndNil(lfrmClassUnitName);
    end;
  end;
end;

class procedure TfrmClassUnitName.InitializeClassHistory(pOwnerClass: TClass);
begin
  if (FOwnerClass = nil) then
  begin
    FHistoryClassUnitNames := TDictionary<String, String>.Create;
    FOwnerClass := pOwnerClass;
  end;
end;

procedure TfrmClassUnitName.SetClassName(pClassName: String);
begin
  lblExplain.Caption := Format('Type the name of then unit for class "%s".', [pClassName]);
end;

end.
