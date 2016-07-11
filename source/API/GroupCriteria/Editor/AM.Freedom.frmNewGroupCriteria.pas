unit AM.Freedom.frmNewGroupCriteria;

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
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Helper.Policy,
  AM.Freedom.GroupCriteria,
  Vcl.StdCtrls,
  Vcl.Samples.Spin,
  Vcl.ImgList,
  Vcl.ExtCtrls,
  AM.Freedom.GroupCriteriaDescriptor;

type
  TfrmNewGroupCriteria = class(TfrmBase)
    cbxPolicy: TComboBox;
    spedtLimitRows: TSpinEdit;
    FlowPanel1: TFlowPanel;
    btnCancel: TButton;
    btnOK: TButton;
    cbxVarNames: TComboBox;
    cbxGroupVarNames: TComboBox;
    chxIntoExisting: TCheckBox;
    lblPolicy: TLabel;
    lblLimitRows: TLabel;
    lblVarName: TLabel;
    lblExistingVarNames: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure chxIntoExistingClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FExistingGroupList: TList<TGroupCriteriaDescriptor>;
    FInto: TGroupCriteriaDescriptor;
    procedure SetExistingGroupList(const Value: TList<TGroupCriteriaDescriptor>);
    procedure SetInto(const Value: TGroupCriteriaDescriptor);
    function GetNewGroup: TGroupCriteriaDescriptor;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    class function NewGroupCriteria(pInto: TGroupCriteriaDescriptor; pExistingGroupCriterias: TList<TGroupCriteriaDescriptor>): TGroupCriteriaDescriptor;
    property Into: TGroupCriteriaDescriptor read FInto write SetInto;
    property ExistingGroupList: TList<TGroupCriteriaDescriptor> read FExistingGroupList write SetExistingGroupList;
    property NewGroup: TGroupCriteriaDescriptor read GetNewGroup;
  end;

implementation

{$R *.dfm}

uses
  System.StrUtils;

{ TfrmNewGroupCriteria }

procedure TfrmNewGroupCriteria.btnOKClick(Sender: TObject);
begin
  inherited;
  if (cbxVarNames.Text = '') then
  begin
    ShowBalloHintForControl(cbxVarNames, 'The var name is necessary!');
  end
  else
  begin
    ModalResult := mrOk;
  end;
end;

procedure TfrmNewGroupCriteria.chxIntoExistingClick(Sender: TObject);
begin
  inherited;
  lblExistingVarNames.Enabled := chxIntoExisting.Checked;
  cbxGroupVarNames.Enabled := lblExistingVarNames.Enabled;
end;

constructor TfrmNewGroupCriteria.Create(AOwner: TComponent);
var
  lIndex: Integer;
begin
  inherited;
  cbxPolicy.Items.Clear;
  for lIndex := Ord(Low(TPolicy)) to Ord(High(TPolicy)) do
  begin
    cbxPolicy.Items.Add(TPolicy(lIndex).ToString.Trim);
  end;
  cbxPolicy.ItemIndex := 0;
end;

procedure TfrmNewGroupCriteria.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if (ModalResult = mrOk) then
  begin
    if (Assigned(FExistingGroupList)) and chxIntoExisting.Checked then
    begin
      FExistingGroupList.Items[cbxGroupVarNames.ItemIndex].AddGroupCriteria(cbxVarNames.Text, cbxGroupVarNames.Text, TPolicy(cbxPolicy.ItemIndex), spedtLimitRows.Value);
    end;
  end;
end;

function TfrmNewGroupCriteria.GetNewGroup: TGroupCriteriaDescriptor;
var
  lExisting: String;
begin
  lExisting := ifthen(chxIntoExisting.Checked, cbxGroupVarNames.Text);
  Result := TGroupCriteriaDescriptor.Create(cbxVarNames.Text, lExisting, TPolicy(cbxPolicy.ItemIndex), spedtLimitRows.Value);
end;

class function TfrmNewGroupCriteria.NewGroupCriteria(pInto: TGroupCriteriaDescriptor;
  pExistingGroupCriterias: TList<TGroupCriteriaDescriptor>): TGroupCriteriaDescriptor;
var
  frmNewGroupCriteria: TfrmNewGroupCriteria;
begin
  frmNewGroupCriteria := TfrmNewGroupCriteria.Create(nil);
  try
    frmNewGroupCriteria.Into := pInto;
    frmNewGroupCriteria.ExistingGroupList := pExistingGroupCriterias;
    Result := nil;
    if (frmNewGroupCriteria.ShowModal = mrOk) then
    begin
      if (not frmNewGroupCriteria.chxIntoExisting.Checked) then
      begin
        Result := frmNewGroupCriteria.NewGroup;
      end;
    end;
  finally
    frmNewGroupCriteria.Free;
  end;
end;

procedure TfrmNewGroupCriteria.SetExistingGroupList(const Value: TList<TGroupCriteriaDescriptor>);
var
  lGroupDescriptor: TGroupCriteriaDescriptor;
begin
  FExistingGroupList := Value;
  if Assigned(FExistingGroupList) then
  begin
    cbxGroupVarNames.Items.Clear;
    for lGroupDescriptor in FExistingGroupList do
    begin
      cbxGroupVarNames.Items.Add(lGroupDescriptor.VarName);
    end;
    if Assigned(FInto) then
    begin
      if (cbxGroupVarNames.Items.IndexOf(FInto.VarName) < 0) then
      begin
        cbxGroupVarNames.Items.Add(FInto.VarName);
      end;
      cbxGroupVarNames.ItemIndex := cbxGroupVarNames.Items.IndexOf(FInto.VarName);
    end;
  end;
end;

procedure TfrmNewGroupCriteria.SetInto(const Value: TGroupCriteriaDescriptor);
begin
  FInto := Value;
  chxIntoExisting.Checked := Assigned(FInto);
end;

end.
