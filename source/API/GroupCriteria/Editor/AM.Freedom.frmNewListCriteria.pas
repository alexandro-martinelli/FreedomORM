unit AM.Freedom.frmNewListCriteria;

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
  AM.Freedom.GroupCriteriaDescriptor,
  Vcl.ImgList,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls, AM.Freedom.frmNewCriteria;

type
  TfrmNewListCriteria = class(TfrmBase)
    flpnlButtons: TFlowPanel;
    btnOK: TButton;
    btnCancel: TButton;
    pnlCriteriasLIst: TPanel;
    lblCriteriasList: TLabel;
    tvCriterias: TTreeView;
    flpnlGroupCriteria: TFlowPanel;
    btnAddGroupCriteria: TButton;
    btnDelGroupCriteria: TButton;
    BtnAddCriteria: TButton;
    btnDelCriteria: TButton;
    procedure btnAddGroupCriteriaClick(Sender: TObject);
    procedure btnDelGroupCriteriaClick(Sender: TObject);
    procedure BtnAddCriteriaClick(Sender: TObject);
    procedure btnDelCriteriaClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FListGroupDescriptor: TListGroupDescriptor;
    function GetCurrentGroup: TGroupCriteriaDescriptor;
    function GetParentGroup(pGroup: TGroupCriteriaDescriptor): TGroupCriteriaDescriptor;
    function GetCurrentCriteria: TCriteriaDescriptor;
    function GetListGroupCriteria: TList<TGroupCriteriaDescriptor>;
    procedure RefreshTreeView(pFocusNodeObject: TObject);
    procedure FocusSelectedNodeObject(pFocusNodeObject: TObject);
    procedure GenerateTextCriteria;
    procedure GenerateScriptCriteria;
    procedure NewGroupCriteria;
    procedure DeleteGroupCriteria;
    procedure NewCriteria;
    procedure DeleteCriteria;
    function GetTextCriteria: TStrings;
  public
    constructor Create(AOwner: TComponent); override;
    class function NewListCriteria(pOnlyCriteria: Boolean = False): TStrings;
    property TextCriteria: TStrings read GetTextCriteria;
  end;

implementation

{$R *.dfm}

uses AM.Freedom.frmNewGroupCriteria;

{ TfrmNewCriteria }

procedure TfrmNewListCriteria.BtnAddCriteriaClick(Sender: TObject);
begin
  NewCriteria;
end;

procedure TfrmNewListCriteria.btnDelCriteriaClick(Sender: TObject);
begin
  DeleteCriteria;
end;

procedure TfrmNewListCriteria.btnAddGroupCriteriaClick(Sender: TObject);
begin
  NewGroupCriteria;
end;

procedure TfrmNewListCriteria.btnDelGroupCriteriaClick(Sender: TObject);
begin
  DeleteGroupCriteria;
end;

constructor TfrmNewListCriteria.Create(AOwner: TComponent);
begin
  inherited;
  FListGroupDescriptor := TListGroupDescriptor.Create;
  RefreshTreeView(nil);
end;

procedure TfrmNewListCriteria.DeleteCriteria;
var
  lCurrentCriteria: TCriteriaDescriptor;
begin
  lCurrentCriteria := GetCurrentCriteria;
  if (Assigned(lCurrentCriteria)) then
  begin
    lCurrentCriteria.Free;
    RefreshTreeView(nil);
  end;
end;

procedure TfrmNewListCriteria.DeleteGroupCriteria;
var
  lParentGroup, lGroup: TGroupCriteriaDescriptor;
begin
  lGroup := GetCurrentGroup;
  if (Assigned(lGroup)) then
  begin
    lParentGroup := GetParentGroup(lGroup);
    if Assigned(lParentGroup) then
    begin
      lParentGroup.Extract(lGroup);
    end
    else
    begin
      FListGroupDescriptor.Extract(lGroup);
    end;
    lGroup.Free;
    RefreshTreeView(nil);
  end;
end;

procedure TfrmNewListCriteria.FocusSelectedNodeObject(pFocusNodeObject: TObject);
var
  lIndex: Integer;
begin
  for lIndex := 0 to tvCriterias.Items.Count - 1 do
  begin
    if Assigned(tvCriterias.Items.Item[lIndex].Data) and
       (tvCriterias.Items.Item[lIndex].Data = pFocusNodeObject) then
    begin
      tvCriterias.Items.Item[lIndex].Selected := True;
      Break;
    end;
  end;
end;

procedure TfrmNewListCriteria.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if (ModalResult <> mrOK) then
  begin

  end;
end;

procedure TfrmNewListCriteria.FormDestroy(Sender: TObject);
begin
  inherited;
  FListGroupDescriptor.Free;
end;

procedure TfrmNewListCriteria.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_DELETE) then
  begin
    if (Shift = [ssCtrl]) and btnDelGroupCriteria.Visible then
    begin
      DeleteGroupCriteria;
    end
    else
    begin
      DeleteCriteria;
    end;
  end
  else if (Key = VK_INSERT) then
  begin
    if (Shift = [ssCtrl]) and btnAddGroupCriteria.Visible then
    begin
      NewGroupCriteria;
    end
    else
    begin
      NewCriteria;
    end;
  end;
end;

procedure TfrmNewListCriteria.GenerateScriptCriteria;
begin

end;

procedure TfrmNewListCriteria.GenerateTextCriteria;
begin

end;

function TfrmNewListCriteria.GetCurrentCriteria: TCriteriaDescriptor;
begin
  Result := nil;
  if (tvCriterias.Selected <> nil) and (tvCriterias.Selected.Data <> nil) and
      TObject(tvCriterias.Selected.Data).InheritsFrom(TCriteriaDescriptor) then
  begin
    Result := TCriteriaDescriptor(tvCriterias.Selected.Data);
  end;
end;

function TfrmNewListCriteria.GetCurrentGroup: TGroupCriteriaDescriptor;
begin
  Result := nil;
  if (tvCriterias.Selected <> nil) then
  begin
    if TObject(tvCriterias.Selected.Data).InheritsFrom(TGroupCriteriaDescriptor) then
    begin
      Result := TGroupCriteriaDescriptor(tvCriterias.Selected.Data);
    end
    else
    begin
      if (tvCriterias.Selected.Parent <> nil) then
      begin
        Result := TGroupCriteriaDescriptor(tvCriterias.Selected.Parent.Data);
      end;
    end;
  end;
end;

function TfrmNewListCriteria.GetListGroupCriteria: TList<TGroupCriteriaDescriptor>;
begin
  Result := FListGroupDescriptor.ListAllGroups;
end;

function TfrmNewListCriteria.GetParentGroup(pGroup: TGroupCriteriaDescriptor): TGroupCriteriaDescriptor;
var
  lNode: TTreeNode;
begin
  Result := nil;
  if (Assigned(pGroup)) then
  begin
    for lNode in tvCriterias.Items do
    begin
      if (TObject(lNode.Data).InheritsFrom(TGroupCriteriaDescriptor)) and
         (TGroupCriteriaDescriptor(lNode.Data) = pGroup) then
      begin
        if (Assigned(lNode.Parent)) then
        begin
          Result := TGroupCriteriaDescriptor(lNode.Parent.Data);
          Break;
        end;
      end;
    end;
  end;
end;

function TfrmNewListCriteria.GetTextCriteria: TStrings;
begin
  Result := FListGroupDescriptor.TextCriteria;
end;

procedure TfrmNewListCriteria.NewCriteria;
var
  lGroup: TGroupCriteriaDescriptor;
  lCriteria: TCriteriaDescriptor;
begin
  lGroup := GetCurrentGroup;
  lCriteria := TfrmNewCriteria.NewCriteria;
  if (Assigned(lCriteria)) then
  begin
    if (Assigned(lGroup)) then
    begin
      lGroup.ListCriterias.Add(lCriteria);
      RefreshTreeView(lCriteria);
    end
    else
    begin
      FListGroupDescriptor.ListCriterias.Add(lCriteria);
      RefreshTreeView(lCriteria);
    end;
  end;
end;

class function TfrmNewListCriteria.NewListCriteria(pOnlyCriteria: Boolean): TStrings;
var
  frmCriteria: TfrmNewListCriteria;
begin
  frmCriteria := TfrmNewListCriteria.Create(nil);
  Result := nil;
  try
    frmCriteria.btnAddGroupCriteria.Visible := not pOnlyCriteria;
    frmCriteria.btnDelGroupCriteria.Visible := not pOnlyCriteria;
    if frmCriteria.ShowModal = mrOk then
    begin
      Result := frmCriteria.TextCriteria;
    end;
  finally
    frmCriteria.Free;
  end;
end;

procedure TfrmNewListCriteria.RefreshTreeView(pFocusNodeObject: TObject);
begin
  tvCriterias.Items.BeginUpdate;
  try
    tvCriterias.Items.Clear;
    FListGroupDescriptor.RefreshTreeview(tvCriterias);
    tvCriterias.FullExpand;
    try
      tvCriterias.SetFocus;
    except
    end;
    if (Assigned(pFocusNodeObject)) then
    begin
      FocusSelectedNodeObject(pFocusNodeObject);
    end;
  finally
    tvCriterias.Items.EndUpdate;
    GenerateTextCriteria;
    GenerateScriptCriteria;
  end;
end;

procedure TfrmNewListCriteria.NewGroupCriteria;
var
  lCurrentGroup: TGroupCriteriaDescriptor;
  lListGroups: TList<TGroupCriteriaDescriptor>;
  lGroup: TGroupCriteriaDescriptor;
begin
  lCurrentGroup := GetCurrentGroup;
  lListGroups := GetListGroupCriteria;
  try
    lGroup := TfrmNewGroupCriteria.NewGroupCriteria(lCurrentGroup, lListGroups);
    if (Assigned(lGroup)) then
    begin
      FListGroupDescriptor.Add(lGroup);
    end;
    RefreshTreeView(lGroup);
  finally
    lListGroups.Free;
  end;
end;

end.
