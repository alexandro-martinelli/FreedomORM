unit AM.Freedom.frmNewUnit;

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
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  AM.Freedom.FreedomObjectDescriptor;

type
  TfrmNewUnit = class(TfrmBase)
    splProperties: TSplitter;
    tvClasses: TTreeView;
    pnlClass: TPanel;
    FlowPanel1: TFlowPanel;
    btnCancel: TButton;
    btnFinish: TButton;
    btnOptions: TButton;
    lblClassName: TLabel;
    lblPropertyFieldNames: TLabel;
    edtUnitName: TComboBox;
    btnAddClass: TButton;
    btnExcClass: TButton;
    btnEditClass: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnAddClassClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnExcClassClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure tvClassesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure btnEditClassClick(Sender: TObject);
  private
    FFreedomUnitDescriptor: TFreedomUnitDescriptor;
    FDragNode: TTreeNode;
    function ValidateUnitName: Boolean;
    function ValidateObjectCount: Boolean;
    procedure RefreshTreeView;
  protected
    function CanAskForClose: Boolean; override;
  public
    class function CreateNewUnit: TFreedomUnitDescriptor;
    constructor Create(AOwner: TComponent); override;
    property FreedomUnitDescriptor: TFreedomUnitDescriptor read FFreedomUnitDescriptor;
    class procedure RefreshUnitTreeView(pTreeView: TTreeView; pParentNode: TTreeNode; pFreedomUnitDescriptor: TFreedomUnitDescriptor);
    class procedure DoExpand(pTreeView: TTreeView);
  end;

implementation

{$R *.dfm}

uses
  AM.Freedom.frmNewClass,
  AM.Freedom.frmOptions;

{ TfrmNewUnit }

procedure TfrmNewUnit.btnAddClassClick(Sender: TObject);
var
  lObjectDescriptor: TFreedomClassDescriptor;
begin
  if ValidateUnitName then
  begin
    FinalizeHistory;
    repeat
      lObjectDescriptor := TfrmNewClass.CreateNewClass;
      if (Assigned(lObjectDescriptor)) then
      begin
        FFreedomUnitDescriptor.Add(lObjectDescriptor)
      end;
      RefreshTreeView;
    until lObjectDescriptor = nil;
    ReloadIniFile;
  end;
end;

procedure TfrmNewUnit.btnEditClassClick(Sender: TObject);
begin
  if (tvClasses.Selected <> nil)and (tvClasses.Selected.Data <> nil) and
     (TObject(tvClasses.Selected.Data).InheritsFrom(TFreedomClassDescriptor)) then
  begin
    TfrmNewClass.EditClass(TFreedomClassDescriptor(tvClasses.Selected.Data));
    RefreshTreeView;
  end;
end;

procedure TfrmNewUnit.btnExcClassClick(Sender: TObject);
var
  lDescriptor: TFreedomClassDescriptor;
begin
  if (tvClasses.Selected <> nil) then
  begin
    if (tvClasses.Selected.Data <> nil) then
    begin
      if (TObject(tvClasses.Selected.Data).InheritsFrom(TFreedomClassDescriptor)) then
      begin
        lDescriptor := TFreedomClassDescriptor(tvClasses.Selected.Data);
        FFreedomUnitDescriptor.Extract(lDescriptor);
        lDescriptor.Free;
        RefreshTreeView;
      end;
    end;
  end;
end;

procedure TfrmNewUnit.btnOptionsClick(Sender: TObject);
begin
  inherited;
  TfrmOptions.ShowOptions;
end;

function TfrmNewUnit.CanAskForClose: Boolean;
begin
  Result := FFreedomUnitDescriptor.Count > 0;
end;

constructor TfrmNewUnit.Create(AOwner: TComponent);
begin
  inherited;
  FFreedomUnitDescriptor := TFreedomUnitDescriptor.Create;
end;

class function TfrmNewUnit.CreateNewUnit: TFreedomUnitDescriptor;
var
  lfrmNewUnit: TfrmNewUnit;
begin
  lfrmNewUnit := TfrmNewUnit.Create(nil);
  try
    lfrmNewUnit.ShowModal;
    Result := lfrmNewUnit.FreedomUnitDescriptor;
  finally
    lfrmNewUnit.Free;
  end;
end;

procedure TfrmNewUnit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if (ModalResult = mrCancel) then
  begin
    FFreedomUnitDescriptor.Free;
    FFreedomUnitDescriptor := nil;
  end;
end;

procedure TfrmNewUnit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  CanClose := (ModalResult = mrCancel) or ValidateUnitName or ValidateObjectCount;
end;

procedure TfrmNewUnit.RefreshTreeView;
begin
  tvClasses.Items.BeginUpdate;
  try
    tvClasses.Items.Clear;
    RefreshUnitTreeView(tvClasses, nil, FFreedomUnitDescriptor);
  finally
    TfrmNewClass.DoExpand(tvClasses);
    tvClasses.Items.EndUpdate;
  end;
end;

class procedure TfrmNewUnit.RefreshUnitTreeView(pTreeView: TTreeView; pParentNode: TTreeNode;
    pFreedomUnitDescriptor: TFreedomUnitDescriptor);
var
  lParentNode: TTreeNode;
  lDescriptor: TFreedomClassDescriptor;
begin
  lParentNode := pTreeView.Items.AddChild(pParentNode, pFreedomUnitDescriptor.NewUnitName);
  for lDescriptor in pFreedomUnitDescriptor do
  begin
    TfrmNewClass.RefreshClassTreeView(pTreeView, lParentNode, lDescriptor);
  end;
end;

procedure TfrmNewUnit.tvClassesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  lNode: TTreeNode;
  lDestDescriptor, lSourceDescriptor: TFreedomClassDescriptor;
begin
  inherited;
  lNode := tvClasses.GetNodeAt(X, Y);
  Accept := Assigned(lNode);
  if (Accept) then
  begin
    Accept := Assigned(lNode.Data) and TObject(lNode.Data).InheritsFrom(TFreedomClassDescriptor);
    if (Accept) then
    begin
      if (State = dsDragEnter) then
      begin
        FDragNode := lNode;
      end
      else if (State = dsDragLeave) then
      begin
        if (Assigned(FDragNode)) and (FDragNode <> lNode) then
        begin
          lDestDescriptor := TFreedomClassDescriptor(lNode.Data);
          lSourceDescriptor := TFreedomClassDescriptor(FDragNode.Data);
          FFreedomUnitDescriptor.Move(FFreedomUnitDescriptor.IndexOf(lSourceDescriptor), FFreedomUnitDescriptor.IndexOf(lDestDescriptor));
          FDragNode := nil;
          RefreshTreeView;
        end;
      end;
    end;
  end;
end;

function TfrmNewUnit.ValidateObjectCount: Boolean;
begin
  Result := True;
  if (FreedomUnitDescriptor.Count = 0) then
  begin
    ShowBalloHintForControl(btnAddClass, 'Please specify one or more classes!');
    Result := False;
  end;
end;

function TfrmNewUnit.ValidateUnitName: Boolean;
begin
  Result := True;
  if Trim(edtUnitName.Text) = '' then
  begin
    ShowBalloHintForControl(edtUnitName, 'The name of unit is required!');
    Result := False;
  end;
  FFreedomUnitDescriptor.NewUnitName := edtUnitName.Text;
end;

class procedure TfrmNewUnit.DoExpand(pTreeView: TTreeView);
begin
  TfrmNewClass.DoExpand(pTreeView);
end;


end.

