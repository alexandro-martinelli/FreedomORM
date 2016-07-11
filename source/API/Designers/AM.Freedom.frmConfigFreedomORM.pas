unit AM.Freedom.frmConfigFreedomORM;

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
  Vcl.ComCtrls;

type
  TfrmConfigFreedomORM = class(TfrmBase)
    pgcConfig: TPageControl;
    tsRename: TTabSheet;
    chkActiveRename: TCheckBox;
    lvRenameClassConfig: TListView;
    btnRemove: TButton;
    btnAddClass: TButton;
    btnLoadFromIniFile: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    lblCheckedHint: TLabel;
    btnSaveToIniFile: TButton;
    btnEditClass: TButton;
    tsShortcuts: TTabSheet;
    lblNewFreedomObject: TLabel;
    lblNewFreedomDBObject: TLabel;
    lblNewGroupCriteria: TLabel;
    lblNewCriteria: TLabel;
    edthkNewFreedomObject: THotKey;
    edthkNewFreedomDBObject: THotKey;
    edthkNewGroupCriteria: THotKey;
    edthkNewCriteria: THotKey;
    lblReverse: TLabel;
    edthkInverteSentence: THotKey;
    lblMoveBlockUp: TLabel;
    edthkMoveBlockUp: THotKey;
    lblMoveBlockDown: TLabel;
    edthkMoveBlockDown: THotKey;
    lblDeclareVariable: TLabel;
    edthkDeclareVariable: THotKey;
    lblExtractVariable: TLabel;
    edthkExtractVariable: THotKey;
    lblExtractConstant: TLabel;
    edthkExtractConstant: THotKey;
    lblFindMethod: TLabel;
    edthkFindMethod: THotKey;
    lblExtractMethod: TLabel;
    edthkExtractMethod: THotKey;
    lblDeclareMethod: TLabel;
    edthkDeclareMethod: THotKey;
    lblViewRefactorings: TLabel;
    edthkViewRefactorings: THotKey;
    lblDeclareUnit: TLabel;
    edthkDeclareUnit: THotKey;
    lblNewUnit: TLabel;
    edthkNewUnit: THotKey;
    lblUseUnit: TLabel;
    edthkUseUnit: THotKey;
    lblFreedomORMOptions: TLabel;
    edthkFreedomORMOptions: THotKey;
    procedure btnLoadFromIniFileClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnSaveToIniFileClick(Sender: TObject);
    procedure btnAddClassClick(Sender: TObject);
    procedure btnEditClassClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    procedure ExtractRenamedClassesFromIniFile(pIniFileName: String);
    procedure GetConfiguration;
    procedure GetRenameConfiguration;
    procedure ExtractRenamedClasses;
    procedure GetShortCutConfiguration;
    procedure LoadRenamedClassesFromIniFile;
    procedure RemoveSelectedClass;
    procedure SaveToIniFile;
    procedure AddNewClasses;
    procedure EditClasses;
    procedure DoOK;
    procedure ApplyConfiguration;
    procedure ApplyShortCuts;
  protected
    procedure DoShow; override;
  public
    class procedure Configure;
  end;

implementation

{$R *.dfm}

uses
  AM.Freedom.ComponentRenamerConfig,
  AM.Freedom.dclFreedomORMConfig,
  System.IniFiles,
  System.IOUtils,
  AM.Freedom.frmReconfigurePrefix,
  AM.Freedom.FreedomBindKeyBoard;

{ TfrmConfigFreedomORM }

procedure TfrmConfigFreedomORM.AddNewClasses;
var
  lfrmReconfigurePrefix: TfrmReconfigurePrefix;
  lItem: TListItem;
begin
  lfrmReconfigurePrefix := nil;
  repeat
    if (Assigned(lfrmReconfigurePrefix)) then
    begin
      lItem := lvRenameClassConfig.Items.Add;
      lItem.Caption := lfrmReconfigurePrefix.ClassNameRename;
      lItem.SubItems.Add(lfrmReconfigurePrefix.PrefixName);
      lItem.Checked := lfrmReconfigurePrefix.IgnoreClassType;
      FreeAndNil(lfrmReconfigurePrefix);
    end;
    lfrmReconfigurePrefix := TfrmReconfigurePrefix.Create(nil);
  until lfrmReconfigurePrefix.ShowModal = mrCancel;
  FreeAndNil(lfrmReconfigurePrefix);
end;

procedure TfrmConfigFreedomORM.ApplyConfiguration;
var
  lIndex: Integer;
  lItem: TListItem;
  lStrings: TStringList;
  lIni: TIniFile;
  lSection: String;

begin
  ApplyShortCuts;
  lStrings := TStringList.Create;
  try
    lIni := TIniFile.Create(TComponentRenamerConfig.IniFileName);
    lIni.ReadSections(lStrings);
    for lSection in lStrings do
    begin
      lIni.EraseSection(lSection);
    end;
    for lIndex := 0 to lvRenameClassConfig.Items.Count - 1 do
    begin
      lItem := lvRenameClassConfig.Items.Item[lIndex];
      lIni.WriteString(lItem.Caption, 'Prefix', lItem.SubItems.Strings[0]);
      lIni.WriteBool(lItem.Caption, 'Ignored', lItem.Checked);
    end;
    TdclFreedomORMConfig.GetInstance.ActiveRename := chkActiveRename.Checked;
    TComponentRenamerConfig.ReloadConfig;
    TFreedomBindKeyBoard.RefreshKeyBindings;
  finally
    lStrings.Free;
  end;
end;

procedure TfrmConfigFreedomORM.btnAddClassClick(Sender: TObject);
begin
  inherited;
  AddNewClasses;
end;

procedure TfrmConfigFreedomORM.btnEditClassClick(Sender: TObject);
begin
  inherited;
  EditClasses;
end;

procedure TfrmConfigFreedomORM.btnLoadFromIniFileClick(Sender: TObject);
begin
  inherited;
  LoadRenamedClassesFromIniFile;
end;

procedure TfrmConfigFreedomORM.btnOKClick(Sender: TObject);
begin
  inherited;
  DoOK;
end;

procedure TfrmConfigFreedomORM.btnRemoveClick(Sender: TObject);
begin
  inherited;
  RemoveSelectedClass;
end;

procedure TfrmConfigFreedomORM.btnSaveToIniFileClick(Sender: TObject);
begin
  inherited;
  SaveToIniFile;
end;

class procedure TfrmConfigFreedomORM.Configure;
var
  lfrmConfigFreedomORM: TfrmConfigFreedomORM;
begin
  lfrmConfigFreedomORM := TfrmConfigFreedomORM.Create(Application);
  try
    lfrmConfigFreedomORM.GetConfiguration;
    lfrmConfigFreedomORM.ShowModal;
  finally
    FreeAndNil(lfrmConfigFreedomORM);
  end;
end;

procedure TfrmConfigFreedomORM.ApplyShortCuts;
begin
  TdclFreedomORMConfig.GetInstance.NewFreedomObject := edthkNewFreedomObject.HotKey;
  TdclFreedomORMConfig.GetInstance.NewFreedomDBObject := edthkNewFreedomDBObject.HotKey;
  TdclFreedomORMConfig.GetInstance.NewGroupCriteria := edthkNewGroupCriteria.HotKey;
  TdclFreedomORMConfig.GetInstance.NewCriteria := edthkNewCriteria.HotKey;
  TdclFreedomORMConfig.GetInstance.ReverseSentence := edthkInverteSentence.HotKey;
  TdclFreedomORMConfig.GetInstance.MoveBlockUp := edthkMoveBlockUp.HotKey;
  TdclFreedomORMConfig.GetInstance.MoveBlockDown := edthkMoveBlockDown.HotKey;
  TdclFreedomORMConfig.GetInstance.DeclareVariable := edthkDeclareVariable.HotKey;
  TdclFreedomORMConfig.GetInstance.ExtractVariable := edthkExtractVariable.HotKey;
  TdclFreedomORMConfig.GetInstance.ExtractConstant := edthkExtractConstant.HotKey;
  TdclFreedomORMConfig.GetInstance.ExtractMethod := edthkExtractMethod.HotKey;
  TdclFreedomORMConfig.GetInstance.DeclareMethod := edthkDeclareMethod.HotKey;
  TdclFreedomORMConfig.GetInstance.FindMethod := edthkFindMethod.HotKey;
  TdclFreedomORMConfig.GetInstance.ViewRefactorings := edthkViewRefactorings.HotKey;
  TdclFreedomORMConfig.GetInstance.DeclareUnit := edthkDeclareUnit.HotKey;
  TdclFreedomORMConfig.GetInstance.NewUnit := edthkNewUnit.HotKey;
  TdclFreedomORMConfig.GetInstance.UseUnit := edthkUseUnit.HotKey;
  TdclFreedomORMConfig.GetInstance.FreedomORMOptions := edthkFreedomORMOptions.HotKey;
end;

procedure TfrmConfigFreedomORM.DoOK;
begin
  ApplyConfiguration;
  Close;
end;

procedure TfrmConfigFreedomORM.DoShow;
begin
  inherited;
  pgcConfig.ActivePageIndex := 0;
end;

procedure TfrmConfigFreedomORM.EditClasses;
var
  lfrmReconfigurePrefix: TfrmReconfigurePrefix;
  lItem: TListItem;
  lIndex: Integer;
begin
  for lIndex := 0 to lvRenameClassConfig.Items.Count - 1 do
  begin
    lItem := lvRenameClassConfig.Items.Item[lIndex];
    if lItem.Selected then
    begin
      lfrmReconfigurePrefix := TfrmReconfigurePrefix.Create(nil);
      try
        lfrmReconfigurePrefix.ClassNameRename := lItem.Caption;
        lfrmReconfigurePrefix.PrefixName := lItem.SubItems.Strings[0];
        lfrmReconfigurePrefix.IgnoreClassType := lItem.Checked;
        if lfrmReconfigurePrefix.ShowModal = mrOk then
        begin
          lItem.Caption := lfrmReconfigurePrefix.ClassNameRename;
          lItem.SubItems.Clear;
          lItem.SubItems.Add(lfrmReconfigurePrefix.PrefixName);
          lItem.Checked := lfrmReconfigurePrefix.IgnoreClassType;
        end;
      finally
        lfrmReconfigurePrefix.Free;
      end;
    end;
  end;
end;

procedure TfrmConfigFreedomORM.ExtractRenamedClasses;
var
  lIniFileName: String;
begin
  lIniFileName := TComponentRenamerConfig.IniFileName;
  ExtractRenamedClassesFromIniFile(lIniFileName);
end;

procedure TfrmConfigFreedomORM.ExtractRenamedClassesFromIniFile(pIniFileName: String);
var
  lIni: TIniFile;
  lStrings: TStrings;
  lSection: String;
  lItem: TListItem;
begin
  if (Trim(pIniFileName) <> '') then
  begin
    lvRenameClassConfig.Items.BeginUpdate;
    lIni := TIniFile.Create(pIniFileName);
    lStrings := TStringList.Create;
    try
      lvRenameClassConfig.Items.Clear;
      lIni.ReadSections(lStrings);
      for lSection in lStrings do
      begin
        lItem := lvRenameClassConfig.Items.Add;
        lItem.Caption := lSection;
        lItem.SubItems.Add(lIni.ReadString(lSection, 'Prefix', ''));
        lItem.Checked := lIni.ReadBool(lSection, 'Ignored', False);
      end;
    finally
      lIni.Free;
      lStrings.Free;
      lvRenameClassConfig.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmConfigFreedomORM.GetConfiguration;
begin
  GetRenameConfiguration;
  GetShortCutConfiguration;
end;

procedure TfrmConfigFreedomORM.GetRenameConfiguration;
begin
  chkActiveRename.Checked := TdclFreedomORMConfig.GetInstance.ActiveRename;
  ExtractRenamedClasses;
end;

procedure TfrmConfigFreedomORM.GetShortCutConfiguration;
begin
  edthkNewFreedomObject.HotKey := TdclFreedomORMConfig.GetInstance.NewFreedomObject;
  edthkNewFreedomDBObject.HotKey := TdclFreedomORMConfig.GetInstance.NewFreedomDBObject;
  edthkNewGroupCriteria.HotKey := TdclFreedomORMConfig.GetInstance.NewGroupCriteria;
  edthkNewCriteria.HotKey := TdclFreedomORMConfig.GetInstance.NewCriteria;
  edthkInverteSentence.HotKey := TdclFreedomORMConfig.GetInstance.ReverseSentence;
  edthkMoveBlockUp.HotKey := TdclFreedomORMConfig.GetInstance.MoveBlockUp;
  edthkMoveBlockDown.HotKey := TdclFreedomORMConfig.GetInstance.MoveBlockDown;
  edthkDeclareVariable.HotKey := TdclFreedomORMConfig.GetInstance.DeclareVariable;
  edthkExtractVariable.HotKey := TdclFreedomORMConfig.GetInstance.ExtractVariable;
  edthkExtractConstant.HotKey := TdclFreedomORMConfig.GetInstance.ExtractConstant;
  edthkExtractMethod.HotKey := TdclFreedomORMConfig.GetInstance.ExtractMethod;
  edthkDeclareMethod.HotKey := TdclFreedomORMConfig.GetInstance.DeclareMethod;
  edthkFindMethod.HotKey := TdclFreedomORMConfig.GetInstance.FindMethod;
  edthkViewRefactorings.HotKey := TdclFreedomORMConfig.GetInstance.ViewRefactorings;
  edthkDeclareUnit.HotKey := TdclFreedomORMConfig.GetInstance.DeclareUnit;
  edthkNewUnit.HotKey := TdclFreedomORMConfig.GetInstance.NewUnit;
  edthkUseUnit.HotKey := TdclFreedomORMConfig.GetInstance.UseUnit;
  edthkFreedomORMOptions.HotKey := TdclFreedomORMConfig.GetInstance.FreedomORMOptions;
end;

procedure TfrmConfigFreedomORM.LoadRenamedClassesFromIniFile;
var
  lOpenDlg: TOpenDialog;
begin
  lOpenDlg := TOpenDialog.Create(nil);
  try
    lOpenDlg.Filter := 'Arquivos ini(*.ini)|*.ini';
    if lOpenDlg.Execute(GetDesktopWindow) then
    begin
      ExtractRenamedClassesFromIniFile(lOpenDlg.FileName);
    end;
  finally
    lOpenDlg.Free;
  end;
end;

procedure TfrmConfigFreedomORM.RemoveSelectedClass;
var
  lItem: TListItem;
  lIndex: Integer;
begin
  if (lvRenameClassConfig.SelCount > 0) then
  begin
    for lIndex := lvRenameClassConfig.Items.Count -1  downto 0 do
    begin
      lItem := lvRenameClassConfig.Items.Item[lIndex];
      if (lItem.Selected) then
      begin
        lItem.Free;
      end;
    end;
  end;
end;

procedure TfrmConfigFreedomORM.SaveToIniFile;
var
  lSaveDlg: TSaveDialog;
begin
  lSaveDlg := TSaveDialog.Create(nil);
  try
    lSaveDlg.Filter := 'Arquivos ini(*.ini)|*.ini';
    if lSaveDlg.Execute(GetDesktopWindow) then
    begin
      TFile.Copy(TComponentRenamerConfig.IniFileName, lSaveDlg.FileName, True);
    end;
  finally
    lSaveDlg.Free;
  end;
end;

end.
