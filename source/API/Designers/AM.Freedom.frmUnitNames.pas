unit AM.Freedom.frmUnitNames;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  AM.Freedom.frmBase,
  Vcl.ImgList,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  UnitReader.EnumerationTypes,
  System.Generics.Collections,
  ToolsAPI,
  AM.Freedom.ProjectUnit,
  Vcl.Menus;

type
  TUnitDeclaration = class
  private
    FDeclareUnitNames: TStrings;
    FDeclareOnInterface: Boolean;
    FDeclareOnNewLine: Boolean;
    procedure SetDeclareUnitNames(const Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    property DeclareUnitNames: TStrings read FDeclareUnitNames write SetDeclareUnitNames;
    property DeclareOnInterface: Boolean read FDeclareOnInterface write FDeclareOnInterface;
    property DeclareOnNewLine: Boolean read FDeclareOnNewLine write FDeclareOnNewLine;
  end;

  TfrmUnitNames = class(TfrmBase)
    btnedtSearch: TButtonedEdit;
    lvUnits: TListView;
    btnOK: TButton;
    btnCancel: TButton;
    chkDeclareOnInterface: TCheckBox;
    chkDeclareOnNewLine: TCheckBox;
    btnAddToList: TButton;
    ppmUnits: TPopupMenu;
    miHideSystemUnits: TMenuItem;
    miProjectSeparator: TMenuItem;
    procedure btnedtSearchChange(Sender: TObject);
    procedure btnedtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnOKClick(Sender: TObject);
    procedure btnAddToListDropDownClick(Sender: TObject);
    procedure btnAddToListClick(Sender: TObject);
    procedure miHideSystemUnitsClick(Sender: TObject);
    procedure lvUnitsDblClick(Sender: TObject);
  strict private
    class var FUnits: TList<TList<string>>;
    procedure InternalDoOK;
  strict private
  const
    cCaminhoLibrary = 'Software\Embarcadero\BDS\15.0\Library\';
  private
    FProjectUnits: TProjectUnits;
    FAlreadDeclaredUnits: TStrings;
    FSelectedUnits: TStrings;
    FSystemUnits: Boolean;
    FHideSystemUnits: Boolean;
    FHideProjects: TStrings;
    FNewSystemUnits: Boolean;
    FCanSearch: Boolean;
    function SelectedUnitNames: TStrings;
    function OnInterface: Boolean;
    function DeclareOnNewLine: Boolean;
    procedure ListUnits(pSystemUnits: Boolean);
    procedure FixProjectUnits;
    procedure FixUnits;
    procedure FixSystemUnits;
    procedure ExtractUnitsFromPath(pPath: string);
    function GetLibraryPath: string;
    function GetBrowsingPath: string;
    procedure ListUnitsInDir(aDirectory: string);
    function RemoveExtension(aFileName: string): string;
    procedure FixViewUnits(pSearchText: string = '');
    function ExtractNameForEnvironmentVariable(aEnvirommentVariable: string): string;
    procedure DoOk;
    procedure AddSelectedToList;
    procedure ExtractFilesForProject(pProject: IOTAProject; pIsCurrentProject: Boolean = False);
    procedure OnProjectMenuClick(Sender: TObject);
    procedure RemoveHideProject(pProjectName: string);
    procedure AddHideProject(pProjectName: string);
    procedure DoSearch;
  protected
    procedure DoInitializeHistory; override;
    procedure DoFinalizeHistory; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function DeclareUnit(pAlreadDeclaredUnits: TStrings; pCurrentSection: TTokenType): TUnitDeclaration;
    class function UseUnit(pAlreadDeclaredUnits: TStrings; pCurrentSection: TTokenType): TUnitDeclaration;
  end;

implementation

uses
  System.Win.Registry,
  System.StrUtils,
  System.SysUtils,
  AM.Freedom.frmEditSelectedUnits,
  AM.Freedom.FileSearchWriter;
{$R *.dfm}
{ TfrmUnitNames }

class function TfrmUnitNames.DeclareUnit(pAlreadDeclaredUnits: TStrings; pCurrentSection: TTokenType): TUnitDeclaration;
var
  lfrmUnitNames: TfrmUnitNames;
begin
  lfrmUnitNames := TfrmUnitNames.Create(nil);
  Result := nil;
  try
    lfrmUnitNames.FAlreadDeclaredUnits := pAlreadDeclaredUnits;
    lfrmUnitNames.Caption := 'Declare Unit';
    lfrmUnitNames.chkDeclareOnInterface.Checked := pCurrentSection = tkInterfaceSection;
    lfrmUnitNames.ListUnits(True);
    if (lfrmUnitNames.ShowModal = mrOK) then
    begin
      Result := TUnitDeclaration.Create;
      Result.DeclareUnitNames := lfrmUnitNames.SelectedUnitNames;
      Result.DeclareOnInterface := lfrmUnitNames.OnInterface;
      Result.DeclareOnNewLine := lfrmUnitNames.DeclareOnNewLine;
    end;
  finally
    lfrmUnitNames.Free;
  end;
end;

destructor TfrmUnitNames.Destroy;
begin
  FSelectedUnits.Free;
  FHideProjects.Free;
  inherited;
end;

procedure TfrmUnitNames.ListUnits(pSystemUnits: Boolean);
begin
  Screen.Cursor := crHourGlass;
  try
    FSystemUnits := pSystemUnits;
    if (not Assigned(FUnits)) and FSystemUnits then
    begin
      FixUnits;
    end;
    if (not Assigned(FProjectUnits)) then
    begin
      FixProjectUnits;
    end;
    miHideSystemUnits.Visible := FSystemUnits;
    miProjectSeparator.Visible := FSystemUnits;
    if (not Assigned(FUnits)) and FSystemUnits then
    begin
      repeat
        FUnits := TFileSearchWriter.RetrieveUnits;
        Application.ProcessMessages;
      until Assigned(FUnits);
    end;
    FixSystemUnits;
    FixViewUnits;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmUnitNames.FixSystemUnits;
var
  lIndex: Integer;
  lList: TList<string>;
  lInternalIndex: Integer;
  lInternalList: TList<string>;
  lListIndex: Integer;
begin
  if (not Assigned(FUnits)) and FSystemUnits and FNewSystemUnits then
  begin
    for lIndex := 0 to FUnits.Count - 2 do
    begin
      lList := FUnits.Items[lIndex];
      for lInternalIndex := lIndex + 1 to FUnits.Count - 1 do
      begin
        lInternalList := FUnits.Items[lInternalIndex];
        for lListIndex := lInternalList.Count - 1 downto 0 do
        begin
          if (lList.Contains(lInternalList.Items[lListIndex])) then
          begin
            lInternalList.Delete(lListIndex);
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmUnitNames.ListUnitsInDir(aDirectory: string);
begin
  aDirectory := IncludeTrailingPathDelimiter(aDirectory);
  TFileSearchWriter.AddDirectory(aDirectory);
end;

procedure TfrmUnitNames.lvUnitsDblClick(Sender: TObject);
begin
  inherited;
  InternalDoOK;
end;

procedure TfrmUnitNames.miHideSystemUnitsClick(Sender: TObject);
begin
  inherited;
  FHideSystemUnits := miHideSystemUnits.Checked;
  DoSearch;
end;

function TfrmUnitNames.OnInterface: Boolean;
begin
  Result := chkDeclareOnInterface.Checked;
end;

function TfrmUnitNames.RemoveExtension(aFileName: string): string;
var
  lStrings: TStrings;
  lIndex: Integer;
begin
  lStrings := TStringList.Create;
  try
    ExtractStrings(['.'], [' '], PWideChar(aFileName), lStrings);
    Result := '';
    for lIndex := 0 to lStrings.Count - 2 do
    begin
      Result := Result + ifthen(Result <> '', '.') + lStrings.Strings[lIndex];
    end;
  finally
    lStrings.Free;
  end;
end;

procedure TfrmUnitNames.FixUnits;
begin
  FNewSystemUnits := True;
  TFileSearchWriter.ClearThreadList;
  ExtractUnitsFromPath(GetLibraryPath);
  ExtractUnitsFromPath(GetBrowsingPath);
  TFileSearchWriter.StartAllThreads;
end;

procedure TfrmUnitNames.btnAddToListClick(Sender: TObject);
begin
  AddSelectedToList;
end;

procedure TfrmUnitNames.btnAddToListDropDownClick(Sender: TObject);
begin
  inherited;
  if (FSelectedUnits.Count > 0) then
  begin
    TfrmEditSelectedUnits.EditSelectedUnits(FSelectedUnits);
  end;
end;

procedure TfrmUnitNames.btnedtSearchChange(Sender: TObject);
begin
  DoSearch;
end;

procedure TfrmUnitNames.btnedtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_DOWN) then
  begin
    if (lvUnits.Items.Count > 0) then
    begin
      lvUnits.SetFocus;
      lvUnits.Items.Item[0].Selected := True;
      lvUnits.Items.Item[0].Focused := True;
    end;
  end;
end;

procedure TfrmUnitNames.btnOKClick(Sender: TObject);
begin
  inherited;
  DoOk;
end;

constructor TfrmUnitNames.Create(AOwner: TComponent);
begin
  inherited;
  FSelectedUnits := TStringList.Create;
  FHideProjects := TStringList.Create;
  FNewSystemUnits := False;
end;

function TfrmUnitNames.DeclareOnNewLine: Boolean;
begin
  Result := chkDeclareOnNewLine.Checked;
end;

procedure TfrmUnitNames.DoFinalizeHistory;
begin
  inherited;
  IniFile.WriteBool('Config', 'DeclareOnInterface', chkDeclareOnInterface.Checked);
  IniFile.WriteBool('Config', 'DeclareOnNewLine', chkDeclareOnNewLine.Checked);
end;

procedure TfrmUnitNames.DoInitializeHistory;
begin
  inherited;
  chkDeclareOnInterface.Checked := IniFile.ReadBool('Config', 'DeclareOnInterface', False);
  chkDeclareOnNewLine.Checked := IniFile.ReadBool('Config', 'DeclareOnNewLine', False);
end;

procedure TfrmUnitNames.DoOk;
begin
  if (lvUnits.SelCount > 0) or (FSelectedUnits.Count > 0) then
  begin
    InternalDoOK;
  end;
end;

procedure TfrmUnitNames.InternalDoOK;
begin
  AddSelectedToList;
  ModalResult := mrOK;
end;

procedure TfrmUnitNames.AddSelectedToList;
var
  lIndex: Integer;
begin
  if (lvUnits.SelCount > 0) then
  begin
    for lIndex := 0 to lvUnits.Items.Count - 1 do
    begin
      if (lvUnits.Items.Item[lIndex].Selected) and (FSelectedUnits.IndexOf(lvUnits.Items.Item[lIndex].Caption) < 0) then
      begin
        FSelectedUnits.Add(lvUnits.Items.Item[lIndex].Caption);
      end;
    end;
  end;
end;

function TfrmUnitNames.ExtractNameForEnvironmentVariable(aEnvirommentVariable: string): string;
begin
  Result := Copy(aEnvirommentVariable, 3, length(aEnvirommentVariable) - 3);
end;

procedure TfrmUnitNames.ExtractUnitsFromPath(pPath: string);
var
  lStrings: TStrings;
  lString: string;
  lInternalString: string;
  lDirectory: string;
  lFiles: TStrings;
  lEnvirommentVariable: string;
  lPos: Integer;
begin
  lFiles := TStringList.Create;
  lStrings := TStringList.Create;
  try
    ExtractStrings([';'], [' '], PWideChar(pPath), lStrings);
    for lString in lStrings do
    begin
      lInternalString := lString;
      while (Pos('$', lInternalString) > 0) do
      begin
        lPos := Pos('$', lInternalString);
        lEnvirommentVariable := Copy(lInternalString, lPos, Pos(')', lInternalString, lPos) - (lPos - 1));
        if ContainsText(lEnvirommentVariable, '$(Platform)') then
        begin
          lDirectory := (BorlandIDEServices as IOTAModuleServices).GetActiveProject.CurrentPlatform;
        end
        else if ContainsText(lEnvirommentVariable, '$(Config)') then
        begin
          lDirectory := (BorlandIDEServices as IOTAModuleServices).GetActiveProject.CurrentConfiguration;
        end
        else
        begin
          lDirectory := GetEnvironmentVariable(ExtractNameForEnvironmentVariable(lEnvirommentVariable));
        end;
        lInternalString := ReplaceText(lInternalString, lEnvirommentVariable, lDirectory);
      end;
      lInternalString := IncludeTrailingPathDelimiter(lInternalString);
      ListUnitsInDir(lInternalString);
    end;
  finally
    lFiles.Free;
    lStrings.Free;
  end;
end;

function TfrmUnitNames.GetBrowsingPath: string;
var
  lRegistry: TRegistry;
const
  cBrowsingPath = 'Browsing Path';
begin
  lRegistry := TRegistry.Create;
  try
    lRegistry.RootKey := HKEY_CURRENT_USER;
    lRegistry.OpenKey(cCaminhoLibrary + (BorlandIDEServices as IOTAModuleServices).GetActiveProject.CurrentPlatform + '\', False);
    Result := lRegistry.ReadString(cBrowsingPath);
  finally
    lRegistry.Free
  end;
end;

function TfrmUnitNames.GetLibraryPath: string;
var
  lRegistry: TRegistry;
const
  cSearchPath = 'Search Path';
begin
  lRegistry := TRegistry.Create;
  try
    lRegistry.RootKey := HKEY_CURRENT_USER;
    lRegistry.OpenKey(cCaminhoLibrary + (BorlandIDEServices as IOTAModuleServices).GetActiveProject.CurrentPlatform + '\', False);
    Result := lRegistry.ReadString(cSearchPath);
  finally
    lRegistry.Free
  end;
end;

procedure TfrmUnitNames.FixViewUnits(pSearchText: string);
var
  lFile: string;
  lItem: TListItem;
  lProjectName: string;
  lUnit: TProjectUnit;
  lList: TList<string>;
begin
  lvUnits.Items.BeginUpdate;
  Screen.Cursor := crHourGlass;
  FCanSearch := False;
  try
    lProjectName := '';
    lvUnits.Items.Clear;
    for lUnit in FProjectUnits do
    begin
      if (FHideProjects.IndexOf(lUnit.ProjectName) < 0) then
      begin
        lFile := lUnit.FileName;
        if (FAlreadDeclaredUnits.IndexOf(lFile) < 0) and ((pSearchText = '') or ContainsText(lFile, pSearchText)) then
        begin
          lItem := lvUnits.Items.Add;
          lItem.Caption := lFile;
          lItem.SubItems.Add(lUnit.ProjectName);
          lItem.ImageIndex := 0;
        end;
      end;
    end;
    if FSystemUnits and Assigned(FUnits) and (not FHideSystemUnits) then
    begin
      for lList in FUnits do
      begin
        for lFile in lList do
        begin
          if (FAlreadDeclaredUnits.IndexOf(lFile) < 0) and ((pSearchText = '') or ContainsText(lFile, pSearchText)) then
          begin
            lItem := lvUnits.Items.Add;
            lItem.Caption := lFile;
            lItem.SubItems.Add('System units');
            lItem.ImageIndex := 0;
          end;
        end;
      end;
    end;
  finally
    lvUnits.Items.EndUpdate;
    FCanSearch := True;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmUnitNames.FixProjectUnits;
var
  lModuleServices: IOTAModuleServices;
  lIndex: Integer;
  lCurrentProject: IOTAProject;
begin
  FProjectUnits := TProjectUnits.Create;
  lModuleServices := (BorlandIDEServices as IOTAModuleServices);
  lCurrentProject := lModuleServices.GetActiveProject;
  ExtractFilesForProject(lCurrentProject, True);
  for lIndex := 0 to lModuleServices.MainProjectGroup.ProjectCount - 1 do
  begin
    if (lCurrentProject <> lModuleServices.MainProjectGroup.Projects[lIndex]) then
    begin
      ExtractFilesForProject(lModuleServices.MainProjectGroup.Projects[lIndex]);
    end;
  end;
end;

procedure TfrmUnitNames.ExtractFilesForProject(pProject: IOTAProject; pIsCurrentProject: Boolean);
var
  lFiles: TStrings;
  lInternalFile: string;
  lFile: string;
  lProjectName: string;
  lMenuItem: TMenuItem;
begin
  lFiles := TStringList.Create;
  try
    pProject.GetCompleteFileList(lFiles);
    lProjectName := RemoveExtension(ExtractFileName(pProject.FileName));
    if (not pIsCurrentProject) then
    begin
      AddHideProject(lProjectName);
    end;
    lMenuItem := TMenuItem.Create(ppmUnits);
    lMenuItem.Caption := lProjectName;
    lMenuItem.AutoCheck := True;
    lMenuItem.Checked := pIsCurrentProject;
    lMenuItem.OnClick := OnProjectMenuClick;
    ppmUnits.Items.Add(lMenuItem);
    for lFile in lFiles do
    begin
      if (ExtractFileExt(lFile) = '.pas') or (ExtractFileExt(lFile) = '.dcu') then
      begin
        lInternalFile := RemoveExtension(ExtractFileName(lFile));
        if (not FProjectUnits.Contains(lInternalFile, lProjectName)) then
        begin
          FProjectUnits.AddUnit(lInternalFile, lProjectName);
        end;
      end;
    end;
  finally
    lFiles.Free;
  end;
end;

procedure TfrmUnitNames.OnProjectMenuClick(Sender: TObject);
begin
  if (Sender is TMenuItem) then
  begin
    if TMenuItem(Sender).Checked then
    begin
      RemoveHideProject(ReplaceText(TMenuItem(Sender).Caption, '&', ''));
    end
    else
    begin
      AddHideProject(ReplaceText(TMenuItem(Sender).Caption, '&', ''));
    end;
  end;
end;

procedure TfrmUnitNames.AddHideProject(pProjectName: string);
var
  lIndex: Integer;
begin
  lIndex := FHideProjects.IndexOf(pProjectName);
  if (lIndex < 0) then
  begin
    FHideProjects.Add(pProjectName);
    DoSearch;
  end;
end;

procedure TfrmUnitNames.DoSearch;
begin
  if (FCanSearch) then
  begin
    FixViewUnits(btnedtSearch.Text);
  end;
end;

procedure TfrmUnitNames.RemoveHideProject(pProjectName: string);
var
  lIndex: Integer;
begin
  lIndex := FHideProjects.IndexOf(pProjectName);
  if (lIndex > -1) then
  begin
    FHideProjects.Delete(lIndex);
    DoSearch;
  end;
end;

function TfrmUnitNames.SelectedUnitNames: TStrings;
begin
  Result := FSelectedUnits;
end;

class function TfrmUnitNames.UseUnit(pAlreadDeclaredUnits: TStrings; pCurrentSection: TTokenType): TUnitDeclaration;
var
  lfrmUnitNames: TfrmUnitNames;
begin
  lfrmUnitNames := TfrmUnitNames.Create(nil);
  Result := nil;
  try
    lfrmUnitNames.FAlreadDeclaredUnits := pAlreadDeclaredUnits;
    lfrmUnitNames.Caption := 'Use Unit';
    lfrmUnitNames.chkDeclareOnInterface.Checked := pCurrentSection = tkInterfaceSection;
    lfrmUnitNames.ListUnits(False);
    if (lfrmUnitNames.ShowModal = mrOK) then
    begin
      Result := TUnitDeclaration.Create;
      Result.DeclareUnitNames := lfrmUnitNames.SelectedUnitNames;
      Result.DeclareOnInterface := lfrmUnitNames.OnInterface;
      Result.DeclareOnNewLine := lfrmUnitNames.DeclareOnNewLine;
    end;
  finally
    lfrmUnitNames.Free;
  end;
end;
{ TUnitDeclaration }

constructor TUnitDeclaration.Create;
begin
  FDeclareUnitNames := TStringList.Create;
end;

destructor TUnitDeclaration.Destroy;
begin
  FDeclareUnitNames.Free;
  inherited;
end;

procedure TUnitDeclaration.SetDeclareUnitNames(const Value: TStrings);
begin
  FDeclareUnitNames.Assign(Value);
end;

end.
