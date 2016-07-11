unit AM.Freedom.frmNewDBObjectEditor;

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
  Vcl.ExtCtrls,
  Vcl.Imaging.pngimage,
  Vcl.ImgList,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Grids,
  Vcl.ValEdit,
  AM.Freedom.frmBase,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.FreedomObjectDescriptor,
  AM.Freedom.TableSchemas,
  Data.Bind.Components,
  Data.Bind.ObjectScope,
  AM.Freedom.frmNewUnit;

type
  TfrmNewDBObjectEditor = class(TfrmBase)
    {$REGION 'Fields'}
    imgDB: TImage;
    lblWelcome: TLabel;
    FlowPanel1: TFlowPanel;
    pgcSteps: TPageControl;
    tsDataBaseType: TTabSheet;
    tsDataBaseParams: TTabSheet;
    tsTableSelector: TTabSheet;
    tsAdditionalOptions: TTabSheet;
    lblDataBaseType: TLabel;
    rdbPostGree: TRadioButton;
    rdbMSSQL: TRadioButton;
    rdbFireBird: TRadioButton;
    lblConnectionParams: TLabel;
    vleConnectionParams: TValueListEditor;
    btnTestConn: TButton;
    lblTestConnection: TLabel;
    lblTables: TLabel;
    btnBack: TButton;
    btnNext: TButton;
    btnFinish: TButton;
    btnCancel: TButton;
    lblAdiditionalInformation: TLabel;
    lblMappingCharCase: TLabel;
    lblSchemaCharCase: TLabel;
    cbxMappingCharCase: TComboBox;
    cbxSchemaCharCase: TComboBox;
    chkRemoveFieldUnderscore: TCheckBox;
    lvTables: TListView;
    btnedtFilter: TButtonedEdit;
    chkRemoveClassUnderscore: TCheckBox;
    Label1: TLabel;
    cbxCapitalization: TComboBox;
    btnOptions: TButton;
    tsMappedProject: TTabSheet;
    tvDBProject: TTreeView;
    {$ENDREGION}
    procedure btnNextClick(Sender: TObject);
    procedure btnTestConnClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnedtFilterChange(Sender: TObject);
    procedure btnedtFilterRightButtonClick(Sender: TObject);
    procedure btnFinishClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOptionsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  strict private
  const
    cDataBaseType = 0;
    cDataBaseParams = 1;
    cTableSelector = 2;
    cAdditionalOptions = 3;
    cMappedProject = 4;
  strict private
    FFreedomProjectDescriptor: TFreedomProjectDescriptor;
    FTableList: TTableSchemaList;
    procedure InitializeTabSheets;
    procedure RefreshButtons;
    function GetDataBaseType: TConnectorType;
    procedure GetDataBaseTypeHistoryParams;
    procedure GetParamsFromDataBaseType;
    procedure GetFireBirdDBParams;
    procedure GetMSSQLDBParams;
    procedure GetPGDBParams;
    procedure GetDataBaseInformation;
    procedure ValidateTableSelector;
    procedure PopulateTableNames(pFilter: String = '');
    function FindGroup(pHeader: String): TListGroup; overload;
    function FindGroup(pGroupID: Integer): TListGroup; overload;
    function HasTablesChecked: Boolean;
    procedure GetSelectTableSchemas(pListTableSchemas: TTableSchemaList);
    function DoGetProject: Boolean;
    procedure RefreshTreeView;
  protected
    procedure DoInitializeHistory; override;
    procedure DoFinalizeHistory; override;
    function CanAskForClose: Boolean; override;
  public
    class function CreateNewDBUnit: TFreedomProjectDescriptor;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property FreedomProjectDescriptor: TFreedomProjectDescriptor read FFreedomProjectDescriptor;
  end;

implementation

{$R *.dfm}

uses
  System.StrUtils,
  AM.Freedom.dmConnection,
  System.IOUtils,
  AM.Freedom.frmNewDBProject,
  AM.Freedom.frmOptions,
  AM.Freedom.frmClassUnitName;

{ TfrmNewDBObjectEditor }

procedure TfrmNewDBObjectEditor.btnBackClick(Sender: TObject);
begin
  pgcSteps.ActivePageIndex := pgcSteps.ActivePageIndex - 1;
  RefreshButtons;
end;

procedure TfrmNewDBObjectEditor.btnedtFilterChange(Sender: TObject);
begin
  btnedtFilter.RightButton.Visible := btnedtFilter.Text <> '';
  PopulateTableNames(btnedtFilter.Text);
end;

procedure TfrmNewDBObjectEditor.btnedtFilterRightButtonClick(Sender: TObject);
begin
  inherited;
  btnedtFilter.Text := '';
  btnedtFilter.SetFocus;
end;

procedure TfrmNewDBObjectEditor.btnFinishClick(Sender: TObject);
begin
  if Assigned(FFreedomProjectDescriptor) then
  begin
    ModalResult := mrOk;
  end
  else
  begin
    ModalResult := mrCancel;
  end;
end;

procedure TfrmNewDBObjectEditor.btnNextClick(Sender: TObject);
var
  lCanContinue: Boolean;
begin
  lCanContinue := True;
  case pgcSteps.ActivePageIndex of
    cDataBaseType:
      GetDataBaseTypeHistoryParams;
    cDataBaseParams:
      GetDataBaseInformation;
    cTableSelector:
      ValidateTableSelector;
    cAdditionalOptions:
      lCanContinue := DoGetProject;
  end;
  if (lCanContinue) then
  begin
    pgcSteps.ActivePageIndex := pgcSteps.ActivePageIndex + 1;
    lblTestConnection.Caption := '';
    RefreshButtons;
    RefreshTreeView;
  end else
  begin
    ModalResult := mrCancel;
  end;
end;

procedure TfrmNewDBObjectEditor.btnOptionsClick(Sender: TObject);
begin
  FinalizeHistory;
  TfrmOptions.ShowOptions;
  ReloadIniFile;
end;

procedure TfrmNewDBObjectEditor.btnTestConnClick(Sender: TObject);
begin
  TdmConnection.TestConnection(GetDataBaseType, vleConnectionParams.Strings, lblTestConnection);
end;

function TfrmNewDBObjectEditor.CanAskForClose: Boolean;
begin
  Result := pgcSteps.ActivePageIndex > 0;
end;

constructor TfrmNewDBObjectEditor.Create(AOwner: TComponent);
begin
  inherited;
  InitializeTabSheets;
  RefreshButtons;
end;

class function TfrmNewDBObjectEditor.CreateNewDBUnit: TFreedomProjectDescriptor;
var
  lfrmNewDBObjectEditor: TfrmNewDBObjectEditor;
begin
  lfrmNewDBObjectEditor := TfrmNewDBObjectEditor.Create(nil);
  TfrmClassUnitName.InitializeClassHistory(Self);
  try
    lfrmNewDBObjectEditor.ShowModal;
    Result := lfrmNewDBObjectEditor.FreedomProjectDescriptor;
  finally
    lfrmNewDBObjectEditor.Free;
    TfrmClassUnitName.FinalizeClassHistory(Self);
  end;
end;

destructor TfrmNewDBObjectEditor.Destroy;
begin
  FreeAndNil(FTableList);
  FinalizeHistory;
  inherited;
end;

procedure TfrmNewDBObjectEditor.DoFinalizeHistory;
var
  lDataBaseType: Integer;
  lFileName: string;
begin
  inherited;
  if rdbPostGree.Checked then
  begin
    lDataBaseType := 0;
  end
  else if rdbMSSQL.Checked then
  begin
    lDataBaseType := 1;
  end
  else
  begin
    lDataBaseType := 2;
  end;
  IniFile.WriteInteger('Options', 'DataBaseType', lDataBaseType);
  IniFile.WriteBool('Options', 'RemoveFieldUnderscore', chkRemoveFieldUnderscore.Checked);
  IniFile.WriteBool('Options', 'RemoveClassUnderscore', chkRemoveClassUnderscore.Checked);
  IniFile.WriteInteger('Options', 'CapitalizeNames', cbxCapitalization.ItemIndex);
  if (pgcSteps.ActivePageIndex > cDataBaseParams) then
  begin
    lFileName := ExtractFilePath(IniFile.FileName);
    case GetDataBaseType of
      Firebird:
        lFileName := lFileName + 'FBParams.cfg';
      SQLServer:
        lFileName := lFileName + 'MSSQLParams.cfg';
      PostGree:
        lFileName := lFileName + 'PGParams.cfg';
    end;
    vleConnectionParams.Strings.SaveToFile(lFileName);
  end;
end;

function TfrmNewDBObjectEditor.DoGetProject: Boolean;
var
  lSelectedTables: TTableSchemaList;
begin
  lSelectedTables := TTableSchemaList.Create;
  GetSelectTableSchemas(lSelectedTables);
  lSelectedTables.Parameters.DataBaseType := GetDataBaseType;
  lSelectedTables.Parameters.ConnectionParams.AddStrings(vleConnectionParams.Strings);
  lSelectedTables.Parameters.MappingCharCase := TCharCase(cbxMappingCharCase.ItemIndex);
  lSelectedTables.Parameters.SchemaCharCase := TCharCase(cbxSchemaCharCase.ItemIndex);
  lSelectedTables.Parameters.RemoveUnderscoreInFields := chkRemoveFieldUnderscore.Checked;
  lSelectedTables.Parameters.RemoveUnderscoreInClass := chkRemoveClassUnderscore.Checked;
  lSelectedTables.Parameters.CapitalizationMode := TCapitalizationMode(cbxCapitalization.ItemIndex);
  FFreedomProjectDescriptor := TfrmNewDBProject.CreateProject(lSelectedTables);
  Result := Assigned(FFreedomProjectDescriptor);
end;

procedure TfrmNewDBObjectEditor.DoInitializeHistory;
var
  lDataBaseType: Integer;
begin
  lDataBaseType := IniFile.ReadInteger('Options', 'DataBaseType', 0);
  case lDataBaseType of
    0:
      rdbPostGree.Checked := True;
    1:
      rdbMSSQL.Checked := True;
    2:
      rdbFireBird.Checked := True;
  end;
  chkRemoveFieldUnderscore.Checked := IniFile.ReadBool('Options', 'RemoveFieldUnderscore', True);
  chkRemoveClassUnderscore.Checked := IniFile.ReadBool('Options', 'RemoveClassUnderscore', True);
  cbxCapitalization.ItemIndex := IniFile.ReadInteger('Options', 'CapitalizeNames', 0);
end;

function TfrmNewDBObjectEditor.FindGroup(pHeader: String): TListGroup;
var
  lIndex: Integer;
begin
  Result := nil;
  for lIndex := 0 to lvTables.Groups.Count - 1 do
  begin
    if (SameText(lvTables.Groups.Items[lIndex].Header, pHeader)) then
    begin
      Result := lvTables.Groups.Items[lIndex];
      Break;
    end;
  end;
end;

function TfrmNewDBObjectEditor.FindGroup(pGroupID: Integer): TListGroup;
var
  lIndex: Integer;
begin
  Result := nil;
  for lIndex := 0 to lvTables.Groups.Count - 1 do
  begin
    if (lvTables.Groups.Items[lIndex].GroupID = pGroupID) then
    begin
      Result := lvTables.Groups.Items[lIndex];
      Break;
    end;
  end;
end;

procedure TfrmNewDBObjectEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if (ModalResult = mrCancel) then
  begin
    FreeAndNil(FFreedomProjectDescriptor);
  end;
end;

procedure TfrmNewDBObjectEditor.FormShow(Sender: TObject);
begin
  inherited;
  Application.ProcessMessages;
end;

procedure TfrmNewDBObjectEditor.GetDataBaseInformation;
begin
  FreeAndNil(FTableList);
  FTableList := TdmConnection.GetTablesFromDataBaseType(GetDataBaseType,
    vleConnectionParams.Strings);
  PopulateTableNames;
end;

function TfrmNewDBObjectEditor.GetDataBaseType: TConnectorType;
begin
  if (rdbPostGree.Checked) then
  begin
    Result := TConnectorType.PostGree;
  end
  else if (rdbMSSQL.Checked) then
  begin
    Result := TConnectorType.SQLServer;
  end
  else
  begin
    Result := TConnectorType.Firebird;
  end;
end;

procedure TfrmNewDBObjectEditor.GetDataBaseTypeHistoryParams;
var
  lFileName: string;
begin
  lFileName := ExtractFilePath(IniFile.FileName);
  case GetDataBaseType of
    Firebird:
      lFileName := lFileName + 'FBParams.cfg';
    SQLServer:
      lFileName := lFileName + 'MSSQLParams.cfg';
    PostGree:
      lFileName := lFileName + 'PGParams.cfg';
  end;
  if (TFile.Exists(lFileName)) then
  begin
    vleConnectionParams.Strings.LoadFromFile(lFileName);
  end
  else
  begin
    GetParamsFromDataBaseType;
  end;
end;

procedure TfrmNewDBObjectEditor.GetFireBirdDBParams;
begin
  vleConnectionParams.Strings.Clear;
  vleConnectionParams.Strings.Add('Database=');
  vleConnectionParams.Strings.Add('User_Name=SYSDBA');
  vleConnectionParams.Strings.Add('Password=masterkey');
  vleConnectionParams.Strings.Add('Server=localhost');
  vleConnectionParams.Strings.Add('ExtendedMetadata=True');
  vleConnectionParams.Strings.Add('CharacterSet=WIN1252');
  vleConnectionParams.Strings.Add('OSAuthent=No');
  vleConnectionParams.Strings.Add('Protocol=TCPIP');
  vleConnectionParams.Strings.Add('SQLDialect=3');
  vleConnectionParams.Strings.Add('RoleName=');
  vleConnectionParams.Strings.Add('CreateDatabase=No');
  vleConnectionParams.Strings.Add('PageSize=16384');
end;

procedure TfrmNewDBObjectEditor.GetMSSQLDBParams;
begin
  vleConnectionParams.Strings.Clear;
  vleConnectionParams.Strings.Add('OSAuthent=Yes');
  vleConnectionParams.Strings.Add('Database=');
  vleConnectionParams.Strings.Add('User_Name=');
  vleConnectionParams.Strings.Add('Password=');
  vleConnectionParams.Strings.Add('ODBCAdvanced=');
  vleConnectionParams.Strings.Add('LoginTimeout=');
  vleConnectionParams.Strings.Add('Server=.');
  vleConnectionParams.Strings.Add('Network=');
  vleConnectionParams.Strings.Add('Address=');
  vleConnectionParams.Strings.Add('Workstation=');
  vleConnectionParams.Strings.Add('Language=');
  vleConnectionParams.Strings.Add('Encrypt=No');
  vleConnectionParams.Strings.Add('MetaCurSchema=');
  vleConnectionParams.Strings.Add('MetaCurCatalog=');
  vleConnectionParams.Strings.Add('MetaDefSchema=');
  vleConnectionParams.Strings.Add('MetaDefCatalog=');
  vleConnectionParams.Strings.Add('ApplicationName=');
  vleConnectionParams.Strings.Add('MARS=Yes');
  vleConnectionParams.Strings.Add('VariantFormat=String');
  vleConnectionParams.Strings.Add('ExtendedMetadata=False');
  vleConnectionParams.Strings.Add('MetaCaseIns=Yes');
end;

procedure TfrmNewDBObjectEditor.GetParamsFromDataBaseType;
begin
  case GetDataBaseType of
    Firebird:
      GetFireBirdDBParams;
    SQLServer:
      GetMSSQLDBParams;
    PostGree:
      GetPGDBParams;
  end;
end;

procedure TfrmNewDBObjectEditor.GetPGDBParams;
begin
  vleConnectionParams.Strings.Clear;
  vleConnectionParams.Strings.Add('Database=');
  vleConnectionParams.Strings.Add('User_Name=postgres');
  vleConnectionParams.Strings.Add('Password=');
  vleConnectionParams.Strings.Add('LoginTimeout=');
  vleConnectionParams.Strings.Add('Server=localhost');
  vleConnectionParams.Strings.Add('MetaCurSchema=');
  vleConnectionParams.Strings.Add('MetaDefSchema=');
  vleConnectionParams.Strings.Add('ApplicationName=');
  vleConnectionParams.Strings.Add('ExtendedMetadata=False');
  vleConnectionParams.Strings.Add('CharacterSet=');
  vleConnectionParams.Strings.Add('PGAdvanced=');
  vleConnectionParams.Strings.Add('UnknownFormat=ERROR');
  vleConnectionParams.Strings.Add('OidAsBlob=No');
  vleConnectionParams.Strings.Add('Port=5432');
end;

procedure TfrmNewDBObjectEditor.GetSelectTableSchemas(pListTableSchemas: TTableSchemaList);
var
  lIndex: Integer;
  lGroup: TListGroup;
begin
  pListTableSchemas.Clear;
  for lIndex := 0 to lvTables.Items.Count - 1 do
  begin
    if lvTables.Items.Item[lIndex].Checked then
    begin
      lGroup := FindGroup(lvTables.Items.Item[lIndex].GroupID);
      if Assigned(lGroup) then
      begin
        pListTableSchemas.AddTableSchema(lvTables.Items.Item[lIndex].Caption, lGroup.Header);
      end
      else
      begin
        pListTableSchemas.AddTableSchema(lvTables.Items.Item[lIndex].Caption, '');
      end;
    end;
  end;
end;

function TfrmNewDBObjectEditor.HasTablesChecked: Boolean;
var
  lIndex: Integer;
begin
  Result := False;
  for lIndex := 0 to lvTables.Items.Count - 1 do
  begin
    Result := lvTables.Items.Item[lIndex].Checked;
    if (Result) then
    begin
      Break;
    end;
  end;
end;

procedure TfrmNewDBObjectEditor.InitializeTabSheets;
begin
  tsDataBaseType.TabVisible := False;
  tsDataBaseParams.TabVisible := False;
  tsTableSelector.TabVisible := False;
  tsAdditionalOptions.TabVisible := False;
  tsMappedProject.TabVisible := False;
  tsDataBaseType.Show;
end;

procedure TfrmNewDBObjectEditor.PopulateTableNames(pFilter: String);
var
  lTableSchema: TTableSchema;
  lGroup: TListGroup;
  lTableItem: TListItem;
  lGroupID: Integer;
begin
  lvTables.Items.BeginUpdate;
  try
    pFilter := Trim(pFilter);
    lvTables.Items.Clear;
    lvTables.Groups.Clear;
    lvTables.Checkboxes := False;
    lGroupID := 1;
    for lTableSchema in FTableList do
    begin
      if (lTableSchema.TableSchema <> '') and (FindGroup(lTableSchema.TableSchema) = nil) and
        (ContainsText(lTableSchema.TableName, pFilter) or (pFilter = '')) then
      begin
        lGroup := lvTables.Groups.Add;
        lGroup.Header := lTableSchema.TableSchema;
        lGroup.State := [lgsNormal, lgsCollapsible];
        lGroup.TitleImage := 5;
        lGroup.GroupID := lGroupID;
        Inc(lGroupID);
      end;
    end;
    for lTableSchema in FTableList do
    begin
      if ContainsText(lTableSchema.TableName, pFilter) or (pFilter = '') then
      begin
        lTableItem := lvTables.Items.Add;
        lTableItem.Caption := lTableSchema.TableName;
        lTableItem.ImageIndex := 6;
        lTableItem.StateIndex := 6;
        if (lTableSchema.TableSchema <> '') then
        begin
          lGroup := FindGroup(lTableSchema.TableSchema);
          if (Assigned(lGroup)) then
          begin
            lTableItem.GroupID := lGroup.GroupID;
          end;
        end;
      end;
    end;
    lvTables.GroupView := lvTables.Groups.Count > 0;
    lvTables.Checkboxes := True;
  finally
    lvTables.Items.EndUpdate;
  end;
end;

procedure TfrmNewDBObjectEditor.RefreshButtons;
begin
  btnFinish.Enabled := pgcSteps.ActivePageIndex = cMappedProject;
  btnFinish.Default := btnFinish.Enabled;
  btnNext.Enabled := not btnFinish.Enabled;
  btnNext.Default := btnNext.Enabled;
  btnBack.Enabled := pgcSteps.ActivePageIndex > cDataBaseType;
end;

procedure TfrmNewDBObjectEditor.ValidateTableSelector;
begin
  if (not HasTablesChecked) then
  begin
    ShowBalloHintForControl(lvTables, 'Select one or more tables for continue!');
    Abort;
  end;
end;

procedure TfrmNewDBObjectEditor.RefreshTreeView;
var
  lUnit: TFreedomUnitDescriptor;
begin
  if Assigned(FFreedomProjectDescriptor) then
  begin
    tvDBProject.Items.BeginUpdate;
    try
      tvDBProject.Items.Clear;
      for lUnit in FFreedomProjectDescriptor do
      begin
        TfrmNewUnit.RefreshUnitTreeView(tvDBProject, nil, lUnit);
      end;
    finally
      TfrmNewUnit.DoExpand(tvDBProject);
      tvDBProject.Items.EndUpdate;
    end;
  end;
end;

end.
