unit AM.Freedom.frmNewDBProject;

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
  Vcl.ImgList,
  Vcl.Grids,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Data.Bind.EngExt,
  Vcl.Bind.DBEngExt,
  Data.Bind.Components,
  System.Rtti,
  System.Bindings.Outputs,
  Vcl.Bind.Editors,
  Vcl.Menus,
  AM.Freedom.frmBase,
  AM.Freedom.FreedomObjectDescriptor,
  AM.Freedom.TableSchemas,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.CustomDBPersistent,
  AM.Freedom.dmConnection,
  AM.Freedom.DBPersistent.FireDac,
  AM.Freedom.Helper.ColumnType;

type
  TfrmNewDBProject = class(TfrmBase)
    FlowPanel1: TFlowPanel;
    btnCancel: TButton;
    btnFinish: TButton;
    btnNext: TButton;
    btnBack: TButton;
    pgcSteps: TPageControl;
    tsTableNames: TTabSheet;
    tsClassBaseClass: TTabSheet;
    tsMappedClass: TTabSheet;
    lvSelectedTables: TListView;
    lblClassName: TLabel;
    lblBaseClass: TLabel;
    Bevel5: TBevel;
    lblClassList: TLabel;
    lblBaseClassList: TLabel;
    lblUnitName: TLabel;
    Bevel1: TBevel;
    edtUnitName: TComboBox;
    edtClassName: TComboBox;
    cbxBaseClassName: TComboBox;
    edtListClassName: TComboBox;
    cbxBaseListClassName: TComboBox;
    lblTableName: TLabel;
    lblTableName2: TLabel;
    BindingsList: TBindingsList;
    tvProperties: TTreeView;
    imgListProperty: TImageList;
    FlowPanel2: TFlowPanel;
    btnAddProperty: TButton;
    btnExcProperty: TButton;
    popmenuProperties: TPopupMenu;
    SetID1: TMenuItem;
    Identity1: TMenuItem;
    Sequence1: TMenuItem;
    None1: TMenuItem;
    btnOptions: TButton;
    btnRemaneProperties: TButton;
    btnEditProperties: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure btnAddPropertyClick(Sender: TObject);
    procedure btnExcPropertyClick(Sender: TObject);
    procedure tvPropertiesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure btnFinishClick(Sender: TObject);
    procedure popmenuPropertiesPopup(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure btnRemanePropertiesClick(Sender: TObject);
    procedure btnEditPropertiesClick(Sender: TObject);
  strict private const
    cImageIndexUndone = 7;
    cImageIndexDone = 8;
    cTableNames = 0;
    cClassBaseClass = 1;
    cMappedClass = 2;
  private
    FFreedomProjectDescriptor: TFreedomProjectDescriptor;
    FSelectedTables: TTableSchemaList;
    FCurrentItem: TTableSchema;
    FdmConexao: TdmConnection;
    FDBPersistent: TCustomDBPersistent;
    FFreedomClassDescriptor: TFreedomClassDescriptor;
    FFreedomUnitDescriptor: TFreedomUnitDescriptor;
    FDragNode: TTreeNode;

    procedure ValidateCurrentItem;
    procedure ValidateUnitClassListClassNames;

    procedure SetSelectedTables(const Value: TTableSchemaList);
    procedure CreateDBPersistent;
    procedure OrganizeTabSheets;
    function GetFirstUndoneCheckedItem: TTableSchema;
    procedure MarkCurrentItemDone;

    procedure GetClassDeclaration;
    procedure AssignFieldPropertyListFromClassDescriptor;
    function ConvertFieldToPropertyName(pFieldName: String; pRemoveUnderscore: Boolean = True): String;
    function CapitalizeStringToCamelCase(pStringName: String): String;

    procedure RefreshButtons;
    procedure RefreshTreeview;
  protected
    procedure DoInitializeHistory; override;
    function CanAskForClose: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function CreateProject(pSelectedTables: TTableSchemaList): TFreedomProjectDescriptor;
    property FreedomProjectDescriptor: TFreedomProjectDescriptor read FFreedomProjectDescriptor;
    property SelectedTables: TTableSchemaList read FSelectedTables write SetSelectedTables;
  end;

implementation

uses
  System.StrUtils,
  AM.Freedom.Persistent.FBPersistent,
  AM.Freedom.Persistent.MSSQLPersistent,
  AM.Freedom.Persistent.PGPersistent,
  AM.Freedom.IDBPersistent,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.frmNewClass,
  AM.Freedom.frmNewProperty,
  AM.Freedom.frmOptions, AM.Freedom.frmRenameProperty;

{$R *.dfm}

{ TfrmNewDBProject }

procedure TfrmNewDBProject.AssignFieldPropertyListFromClassDescriptor;
var
  lColumn: TCustomColumnMapper;
  lProperty: TCustomColumnMapper;
  lFieldPropertyDesc: TFieldPropertyDescriptor;
begin
  case FSelectedTables.Parameters.MappingCharCase of
    Upper: FFreedomClassDescriptor.ObjectMapper.Name := UpperCase(FFreedomClassDescriptor.ObjectMapper.Name);
    Lower: FFreedomClassDescriptor.ObjectMapper.Name := LowerCase(FFreedomClassDescriptor.ObjectMapper.Name);
  end;
  for lColumn in FFreedomClassDescriptor.ObjectMapper.Columns do
  begin
    case FSelectedTables.Parameters.MappingCharCase of
      Upper: lColumn.Name := UpperCase(lColumn.Name);
      Lower: lColumn.Name := LowerCase(lColumn.Name);
    end;
    lFieldPropertyDesc := TFieldPropertyDescriptor.Create;
    FFreedomClassDescriptor.FieldPropertyList.Add(lFieldPropertyDesc);
    lFieldPropertyDesc.FieldMapper := lColumn;
    lProperty := TCustomColumnMapper.Create;
    lFieldPropertyDesc.PropertyMapper := lProperty;
    lProperty.Name := ConvertFieldToPropertyName(lColumn.Name, FSelectedTables.Parameters.RemoveUnderscoreInFields);
    lProperty.ColumnType := lColumn.ColumnType;
    lProperty.Size := lColumn.Size;
    if lColumn.ColumnType = ctyBlob then
    begin
      lFieldPropertyDesc.TypeName := 'TStream';
    end
    else if lColumn.ColumnType = ctyMemo then
    begin
      lFieldPropertyDesc.TypeName := 'TStrings';
    end
    else if lColumn.ColumnType = ctyXML then
    begin
      lFieldPropertyDesc.TypeName := 'TXML';
    end
    else
    begin
      lFieldPropertyDesc.TypeName := lColumn.ColumnType.ToString;
    end;
  end;
end;

procedure TfrmNewDBProject.btnAddPropertyClick(Sender: TObject);
var
  lResult: TFieldPropertyDescriptor;
begin
  FinalizeHistory;
  repeat
    lResult := TfrmNewProperty.FieldProperty(FFreedomClassDescriptor);
    if Assigned(lResult) then
    begin
      if Assigned(lResult.FieldMapper) then
      begin
        if Assigned(lResult.FieldMapper) then
        begin
          FFreedomClassDescriptor.ObjectMapper.Columns.Add(lResult.FieldMapper)
        end;
        FFreedomClassDescriptor.FieldPropertyList.Add(lResult);
      end;
    end;
  until lResult = nil;
  RefreshTreeview;
  ReloadIniFile;
end;

procedure TfrmNewDBProject.btnBackClick(Sender: TObject);
begin
  inherited;
  pgcSteps.ActivePageIndex := pgcSteps.ActivePageIndex - 1;
  RefreshButtons;
end;

procedure TfrmNewDBProject.btnEditPropertiesClick(Sender: TObject);
var
  lDescriptor, lNewProperty: TFieldPropertyDescriptor;
  lIndex: Integer;
  lFieldIndex: Integer;
begin
  if tvProperties.Selected <> nil then
  begin
    FinalizeHistory;
    for lIndex := 0 to tvProperties.SelectionCount - 1 do
    begin
      if (tvProperties.Selections[lIndex].Data <> nil) and
         TObject(tvProperties.Selections[lIndex].Data).InheritsFrom(TFieldPropertyDescriptor) then
      begin
        lDescriptor := TFieldPropertyDescriptor(tvProperties.Selections[lIndex].Data);
        if Assigned(lDescriptor) then
        begin
          lFieldIndex := FFreedomClassDescriptor.FieldPropertyList.IndexOf(lDescriptor);
          FFreedomClassDescriptor.FieldPropertyList.Extract(lDescriptor);
          lNewProperty := TfrmNewProperty.EditFieldProperty(FFreedomClassDescriptor, lDescriptor);
          if (not Assigned(lNewProperty)) then
          begin
            lNewProperty := lDescriptor;
          end
          else
          begin
            FreeAndNil(lDescriptor);
          end;
          FFreedomClassDescriptor.FieldPropertyList.Insert(lFieldIndex, lNewProperty);
          if (lNewProperty = lDescriptor) then
          begin
            Break;
          end;
        end;
      end;
    end;
    RefreshTreeview;
    ReloadIniFile;
  end;
end;

procedure TfrmNewDBProject.btnExcPropertyClick(Sender: TObject);
var
  lDescriptor: TFieldPropertyDescriptor;
  lIndex: Integer;
begin
  if tvProperties.Selected <> nil then
  begin
    for lIndex := 0 to tvProperties.SelectionCount - 1 do
    begin
      if (tvProperties.Selections[lIndex].Data <> nil) and TObject(tvProperties.Selections[lIndex].Data).InheritsFrom(TFieldPropertyDescriptor) then
      begin
        lDescriptor := TFieldPropertyDescriptor(tvProperties.Selections[lIndex].Data);
        if Assigned(lDescriptor) then
        begin
          FFreedomClassDescriptor.FieldPropertyList.Extract(lDescriptor);
          if Assigned(lDescriptor.FieldMapper) then
          begin
            FFreedomClassDescriptor.ObjectMapper.Columns.Extract(lDescriptor.FieldMapper);
            lDescriptor.FieldMapper.Free;
          end;
          lDescriptor.Free;
        end;
      end;
    end;
    RefreshTreeview;
  end;
end;

procedure TfrmNewDBProject.btnFinishClick(Sender: TObject);
var
  lFreedomUnit: TFreedomUnitDescriptor;
begin
  if (FCurrentItem <> nil) then
  begin
    if (FFreedomClassDescriptor.FieldPropertyList.Count <= 0) then
    begin
      ShowBalloHintForControl(btnAddProperty, 'One or more propertyies/Fields is required!');
      Abort;
    end;
    lFreedomUnit := FFreedomProjectDescriptor.FindUnit(FFreedomUnitDescriptor.NewUnitName);
    if not Assigned(lFreedomUnit) then
    begin
      FFreedomProjectDescriptor.Add(FFreedomUnitDescriptor);
      FFreedomUnitDescriptor := nil;
      FFreedomClassDescriptor := nil;
    end
    else
    begin
      FFreedomUnitDescriptor.Extract(FFreedomClassDescriptor);
      lFreedomUnit.Add(FFreedomClassDescriptor);
      FreeAndNil(FFreedomUnitDescriptor);
      FFreedomClassDescriptor := nil;
    end;
    MarkCurrentItemDone;
    pgcSteps.ActivePageIndex := cTableNames;
    FCurrentItem := GetFirstUndoneCheckedItem;
    RefreshButtons;
    FinalizeHistory;
    ReloadIniFile;
  end
  else
  begin
    ModalResult := mrOK;
  end;
end;

procedure TfrmNewDBProject.btnNextClick(Sender: TObject);
begin
  case pgcSteps.ActivePageIndex of
    cTableNames:
      ValidateCurrentItem;
    cClassBaseClass:
      begin
        ValidateUnitClassListClassNames;
        Screen.Cursor := crHourGlass;
        try
          GetClassDeclaration;
        finally
          Screen.Cursor := crDefault;
        end;
      end;
  end;
  pgcSteps.ActivePageIndex := pgcSteps.ActivePageIndex + 1;
  RefreshButtons;
end;

procedure TfrmNewDBProject.btnOptionsClick(Sender: TObject);
begin
  FinalizeHistory;
  TfrmOptions.ShowOptions;
  ReloadIniFile;
end;

procedure TfrmNewDBProject.btnRemanePropertiesClick(Sender: TObject);
var
  lDescriptor: TFieldPropertyDescriptor;
  lIndex: Integer;
begin
  if tvProperties.Selected <> nil then
  begin
    for lIndex := 0 to tvProperties.SelectionCount - 1 do
    begin
      if (tvProperties.Selections[lIndex].Data <> nil) and TObject(tvProperties.Selections[lIndex].Data).InheritsFrom(TFieldPropertyDescriptor) then
      begin
        lDescriptor := TFieldPropertyDescriptor(tvProperties.Selections[lIndex].Data);
        if Assigned(lDescriptor) then
        begin
          TfrmRenameProperty.Rename(lDescriptor);
        end;
      end;
    end;
    RefreshTreeview;
  end;
end;

function TfrmNewDBProject.CanAskForClose: Boolean;
begin
  Result := True;
end;

function TfrmNewDBProject.CapitalizeStringToCamelCase(pStringName: String): String;
var
  lIndex: Integer;
begin
  Result := '';
  for lIndex := 1 to Length(pStringName) do
  begin
    if lIndex = 1 then
    begin
      Result := Result + AnsiUpperCase(pStringName[lIndex]);
    end
    else
    begin
      Result := Result + AnsiLowerCase(pStringName[lIndex]);
    end;
  end;
end;

function TfrmNewDBProject.ConvertFieldToPropertyName(pFieldName: String; pRemoveUnderscore: Boolean): String;
var
  lStringNames: TStrings;
  lIndex: Integer;
begin
  Result := '';
  lStringNames := TStringList.Create;
  try
    ExtractStrings(['_', '-', '.'], [' '], PWideChar(pFieldName), lStringNames);
    for lIndex := 0 to lStringNames.Count - 1 do
    begin
      case FSelectedTables.Parameters.CapitalizationMode of
        cmCamelCase: lStringNames.Strings[lIndex] := CapitalizeStringToCamelCase(lStringNames.Strings[lIndex]);
        cmLowerCase: lStringNames.Strings[lIndex] := LowerCase(lStringNames.Strings[lIndex]);
        cmUpperCase: lStringNames.Strings[lIndex] := UpperCase(lStringNames.Strings[lIndex]);
      end;
    end;
    for lIndex := 0 to lStringNames.Count - 1 do
    begin
      Result := Result +
          ifthen((lIndex = lStringNames.Count - 1) or (Result = '') or
                 (pRemoveUnderscore and ContainsText(pFieldName, '_')), '', '_') +
          lStringNames.Strings[lIndex];
    end;
  finally
    lStringNames.Free;
  end;
end;

constructor TfrmNewDBProject.Create(AOwner: TComponent);
begin
  inherited;
  FFreedomProjectDescriptor := TFreedomProjectDescriptor.Create(True);
  OrganizeTabSheets;
end;

procedure TfrmNewDBProject.CreateDBPersistent;
var
  lConnector: TFireDacDBConnector;
  lIndex: Integer;
begin
  FdmConexao := TdmConnection.Create(Self);
  for lIndex := FSelectedTables.Parameters.ConnectionParams.Count - 1 Downto 0 do
  begin
    if (Trim(FSelectedTables.Parameters.ConnectionParams.ValueFromIndex[lIndex]) = '') then
    begin
      FSelectedTables.Parameters.ConnectionParams.Delete(lIndex);
    end;
  end;
  FdmConexao.FDConnection.Params.Assign(FSelectedTables.Parameters.ConnectionParams);
  case FSelectedTables.Parameters.DataBaseType of
    Firebird: FdmConexao.FDConnection.DriverName := 'FB';
    SQLServer: FdmConexao.FDConnection.DriverName := 'MSSQL';
    PostGree: FdmConexao.FDConnection.DriverName := 'PG';
  end;
  lConnector := TFireDacDBConnector.Create(FdmConexao.FDConnection, False);
  case FSelectedTables.Parameters.DataBaseType of
    Firebird: FDBPersistent := TFBPersistent.Create(lConnector, True);
    SQLServer: FDBPersistent := TMSSQLPersistent.Create(lConnector, True);
    PostGree: FDBPersistent := TPGPersistent.Create(lConnector, True);
  end;
end;

class function TfrmNewDBProject.CreateProject(pSelectedTables: TTableSchemaList): TFreedomProjectDescriptor;
var
  lfrmNewDBProject: TfrmNewDBProject;
begin
  lfrmNewDBProject := TfrmNewDBProject.Create(nil);
  try
    lfrmNewDBProject.SelectedTables := pSelectedTables;
    lfrmNewDBProject.ShowModal;
    Result := lfrmNewDBProject.FreedomProjectDescriptor;
  finally
    lfrmNewDBProject.Free;
  end;
end;

destructor TfrmNewDBProject.Destroy;
begin
  FSelectedTables.Free;
  FreeAndnil(FDBPersistent);
  FreeAndnil(FdmConexao);
  inherited;
end;

procedure TfrmNewDBProject.DoInitializeHistory;
begin
  inherited;
  cbxBaseClassName.Text := IniFile.ReadString('Options', 'DefaultBaseClassName', 'TFreedomObject');
  cbxBaseListClassName.Text := IniFile.ReadString('Options', 'DefaultListBaseClassName', 'TFreedomObjectList');
end;

procedure TfrmNewDBProject.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if (ModalResult = mrCancel) then
  begin
    FreeAndnil(FFreedomUnitDescriptor);
    FreeAndnil(FFreedomProjectDescriptor);
  end;
end;

procedure TfrmNewDBProject.GetClassDeclaration;
var
  lDBPersistent: IDBPersistent;
begin
  Supports(FDBPersistent, IDBPersistent, lDBPersistent);
  FFreedomClassDescriptor := nil;
  FreeAndnil(FFreedomUnitDescriptor);
  FFreedomUnitDescriptor := TFreedomUnitDescriptor.Create;
  FFreedomUnitDescriptor.NewUnitName := edtUnitName.Text;
  FFreedomClassDescriptor := TFreedomClassDescriptor.Create;
  FFreedomClassDescriptor.NewClassName := edtClassName.Text;
  FFreedomClassDescriptor.NewBaseClass := cbxBaseClassName.Text;
  FFreedomClassDescriptor.NewBaseClassUnit := ExtractClassUnit(cbxBaseClassName.Text);
  FFreedomClassDescriptor.ListClassName := edtListClassName.Text;
  if (FFreedomClassDescriptor.ListClassName <> '') then
  begin
    FFreedomClassDescriptor.BaseClassListName := cbxBaseListClassName.Text;
    FFreedomClassDescriptor.BaseClassListUnit := ExtractClassUnit(cbxBaseListClassName.Text);
  end;
  FFreedomClassDescriptor.ObjectMapper := lDBPersistent.ExtractDBDDL(FCurrentItem.TableName, FCurrentItem.TableSchema);
  AssignFieldPropertyListFromClassDescriptor;
  FFreedomUnitDescriptor.Add(FFreedomClassDescriptor);
  RefreshTreeview;
end;

function TfrmNewDBProject.GetFirstUndoneCheckedItem: TTableSchema;
var
  lIndex: Integer;
begin
  Result := nil;
  for lIndex := 0 to lvSelectedTables.Items.Count - 1 do
  begin
    if (lvSelectedTables.Items[lIndex].Checked) then
    begin
      if (lvSelectedTables.Items[lIndex].SubItemImages[1] = cImageIndexUndone) then
      begin
        Result := FSelectedTables.FindTableSchema(lvSelectedTables.Items[lIndex].Caption, lvSelectedTables.Items[lIndex].SubItems.Strings[0]);
        Break;
      end;
    end;
  end;
end;

procedure TfrmNewDBProject.MarkCurrentItemDone;
var
  lIndex: Integer;
begin
  for lIndex := 0 to lvSelectedTables.Items.Count - 1 do
  begin
    if (lvSelectedTables.Items[lIndex].Caption = FCurrentItem.TableName) and
       (lvSelectedTables.Items[lIndex].SubItems.Strings[0] = FCurrentItem.TableSchema) then
    begin
      lvSelectedTables.Items[lIndex].SubItemImages[1] := cImageIndexDone;
      Break;
    end;
  end;
end;

procedure TfrmNewDBProject.OrganizeTabSheets;
begin
  tsClassBaseClass.TabVisible := False;
  tsTableNames.TabVisible := False;
  tsMappedClass.TabVisible := False;
  tsTableNames.Show;
  RefreshButtons;
end;

procedure TfrmNewDBProject.popmenuPropertiesPopup(Sender: TObject);
begin
  inherited;
//  lNode := tvProperties.GetNodeAt(X, Y);
end;

procedure TfrmNewDBProject.RefreshButtons;
begin
  btnFinish.Enabled := (pgcSteps.ActivePageIndex = cMappedClass) or (FCurrentItem = nil);

  btnNext.Enabled := not btnFinish.Enabled;
  btnNext.Default := btnNext.Enabled;

  btnBack.Enabled := pgcSteps.ActivePageIndex <> cTableNames;

  btnAddProperty.Default := pgcSteps.ActivePageIndex = cMappedClass;
end;

procedure TfrmNewDBProject.SetSelectedTables(const Value: TTableSchemaList);
var
  lItem: TListItem;
  lIndex: Integer;
begin
  FSelectedTables := Value;
  lvSelectedTables.Items.Clear;
  for lIndex := 0 to FSelectedTables.Count - 1 do
  begin
    lItem := lvSelectedTables.Items.Add;
    lItem.Caption := FSelectedTables.Items[lIndex].TableName;
    lItem.ImageIndex := 1;
    lItem.SubItems.Add(FSelectedTables.Items[lIndex].TableSchema);
    lItem.SubItems.Add('');
    lItem.SubItemImages[1] := cImageIndexUndone;
  end;
  CreateDBPersistent;
  FCurrentItem := GetFirstUndoneCheckedItem;
  RefreshButtons;
end;

procedure TfrmNewDBProject.tvPropertiesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  lNode: TTreeNode;
  lDestDescriptor, lSourceDescriptor: TFieldPropertyDescriptor;
begin
  inherited;
  lNode := tvProperties.GetNodeAt(X, Y);
  Accept := Assigned(lNode);
  if (Accept) then
  begin
    Accept := Assigned(lNode.Data) and TObject(lNode.Data).InheritsFrom(TFieldPropertyDescriptor);
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
          lDestDescriptor := TFieldPropertyDescriptor(lNode.Data);
          lSourceDescriptor := TFieldPropertyDescriptor(FDragNode.Data);
          FFreedomClassDescriptor.FieldPropertyList.Move(FFreedomClassDescriptor.FieldPropertyList.IndexOf(lSourceDescriptor),
              FFreedomClassDescriptor.FieldPropertyList.IndexOf(lDestDescriptor));
          FDragNode := nil;
          RefreshTreeView;
        end;
      end;
    end;
  end;
end;

procedure TfrmNewDBProject.ValidateCurrentItem;
begin
  lblTableName.Caption := Format('Table Name: %s %s', [FCurrentItem.TableName,
      ifthen(FCurrentItem.TableSchema <> '', '- ' + FCurrentItem.TableSchema)]);
  lblTableName2.Caption := lblTableName.Caption;
  edtClassName.Text := Format('T%s%s', [IniFile.ReadString('Options', 'ClassPrefix', ''),
      ConvertFieldToPropertyName(FCurrentItem.TableName,
      FSelectedTables.Parameters.RemoveUnderscoreInClass)]);
  if edtListClassName.Text <> '' then
  begin
    edtListClassName.Text := Format('T%sLista%s', [IniFile.ReadString('Options', 'ClassPrefix', ''),
      ConvertFieldToPropertyName(FCurrentItem.TableName, FSelectedTables.Parameters.RemoveUnderscoreInClass)]);
  end;
end;

procedure TfrmNewDBProject.ValidateUnitClassListClassNames;
begin
  if (Trim(edtUnitName.Text) = '') then
  begin
    ShowBalloHintForControl(edtUnitName, 'Please especify the name of unit!');
    Abort;
  end;
  if (Trim(edtClassName.Text) = '') then
  begin
    ShowBalloHintForControl(edtClassName, 'Please especify the name of class for this table!');
    Abort;
  end;
  if (Trim(cbxBaseClassName.Text) = '') then
  begin
    ShowBalloHintForControl(cbxBaseClassName, 'Please especify the name of base class for this table!');
    Abort;
  end;
  if (Trim(edtListClassName.Text) <> '') then
  begin
    if (Trim(cbxBaseListClassName.Text) = '') then
    begin
      ShowBalloHintForControl(cbxBaseListClassName, 'Please especify the name of base class list for this table!');
      Abort;
    end;
  end;
end;

procedure TfrmNewDBProject.RefreshTreeview;
begin
  tvProperties.Items.Clear;
  tvProperties.Items.BeginUpdate;
  try
    TfrmNewClass.RefreshClassTreeView(tvProperties, nil, FFreedomClassDescriptor);
  finally
    TfrmNewClass.DoExpand(tvProperties);
    tvProperties.Items.EndUpdate;
  end;
end;

end.
