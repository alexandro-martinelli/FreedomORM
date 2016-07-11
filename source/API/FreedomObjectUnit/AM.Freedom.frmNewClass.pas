unit AM.Freedom.frmNewClass;

interface

uses
  System.IniFiles,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Data.Bind.ObjectScope,
  Data.Bind.Components,
  System.Rtti,
  System.Bindings.Outputs,
  Vcl.Bind.Editors,
  Data.Bind.EngExt,
  Vcl.Bind.DBEngExt,
  AM.Freedom.FreedomObjectDescriptor,
  AM.Freedom.frmNewProperty,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Helper.TStrings,
  System.Generics.Collections,
  Vcl.ImgList,
  AM.Freedom.frmBase,
  AM.Freedom.ObjectMapper.ConstraintMapper;

type
  TEditState = (esEdit, esNew);

  TfrmNewClass = class(TfrmBase)
    {$REGION 'Fields'}
    FlowPanel1: TFlowPanel;
    btnCancel: TButton;
    btnFinish: TButton;
    tvProperties: TTreeView;
    splProperties: TSplitter;
    pnlClass: TPanel;
    pgcSteps: TPageControl;
    lblClassName: TLabel;
    lblBaseClass: TLabel;
    edtClassName: TComboBox;
    cbxBaseClasses: TComboBox;
    lblDescricao: TLabel;
    btnNext: TButton;
    btnBack: TButton;
    lblPrimary: TLabel;
    edtPrimaryColumns: TComboBox;
    lblUnique: TLabel;
    lblPrimaryColumns: TLabel;
    lblUnique1Columns: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    edtPrimarySchemas: TComboBox;
    Label11: TLabel;
    lblAlias: TLabel;
    lblMapping: TLabel;
    chkMappedAt: TCheckBox;
    edtEntityName: TComboBox;
    edtEntityAlias: TComboBox;
    BindingsList1: TBindingsList;
    LinkControlToPropertyEnabled: TLinkControlToProperty;
    LinkControlToPropertyEnabled2: TLinkControlToProperty;
    LinkControlToPropertyEnabled3: TLinkControlToProperty;
    LinkControlToPropertyEnabled4: TLinkControlToProperty;
    lblSchemas: TLabel;
    tsClassBaseClass: TTabSheet;
    tsPrimary: TTabSheet;
    tsUniques: TTabSheet;
    tsMapping: TTabSheet;
    tsSchemas: TTabSheet;
    tsProperties: TTabSheet;
    edtUnique1Column: TComboBox;
    edtUnique1Schemas: TComboBox;
    edtUnique2Column: TComboBox;
    edtUnique2Schemas: TComboBox;
    edtUnique3Column: TComboBox;
    edtUnique3Schemas: TComboBox;
    edtUnique4Column: TComboBox;
    edtUnique4Schemas: TComboBox;
    edtClassSchema1: TComboBox;
    edtClassSchema2: TComboBox;
    edtClassSchema3: TComboBox;
    edtClassSchema4: TComboBox;
    btnAddProperty: TButton;
    btnExcProperty: TButton;
    rbDefSchema1: TRadioButton;
    rbDefSchema2: TRadioButton;
    rbDefSchema3: TRadioButton;
    rbDefSchema4: TRadioButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    lbl1: TLabel;
    lbl2: TLabel;
    edtUnique5Column: TComboBox;
    edtUnique5Schemas: TComboBox;
    Bevel4: TBevel;
    edtClassSchema5: TComboBox;
    rbDefSchema5: TRadioButton;
    bvlClassListSeparator: TBevel;
    lblClassList: TLabel;
    edtClassList: TComboBox;
    lblBaseClassList: TLabel;
    cbxBaseClassList: TComboBox;
    lblPropertyFieldNames: TLabel;
    btnOptions: TButton;
    tsForeigns: TTabSheet;
    Label1: TLabel;
    lvForeign: TListView;
    btnAddForeign: TButton;
    btnRemoveForeign: TButton;
    btnEditForeign: TButton;
    btnRemaneProperties: TButton;
    btnEditProperties: TButton;
    rdbEntity: TRadioButton;
    rdbCursor: TRadioButton;
    lblInheritsCursorClassName: TLabel;
    lblCursorClassName: TLabel;
    cbxCursorClassName: TComboBox;
    cbxInheritsClassCursor: TComboBox;
    {$ENDREGION}
    procedure btnAddPropertyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnExcPropertyClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOptionsClick(Sender: TObject);
    procedure tvPropertiesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure btnFinishClick(Sender: TObject);
    procedure btnAddForeignClick(Sender: TObject);
    procedure btnEditForeignClick(Sender: TObject);
    procedure btnRemoveForeignClick(Sender: TObject);
    procedure btnRemanePropertiesClick(Sender: TObject);
    procedure btnEditPropertiesClick(Sender: TObject);
    procedure chkMappedAtClick(Sender: TObject);
    procedure rdbEntityClick(Sender: TObject);
  strict private const
    cUnitClassBaseClass = 0;
    cMapping = 1;
    cPrimary = 2;
    cUniques = 3;
    cForeign = 4;
    cSchemas = 5;
    cProperties = 6;
  strict private
    FDragNode: TTreeNode;
    FEditState: TEditState;
    procedure ValidateStepOne;
    procedure ValidateIfMapping;
    procedure RefreshTreeview;
    procedure RefreshButtons;
    function GetNextPageIndex: Integer;
    procedure GetAllMappings;
    procedure InitializeTabSheets;
    procedure InitializeOptions;
    function ValidateObject: Boolean;
    function GetSelectForeign: TForeignMapper;
    class function GetNameFromDescriptor(pDescriptor: TFieldPropertyDescriptor; pIsField: Boolean): string;
    class function GetMapperColumnOptions(pColumn: TCustomColumnMapper): string;
    class function GetMapperIDOptions(pColumn: TCustomColumnMapper): string;
    class function GetMapperText(pColumn: TCustomColumnMapper): string;
    procedure VerifyMappingClassComponents;
    procedure SetTabStopsToMappingClassComponents;
    procedure VerifyCursorMappingClassComponents;
    procedure VerifyEntityMappingClassComponents;
  private
    FFreedomObjectDescriptor: TFreedomClassDescriptor;
    procedure SetEditState(pFreedomObjectDescriptor: TFreedomClassDescriptor);
    procedure FrefreshForeignView;
    function ExtractText(pList: TList<String>): String;
    procedure SetNewClassName(aNewClassName: String);
  protected
    procedure DoInitializeHistory; override;
    function CanAskForClose: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function CreateNewClass(pNewClassName: String = ''): TFreedomClassDescriptor;
    class procedure EditClass(lDescriptor: TFreedomClassDescriptor);
    property FreedomObjectDescriptor: TFreedomClassDescriptor read FFreedomObjectDescriptor;
    class procedure RefreshClassTreeView(pTreeView: TTreeView; pParentNode: TTreeNode; pFreedomObjectDescriptor: TFreedomClassDescriptor);
    class procedure DoExpand(pTreeView: TTreeView);
  end;

implementation

uses
  System.StrUtils,
  AM.Freedom.frmOptions,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.ObjectMapper.Schemas,
  System.UITypes,
  AM.Freedom.frmClassUnitName,
  AM.Freedom.frmNewForeign,
  AM.Freedom.Helper.ForeignOption, AM.Freedom.frmRenameProperty;

{$R *.dfm}

{ TNewFreedomObjectUnit }

procedure TfrmNewClass.btnAddForeignClick(Sender: TObject);
var
  lForeign: TForeignMapper;
begin
  inherited;
  lForeign := TfrmNewForeign.AddForeign;
  if (Assigned(lForeign)) then
  begin
    FFreedomObjectDescriptor.ObjectMapper.Foreigns.Add(lForeign);
    FrefreshForeignView;
  end;
end;

procedure TfrmNewClass.btnAddPropertyClick(Sender: TObject);
var
  lResult: TFieldPropertyDescriptor;
begin
  ValidateStepOne;
  FinalizeHistory;
  repeat
    lResult := TfrmNewProperty.FieldProperty(FFreedomObjectDescriptor);
    if Assigned(lResult) then
    begin
      if Assigned(lResult.FieldMapper) then
      begin
        if Assigned(lResult.FieldMapper) then
        begin
          FFreedomObjectDescriptor.ObjectMapper.Columns.Add(lResult.FieldMapper)
        end;
        FFreedomObjectDescriptor.FieldPropertyList.Add(lResult);
      end;
    end;
    RefreshTreeview;
  until lResult = nil;
  ReloadIniFile;
end;

procedure TfrmNewClass.btnBackClick(Sender: TObject);
begin
  pgcSteps.ActivePageIndex := pgcSteps.ActivePageIndex - 1;
  RefreshButtons;
end;

procedure TfrmNewClass.btnEditForeignClick(Sender: TObject);
var
  lForeign: TForeignMapper;
begin
  inherited;
  lForeign := GetSelectForeign;
  if (Assigned(lForeign)) then
  begin
    TfrmNewForeign.AlterForeign(lForeign);
    FrefreshForeignView;
  end;
end;

procedure TfrmNewClass.btnEditPropertiesClick(Sender: TObject);
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
          lFieldIndex := FFreedomObjectDescriptor.FieldPropertyList.IndexOf(lDescriptor);
          FFreedomObjectDescriptor.FieldPropertyList.Extract(lDescriptor);
          lNewProperty := TfrmNewProperty.EditFieldProperty(FFreedomObjectDescriptor, lDescriptor);
          if (not Assigned(lNewProperty)) then
          begin
            lNewProperty := lDescriptor;
          end
          else
          begin
            FreeAndNil(lDescriptor);
          end;
          FFreedomObjectDescriptor.FieldPropertyList.Insert(lFieldIndex, lNewProperty);
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

procedure TfrmNewClass.btnExcPropertyClick(Sender: TObject);
var
  lDescriptor: TFieldPropertyDescriptor;
  xIndex: Integer;
begin
  if tvProperties.Selected <> nil then
  begin
    for xIndex := 0 to tvProperties.SelectionCount - 1 do
    begin
      if (tvProperties.Selections[xIndex].Data <> nil) and TObject(tvProperties.Selections[xIndex].Data).InheritsFrom(TFieldPropertyDescriptor) then
      begin
        lDescriptor := TFieldPropertyDescriptor(tvProperties.Selections[xIndex].Data);
        if Assigned(lDescriptor) then
        begin
          FFreedomObjectDescriptor.FieldPropertyList.Extract(lDescriptor);
          if Assigned(lDescriptor.FieldMapper) then
          begin
            FFreedomObjectDescriptor.ObjectMapper.Columns.Extract(lDescriptor.FieldMapper);
            lDescriptor.FieldMapper.Free;
          end;
          lDescriptor.Free;
        end;
      end;
    end;
    RefreshTreeview;
  end;
end;

procedure TfrmNewClass.btnFinishClick(Sender: TObject);
begin
  if (ValidateObject) then
  begin
    ModalResult := mrOk;
  end;
end;

procedure TfrmNewClass.btnNextClick(Sender: TObject);
begin
  case pgcSteps.ActivePageIndex of
    cUnitClassBaseClass:
      ValidateStepOne;
    cMapping:
      ValidateIfMapping;
  end;
  pgcSteps.ActivePageIndex := GetNextPageIndex;
  RefreshButtons;
  if pgcSteps.ActivePageIndex = cProperties then
  begin
    GetAllMappings;
  end;
end;

procedure TfrmNewClass.btnOptionsClick(Sender: TObject);
begin
  TfrmOptions.ShowOptions;
  InitializeOptions;
end;

procedure TfrmNewClass.btnRemanePropertiesClick(Sender: TObject);
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

procedure TfrmNewClass.btnRemoveForeignClick(Sender: TObject);
var
  lForeign: TForeignMapper;
  lIndex: Integer;
begin
  for lIndex := lvForeign.Items.Count - 1 downto 0 do
  begin
    if (lvForeign.Items.Item[lIndex].Selected) then
    begin
      lForeign := TForeignMapper(lvForeign.Items.Item[lIndex].Data);
      FFreedomObjectDescriptor.ObjectMapper.Foreigns.Remove(lForeign);
      lvForeign.Items.Item[lIndex].Delete;
    end;
  end;
  FrefreshForeignView;
end;

function TfrmNewClass.CanAskForClose: Boolean;
begin
  Result := pgcSteps.ActivePageIndex > 0;
end;

procedure TfrmNewClass.chkMappedAtClick(Sender: TObject);
begin
  inherited;
  VerifyMappingClassComponents;
end;

constructor TfrmNewClass.Create(AOwner: TComponent);
begin
  inherited;
  FEditState := esNew;
  FFreedomObjectDescriptor := TFreedomClassDescriptor.Create;
  InitializeTabSheets;
  InitializeOptions;
end;

class function TfrmNewClass.CreateNewClass(pNewClassName: String): TFreedomClassDescriptor;
var
  lForm: TfrmNewClass;
begin
  lForm := TfrmNewClass.Create(nil);
  TfrmClassUnitName.InitializeClassHistory(Self);
  try
    lForm.SetNewClassName(pNewClassName);
    lForm.ShowModal;
    Result := lForm.FreedomObjectDescriptor;
  finally
    lForm.Free;
    TfrmClassUnitName.FinalizeClassHistory(Self);
  end;
end;

class procedure TfrmNewClass.DoExpand(pTreeView: TTreeView);
var
  lNode: TTreeNode;
begin
  for lNode in pTreeView.Items do
  begin
    if (not Assigned(lNode.Data)) or not TObject(lNode.Data).InheritsFrom(TFieldPropertyDescriptor) then
    begin
      lNode.Expand(False);
    end;
  end;
end;

procedure TfrmNewClass.DoInitializeHistory;
begin
  inherited;
  cbxBaseClasses.Text := IniFile.ReadString('Options', 'DefaultBaseClassName', 'TFreedomObject');
  cbxBaseClassList.Text := IniFile.ReadString('Options', 'DefaultListBaseClassName', 'TFreedomObjectList');
  if (cbxInheritsClassCursor.Items.IndexOf('TCustomObjectCursor') < 0) then
  begin
    cbxInheritsClassCursor.Items.Add('TCustomObjectCursor');
  end;
  if (cbxInheritsClassCursor.Items.IndexOf('TCustomDBObjectCursor') < 0) then
  begin
    cbxInheritsClassCursor.Items.Add('TCustomDBObjectCursor');
  end;
end;

class procedure TfrmNewClass.EditClass(lDescriptor: TFreedomClassDescriptor);
var
  lForm: TfrmNewClass;
begin
  lForm := TfrmNewClass.Create(nil);
  try
    lForm.SetEditState(lDescriptor);
    lForm.ShowModal;
  finally
    lForm.Free;
  end;
end;

function TfrmNewClass.ExtractText(pList: TList<String>): String;
var
  lString: String;
begin
  Result := '';
  for lString in pList do
  begin
    Result := Result + IfThen(Result <> '', ';') + lString;
  end;
end;

procedure TfrmNewClass.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if (ModalResult = mrCancel) and (FEditState = esNew) then
  begin
    FFreedomObjectDescriptor.Free;
    FFreedomObjectDescriptor := nil;
  end;
end;

procedure TfrmNewClass.FormShow(Sender: TObject);
begin
  if (pgcSteps.ActivePageIndex = cUnitClassBaseClass) then
  begin
    edtClassName.SetFocus;
  end
  else
  begin
    btnAddProperty.SetFocus;
  end;
end;

procedure TfrmNewClass.GetAllMappings;
var
  lColumnNames, lSchemaNames: TStrings;
  procedure GetPrimary;
  var
    lColumnList: TList<string>;
    lSchemaList: TList<string>;
  begin
    if Trim(edtPrimaryColumns.Text) <> '' then
    begin
      ExtractStrings([',', ';'], [' '], pWideChar(edtPrimaryColumns.Text), lColumnNames);
      ExtractStrings([',', ';'], [' '], pWideChar(edtPrimarySchemas.Text), lSchemaNames);
      lColumnList := lColumnNames.ToList;
      lSchemaList := lSchemaNames.ToList;
      try
        FFreedomObjectDescriptor.ObjectMapper.Primarys.Add(TPrimaryMapper.Create(lColumnList, lSchemaList))
      finally
        lColumnList.Free;
        lSchemaList.Free;
      end;
    end;
    lColumnNames.Clear;
    lSchemaNames.Clear;
  end;
  procedure GetUnique(pUniqueColumns: string; pUniqueSchemas: string);
  var
    lColumnList: TList<string>;
    lSchemaList: TList<string>;
  begin
    if Trim(pUniqueColumns) <> '' then
    begin
      ExtractStrings([',', ';'], [' '], pWideChar(pUniqueColumns), lColumnNames);
      ExtractStrings([',', ';'], [' '], pWideChar(pUniqueSchemas), lSchemaNames);
      lColumnList := lColumnNames.ToList;
      lSchemaList := lSchemaNames.ToList;
      try
        FFreedomObjectDescriptor.ObjectMapper.Uniques.Add(TUniqueMapper.Create(lColumnList, lSchemaList))
      finally
        lColumnList.Free;
        lSchemaList.Free;
      end;
    end;
    lColumnNames.Clear;
    lSchemaNames.Clear;
  end;
  procedure GetSchema(pSchemaName: string; pDefault: Boolean);
  begin
    if pSchemaName <> '' then
    begin
      FFreedomObjectDescriptor.ObjectMapper.Schemas.AddSchema(pSchemaName, pDefault)
    end;
  end;

begin
  if chkMappedAt.Checked then
  begin
    if (rdbEntity.Checked) then
    begin
      FFreedomObjectDescriptor.ObjectMapper.Name := edtEntityName.Text;
      FFreedomObjectDescriptor.ObjectMapper.Alias := edtEntityAlias.Text;
      lColumnNames := TStringList.Create;
      lSchemaNames := TStringList.Create;
      try
        GetPrimary;
        GetUnique(edtUnique1Column.Text, edtUnique1Schemas.Text);
        GetUnique(edtUnique2Column.Text, edtUnique2Schemas.Text);
        GetUnique(edtUnique3Column.Text, edtUnique3Schemas.Text);
        GetUnique(edtUnique4Column.Text, edtUnique4Schemas.Text);
        GetUnique(edtUnique5Column.Text, edtUnique5Schemas.Text);
      finally
        lColumnNames.Free;
        lSchemaNames.Free;
      end;
      GetSchema(edtClassSchema1.Text, rbDefSchema1.Checked);
      GetSchema(edtClassSchema2.Text, rbDefSchema2.Checked);
      GetSchema(edtClassSchema3.Text, rbDefSchema3.Checked);
      GetSchema(edtClassSchema4.Text, rbDefSchema4.Checked);
      GetSchema(edtClassSchema5.Text, rbDefSchema5.Checked);
      FFreedomObjectDescriptor.ObjectCursorClassName := '';
      FFreedomObjectDescriptor.ObjectCursorClassUnitName := '';
      FFreedomObjectDescriptor.InheritsObjectCursorClassName := '';
    end
    else
    begin
      FFreedomObjectDescriptor.ObjectCursorClassName := cbxCursorClassName.Text;
      FFreedomObjectDescriptor.InheritsObjectCursorClassName := cbxInheritsClassCursor.Text;
      if (FFreedomObjectDescriptor.InheritsObjectCursorClassName = '') then
      begin
        TfrmClassUnitName.InitializeClassHistory(Self.ClassType);
        try
          FFreedomObjectDescriptor.ObjectCursorClassUnitName := TfrmClassUnitName.GetUnitNameForClassName(FFreedomObjectDescriptor.ObjectCursorClassName);
        finally
          TfrmClassUnitName.FinalizeClassHistory(Self.ClassType);
        end;
      end
      else if not ContainsText('TCustomObjectCursor;TCustomDBObjectCursor', FFreedomObjectDescriptor.InheritsObjectCursorClassName) then
      begin
        TfrmClassUnitName.InitializeClassHistory(Self.ClassType);
        try
          FFreedomObjectDescriptor.ObjectCursorClassUnitName := TfrmClassUnitName.GetUnitNameForClassName(FFreedomObjectDescriptor.InheritsObjectCursorClassName);
        finally
          TfrmClassUnitName.FinalizeClassHistory(Self.ClassType);
        end;
      end;
    end;
  end
  else
  begin
    FFreedomObjectDescriptor.ObjectMapper.Name := '';
    FFreedomObjectDescriptor.ObjectMapper.Alias := '';
    FFreedomObjectDescriptor.ObjectMapper.Unmapped := True;
    FFreedomObjectDescriptor.ObjectMapper.Primarys.Clear;
    FFreedomObjectDescriptor.ObjectMapper.Uniques.Clear;
    FFreedomObjectDescriptor.ObjectMapper.Schemas.Clear;
    FFreedomObjectDescriptor.ObjectMapper.Foreigns.Clear;
    FFreedomObjectDescriptor.ObjectCursorClassName := '';
    FFreedomObjectDescriptor.InheritsObjectCursorClassName := '';
    FFreedomObjectDescriptor.ObjectCursorClassUnitName := '';
  end;
end;

function TfrmNewClass.GetNextPageIndex: Integer;
begin
  Result := pgcSteps.ActivePageIndex + 1;
  if (pgcSteps.ActivePageIndex = cMapping) and (not chkMappedAt.Checked or rdbCursor.Checked) then
  begin
    Result := cProperties;
  end;
end;

function TfrmNewClass.GetSelectForeign: TForeignMapper;
begin
  Result := nil;
  if (lvForeign.Selected <> nil) then
  begin
    Result := TForeignMapper(lvForeign.Selected.Data);
  end;
end;

procedure TfrmNewClass.RefreshButtons;
begin
  btnFinish.Enabled := pgcSteps.ActivePageIndex = cProperties;
  btnFinish.Default := btnFinish.Enabled;
  btnNext.Enabled := not btnFinish.Enabled;
  btnNext.Default := btnNext.Enabled;
  btnBack.Enabled := pgcSteps.ActivePageIndex <> cUnitClassBaseClass;
end;

class procedure TfrmNewClass.RefreshClassTreeView(pTreeView: TTreeView; pParentNode: TTreeNode; pFreedomObjectDescriptor: TFreedomClassDescriptor);
var
  lParentNode, lClassNode: TTreeNode;
  lName: string;
  lDescriptor: TFieldPropertyDescriptor;
  lFieldNode: TTreeNode;
  lSchema: TSchemaItem;
  lMappingNode: TTreeNode;
begin
  lClassNode := pTreeView.Items.AddChildObject(pParentNode, pFreedomObjectDescriptor.CompleteClassName, pFreedomObjectDescriptor);
  lClassNode.ImageIndex := 1;
  lClassNode.SelectedIndex := 1;
  if pFreedomObjectDescriptor.FieldPropertyList.Count > 0 then
  begin
    if pFreedomObjectDescriptor.ObjectMapper.Columns.Count > 0 then
    begin
      lParentNode := pTreeView.Items.AddChild(lClassNode, 'Fields');
      lParentNode.ImageIndex := 2;
      lParentNode.SelectedIndex := 2;
      for lDescriptor in pFreedomObjectDescriptor.FieldPropertyList do
      begin
        lName := GetNameFromDescriptor(lDescriptor, True);
        lFieldNode := pTreeView.Items.AddChildObject(lParentNode, lName, lDescriptor);
        lFieldNode.ImageIndex := 2;
        lFieldNode.SelectedIndex := 2;
        if (lDescriptor.FieldMapper <> nil) then
        begin
          lName := GetMapperText(lDescriptor.FieldMapper);
          lMappingNode := pTreeView.Items.AddChild(lFieldNode, lName);
          lMappingNode.ImageIndex := 4;
          lMappingNode.SelectedIndex := 4;
          lName := GetMapperColumnOptions(lDescriptor.FieldMapper);
          lMappingNode := pTreeView.Items.AddChild(lFieldNode, lName);
          lMappingNode.ImageIndex := 4;
          lMappingNode.SelectedIndex := 4;
          lName := GetMapperIDOptions(lDescriptor.FieldMapper);
          if lName <> '' then
          begin
            lMappingNode := pTreeView.Items.AddChild(lFieldNode, lName);
            lMappingNode.ImageIndex := 4;
            lMappingNode.SelectedIndex := 4;
          end;
          if lDescriptor.FieldMapper.Schemas.Count > 0 then
          begin
            for lSchema in lDescriptor.FieldMapper.Schemas do
            begin
              lName := 'Schema: ' + lSchema.Name;
              lMappingNode := pTreeView.Items.AddChild(lFieldNode, lName);
              lMappingNode.ImageIndex := 4;
              lMappingNode.SelectedIndex := 4;
            end;
          end;
        end;
      end;
    end;
    lParentNode := pTreeView.Items.AddChild(lClassNode, 'Properties');
    lParentNode.ImageIndex := 3;
    lParentNode.SelectedIndex := 3;
    for lDescriptor in pFreedomObjectDescriptor.FieldPropertyList do
    begin
      lName := GetNameFromDescriptor(lDescriptor, False);
      lFieldNode := pTreeView.Items.AddChildObject(lParentNode, lName, lDescriptor);
      lFieldNode.ImageIndex := 3;
      lFieldNode.SelectedIndex := 3;
    end;
  end;
end;

class function TfrmNewClass.GetNameFromDescriptor(pDescriptor: TFieldPropertyDescriptor; pIsField: Boolean): string;
var
  lColumn: TCustomColumnMapper;
begin
  lColumn := pDescriptor.PropertyMapper;
  Result := lColumn.Name;
  if (pDescriptor.FieldMapper.IsNullable) and (not pDescriptor.HideNullable or pIsField) then
  begin
    if pDescriptor.UseNullableTypes then
    begin
      Result := Format('%s: T%sNullable', [Result, lColumn.ColumnType.ToString]);
    end
    else
    begin
      Result := Format('%s: TNullable<%s>', [Result, lColumn.ColumnType.ToString]);
    end;
  end
  else
  begin
    Result := Format('%s: %s', [Result, lColumn.ColumnType.ToString]);
  end;
  case lColumn.ColumnType of
    ctyChar, ctyString:
      if (lColumn.Size > 0) then
      begin
        Result := Result + '(' + IntToStr(lColumn.Size) + ')';
      end;
    ctySingle, ctyDouble, ctyCurrency, ctyExtended:
      Result := Result + ifthen(lColumn.Size > 0, '(' + IntToStr(lColumn.Size) + ifthen(lColumn.Scale > 0, ', ' + IntToStr(lColumn.Scale)) + ')');
    ctyBlob:
      Result := lColumn.Name + ': TStream';
    ctyMemo:
      Result := lColumn.Name + ': TStrings';
    ctyXML:
      Result := lColumn.Name + ': TXML';
    ctyEnumerator:
      Result := lColumn.Name + ': ' + pDescriptor.TypeName;
    ctyDetail, ctyJoin:
      begin
        Result := lColumn.Name + ': ';
        if (pIsField) then
        begin
          Result := Result + pDescriptor.TypeName;
        end
        else
        begin
          Result := Result + pDescriptor.LazyClassName;
        end;
      end;
  end;
  if pIsField then
  begin
    Result := 'F' + Result;
  end;
end;

class function TfrmNewClass.GetMapperText(pColumn: TCustomColumnMapper): string;
var
  lChars: string;
  lIndex: Integer;
begin
  Result := pColumn.Name + ': ';
  if (pColumn.ColumnType = ctyBoolean) then
  begin
    if (TBooleanColumnMapper(pColumn).InternalColumnType <> ctyBoolean) then
    begin
      Result := Format('%s%s(%s,%s)', [Result, TBooleanColumnMapper(pColumn).InternalColumnType.ToString, TBooleanColumnMapper(pColumn).ValueTrue, TBooleanColumnMapper(pColumn).ValueFalse]);
    end
    else
    begin
      Result := Result + pColumn.ColumnType.ToString;
    end;
  end
  else if (pColumn.ColumnType = ctyEnumerator) then
  begin
    case TEnumerationColumnMapper(pColumn).EnumType of
      emByte:
        Result := Result + 'Byte';
      emChar:
        begin
          Result := Result + 'Char(';
          lChars := '';
          for lIndex := low(TEnumerationColumnMapper(pColumn).EnumCharOf) to high(TEnumerationColumnMapper(pColumn).EnumCharOf) do
          begin
            lChars := lChars + ifthen(lChars <> '', ',') + TEnumerationColumnMapper(pColumn).EnumCharOf[lIndex];
          end;
          Result := Result + lChars + ')';
        end;
    end;
  end
  else
  begin
    Result := Result + pColumn.ColumnType.ToString;
  end;
end;

class function TfrmNewClass.GetMapperColumnOptions(pColumn: TCustomColumnMapper): string;
begin
  Result := '';
  if Required in pColumn.ColumnOptions then
  begin
    Result := 'Required'
  end;
  if NoInsert in pColumn.ColumnOptions then
  begin
    Result := Result + ifthen(Result <> '', ', ') + 'NoInsert';
  end;
  if NoUpdate in pColumn.ColumnOptions then
  begin
    Result := Result + ifthen(Result <> '', ', ') + 'NoUpdate';
  end;
  if NoDelete in pColumn.ColumnOptions then
  begin
    Result := Result + ifthen(Result <> '', ', ') + 'NoDelete';
  end;
  Result := 'ColumnOptions: [' + Result + ']';
end;

class function TfrmNewClass.GetMapperIDOptions(pColumn: TCustomColumnMapper): string;
begin
  Result := '';
  if pColumn.IdOptions.IsId then
  begin
    if pColumn.IdOptions.IdOption = IDentity then
    begin
      Result := 'Identity'
    end
    else if pColumn.IdOptions.IdOption = Sequence then
    begin
      Result := 'Sequence (' + pColumn.IdOptions.SequenceName + ')';
    end
    else
    begin
      Result := Result + 'None';
    end;
    Result := 'IdOptions: ' + Result;
  end;
end;

procedure TfrmNewClass.RefreshTreeview;
begin
  tvProperties.Items.Clear;
  tvProperties.Items.BeginUpdate;
  try
    RefreshClassTreeView(tvProperties, nil, FFreedomObjectDescriptor);
  finally
    DoExpand(tvProperties);
    tvProperties.Items.EndUpdate;
  end;
end;

procedure TfrmNewClass.SetEditState(pFreedomObjectDescriptor: TFreedomClassDescriptor);
begin
  FFreedomObjectDescriptor := pFreedomObjectDescriptor;
  FEditState := esEdit;
  tsProperties.Show;
  RefreshButtons;
  edtClassName.Text := FFreedomObjectDescriptor.NewClassName;
  cbxBaseClasses.Text := FFreedomObjectDescriptor.NewBaseClass;
  edtClassList.Text := FFreedomObjectDescriptor.ListClassName;
  cbxBaseClassList.Text := FFreedomObjectDescriptor.BaseClassListName;
  edtEntityName.Text := FFreedomObjectDescriptor.ObjectMapper.Name;
  edtEntityAlias.Text := FFreedomObjectDescriptor.ObjectMapper.Alias;
end;

procedure TfrmNewClass.SetNewClassName(aNewClassName: String);
begin
  edtClassName.Text := aNewClassName;
  bvlClassListSeparator.Visible := aNewClassName = '';
  edtClassList.Visible := bvlClassListSeparator.Visible;
  cbxBaseClassList.Visible := bvlClassListSeparator.Visible;
  lblClassList.Visible := bvlClassListSeparator.Visible;
  lblBaseClassList.Visible := bvlClassListSeparator.Visible;
end;

procedure TfrmNewClass.SetTabStopsToMappingClassComponents;
begin
  rdbCursor.TabStop := True;
  rdbEntity.TabStop := True;
end;

procedure TfrmNewClass.tvPropertiesDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
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
          FFreedomObjectDescriptor.FieldPropertyList.Move(FFreedomObjectDescriptor.FieldPropertyList.IndexOf(lSourceDescriptor), FFreedomObjectDescriptor.FieldPropertyList.IndexOf(lDestDescriptor));
          FDragNode := nil;
          RefreshTreeview;
        end;
      end;
    end;
  end;
end;

procedure TfrmNewClass.FrefreshForeignView;
var
  lForeign: TForeignMapper;
  lItem: TListItem;
begin
  lvForeign.Items.Clear;
  for lForeign in FFreedomObjectDescriptor.ObjectMapper.Foreigns do
  begin
    lItem := lvForeign.Items.Add;
    lItem.Caption := ExtractText(lForeign.Columns);
    lItem.SubItems.Add(lForeign.ReferencesTo);
    lItem.SubItems.Add(ExtractText(lForeign.ReferencesColumns));
    lItem.SubItems.Add(lForeign.OnUpdate.ToDescription);
    lItem.SubItems.Add(lForeign.OnDelete.ToDescription);
    lItem.Data := lForeign;
  end;
end;

procedure TfrmNewClass.ValidateIfMapping;
begin
  if chkMappedAt.Checked then
  begin
    if (rdbEntity.Checked) then
    begin
      if Trim(edtEntityName.Text) = '' then
      begin
        ShowBalloHintForControl(edtEntityName, 'The name of mapping entity is required!');
        Abort;
      end;
    end
    else if (rdbCursor.Checked) then
    begin
      if Trim(cbxCursorClassName.Text) = '' then
      begin
        ShowBalloHintForControl(cbxCursorClassName, 'The name of mapping object cursor class is required!');
        Abort;
      end;
    end;
  end;
end;

function TfrmNewClass.ValidateObject: Boolean;
begin
  Result := True;
  if (FFreedomObjectDescriptor.FieldPropertyList.Count <= 0) then
  begin
    ShowBalloHintForControl(btnAddProperty, 'The object needs one or more Properties/Fields!');
    btnAddProperty.SetFocus;
    Result := False;
  end;
end;

procedure TfrmNewClass.ValidateStepOne;
begin
  if (Trim(edtClassName.Text) = '') then
  begin
    ShowBalloHintForControl(edtClassName, 'The name of class is required!');
    Abort;
  end;
  if (Trim(cbxBaseClasses.Text) = '') then
  begin
    ShowBalloHintForControl(cbxBaseClasses, 'The name of base class is required!');
    Abort;
  end;
  if (Trim(edtClassList.Text) <> '') and (Trim(cbxBaseClassList.Text) = '') then
  begin
    ShowBalloHintForControl(cbxBaseClassList, 'The name of base class list is required!');
    Abort;
  end;
  FFreedomObjectDescriptor.NewClassName := edtClassName.Text;
  FFreedomObjectDescriptor.NewBaseClass := cbxBaseClasses.Text;
  FFreedomObjectDescriptor.ListClassName := edtClassList.Text;
  FFreedomObjectDescriptor.BaseClassListName := cbxBaseClassList.Text;
  if (FFreedomObjectDescriptor.NewBaseClassUnit = '') then
  begin
    FFreedomObjectDescriptor.NewBaseClassUnit := ExtractClassUnit(cbxBaseClasses.Text);
  end;
  if (edtClassList.Text <> '') and (FFreedomObjectDescriptor.BaseClassListUnit = '') then
  begin
    FFreedomObjectDescriptor.BaseClassListUnit := ExtractClassUnit(cbxBaseClassList.Text);
  end;
  RefreshTreeview;
end;

procedure TfrmNewClass.VerifyCursorMappingClassComponents;
begin
  edtEntityName.Enabled := rdbEntity.Checked;
  edtEntityAlias.Enabled := rdbEntity.Checked;
  lblMapping.Enabled := rdbEntity.Checked;
  lblAlias.Enabled := rdbEntity.Checked;
end;

procedure TfrmNewClass.VerifyEntityMappingClassComponents;
begin
  cbxCursorClassName.Enabled := rdbCursor.Checked;
  cbxInheritsClassCursor.Enabled := rdbCursor.Checked;
  lblCursorClassName.Enabled := rdbCursor.Checked;
  lblInheritsCursorClassName.Enabled := rdbCursor.Checked;
end;

procedure TfrmNewClass.VerifyMappingClassComponents;
begin
  rdbEntity.Enabled := chkMappedAt.Checked;
  if (not rdbEntity.Enabled) then
  begin
    rdbEntity.Checked := False;
  end;
  rdbCursor.Enabled := chkMappedAt.Checked;
  if (not rdbCursor.Enabled) then
  begin
    rdbCursor.Checked := False;
  end;
  if (chkMappedAt.Checked) and not rdbEntity.Checked and not rdbCursor.Checked then
  begin
    rdbEntity.Checked := True;
  end;
  SetTabStopsToMappingClassComponents;
  VerifyEntityMappingClassComponents;
  VerifyCursorMappingClassComponents;
end;

procedure TfrmNewClass.InitializeOptions;
begin
  edtEntityName.CharCase := TEditCharCase(IniFile.ReadInteger('Options', 'MappingCharCase', 1));
  edtEntityAlias.CharCase := edtEntityName.CharCase;
  edtPrimaryColumns.CharCase := edtEntityName.CharCase;
  edtUnique1Column.CharCase := edtEntityName.CharCase;
  edtUnique2Column.CharCase := edtEntityName.CharCase;
  edtUnique3Column.CharCase := edtEntityName.CharCase;
  edtUnique4Column.CharCase := edtEntityName.CharCase;
  edtUnique5Column.CharCase := edtEntityName.CharCase;
  edtPrimarySchemas.CharCase := TEditCharCase(IniFile.ReadInteger('Options', 'SchemaCharCase', 2));
  edtUnique1Schemas.CharCase := edtPrimarySchemas.CharCase;
  edtUnique2Schemas.CharCase := edtPrimarySchemas.CharCase;
  edtUnique3Schemas.CharCase := edtPrimarySchemas.CharCase;
  edtUnique4Schemas.CharCase := edtPrimarySchemas.CharCase;
  edtUnique5Schemas.CharCase := edtPrimarySchemas.CharCase;
  edtClassSchema1.CharCase := edtPrimarySchemas.CharCase;
  edtClassSchema2.CharCase := edtPrimarySchemas.CharCase;
  edtClassSchema3.CharCase := edtPrimarySchemas.CharCase;
  edtClassSchema4.CharCase := edtPrimarySchemas.CharCase;
  edtClassSchema5.CharCase := edtPrimarySchemas.CharCase;
end;

procedure TfrmNewClass.InitializeTabSheets;
begin
  tsClassBaseClass.TabVisible := False;
  tsMapping.TabVisible := False;
  tsPrimary.TabVisible := False;
  tsUniques.TabVisible := False;
  tsSchemas.TabVisible := False;
  tsProperties.TabVisible := False;
  tsForeigns.TabVisible := False;
  tsClassBaseClass.Show;
end;

procedure TfrmNewClass.rdbEntityClick(Sender: TObject);
begin
  inherited;
  VerifyEntityMappingClassComponents;
  VerifyCursorMappingClassComponents;
  SetTabStopsToMappingClassComponents;
end;

end.
