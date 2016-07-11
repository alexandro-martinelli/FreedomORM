unit AM.Freedom.frmNewFreedomObjectUnit;

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
  AM.Freedom.frmBaseObjectUnit;

type
  TNewFreedomObjectUnit = class(TfrmBaseObjectUnit)
    {$REGION 'Fields'}
    FlowPanel1: TFlowPanel;
    btnCancel: TButton;
    btnFinish: TButton;
    tvProperties: TTreeView;
    splProperties: TSplitter;
    pnlClass: TPanel;
    pgcSteps: TPageControl;
    lblUnitName: TLabel;
    lblClassName: TLabel;
    lblBaseClass: TLabel;
    edtUnitName: TComboBox;
    edtClassName: TComboBox;
    cbxBaseClasses: TComboBox;
    lblDescricao: TLabel;
    btnNext: TButton;
    btnBack: TButton;
    lblPrimary: TLabel;
    edtPrimaryColumns: TComboBox;
    lblUnique: TLabel;
    lblPrimaryCOlumns: TLabel;
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
    tsUnitClassBaseClass: TTabSheet;
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
    Bevel5: TBevel;
    lblClassList: TLabel;
    edtClassList: TComboBox;
    lblBaseClassList: TLabel;
    cbxBaseClassList: TComboBox;
    lblPropertyFieldNames: TLabel;
    btnOptions: TButton;
    {$ENDREGION}
    procedure btnAddPropertyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnExcPropertyClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOptionsClick(Sender: TObject);
  strict private
  const
    cUnitClassBaseClass = 0;
    cMapping = 1;
    cPrimary = 2;
    cUniques = 3;
    cSchemas = 4;
    cProperties = 5;
  strict private
    FFreedomObjectDescriptor: TFreedomObjectDescriptor;
    procedure ValidateStepOne;
    procedure ValidateIfMapping;
    procedure RefreshTreeview;
    function GetNameFromDescriptor(pDescriptor: TFieldPropertyDescriptor; pIsField: Boolean): string;
    function GetMapperColumnOptions(pColumn: TCustomCOlumnMapper): string;
    function GetMapperIDOptions(pColumn: TCustomCOlumnMapper): string;
    function GetMapperText(pColumn: TCustomColumnMapper): string;
    procedure RefreshButtons;
    function GetNextPageIndex: Integer;
    procedure GetAllMappings;
    procedure InitializeTabSheets;
    procedure InitializeOptions;
    procedure InitializeHistory;
    procedure FinalizeHistory;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function CreateNewUnit: TFreedomObjectDescriptor;
    property FreedomObjectDescriptor: TFreedomObjectDescriptor read FFreedomObjectDescriptor;
  end;

implementation

uses
  System.StrUtils,
  AM.Freedom.frmObjectUnitOptions,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.ObjectMapper.Schemas,
  AM.Freedom.ObjectMapper.ConstraintMapper,
  System.UITypes;

{$R *.dfm}

{ TNewFreedomObjectUnit }

procedure TNewFreedomObjectUnit.btnAddPropertyClick(Sender: TObject);
var
  lResult: TFieldPropertyDescriptor;
begin
  ValidateStepOne;
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
        RefreshTreeview;
      end;
    end;
  until lResult = nil;
end;

procedure TNewFreedomObjectUnit.btnBackClick(Sender: TObject);
begin
  pgcSteps.ActivePageIndex := pgcSteps.ActivePageIndex - 1;
  RefreshButtons;
end;

procedure TNewFreedomObjectUnit.btnExcPropertyClick(Sender: TObject);
var
  lDescriptor: TFieldPropertyDescriptor;
begin
  if tvProperties.Selected <> nil then
  begin
    lDescriptor := TFieldPropertyDescriptor(tvProperties.Selected.Data);
    if Assigned(lDescriptor) then
    begin
      FFreedomObjectDescriptor.FieldPropertyList.Extract(lDescriptor);
      if Assigned(lDescriptor.FieldMapper) then
      begin
        FFreedomObjectDescriptor.ObjectMapper.Columns.Extract(lDescriptor.FieldMapper);
        lDescriptor.FieldMapper.Free;
      end;
      lDescriptor.Free;
      RefreshTreeview;
    end;
  end;
end;

procedure TNewFreedomObjectUnit.btnNextClick(Sender: TObject);
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

procedure TNewFreedomObjectUnit.btnOptionsClick(Sender: TObject);
begin
  TfrmObjectUnitOptions.ShowOptions;
  InitializeOptions;
end;

constructor TNewFreedomObjectUnit.Create(AOwner: TComponent);
begin
  inherited;
  FFreedomObjectDescriptor := TFreedomObjectDescriptor.Create;
  InitializeTabSheets;
  InitializeHistory;
  InitializeOptions;
end;

class function TNewFreedomObjectUnit.CreateNewUnit: TFreedomObjectDescriptor;
var
  lForm: TNewFreedomObjectUnit;
begin
  lForm := TNewFreedomObjectUnit.Create(nil);
  Result := nil;
  try
    if lForm.ShowModal = mrOK then
    begin
      Result := lForm.FreedomObjectDescriptor;
    end;
  finally
    lForm.Free;
  end;
end;

destructor TNewFreedomObjectUnit.Destroy;
begin
  FinalizeHistory;
  inherited;
end;

procedure TNewFreedomObjectUnit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult = mrCancel then
  begin
    FFreedomObjectDescriptor.Free;
    FFreedomObjectDescriptor := nil;
  end;
end;

procedure TNewFreedomObjectUnit.FormShow(Sender: TObject);
begin
  edtUnitName.SetFocus;
end;

procedure TNewFreedomObjectUnit.GetAllMappings;
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
  procedure GetUnique(pUniqueColumns: String; pUniqueSchemas: String);
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
  procedure GetSchema(pSchemaName: String; pDefault: Boolean);
  begin
    if pSchemaName <> '' then
    begin
      FFreedomObjectDescriptor.ObjectMapper.Schemas.AddSchema(pSchemaName, pDefault)
    end;
  end;
begin
  if chkMappedAt.Checked then
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
  end
  else
  begin
    FFreedomObjectDescriptor.ObjectMapper.Name := '';
    FFreedomObjectDescriptor.ObjectMapper.Alias := '';
    FFreedomObjectDescriptor.ObjectMapper.Unmapped := True;
    FFreedomObjectDescriptor.ObjectMapper.Primarys.Clear;
    FFreedomObjectDescriptor.ObjectMapper.Uniques.Clear;
    FFreedomObjectDescriptor.ObjectMapper.Schemas.Clear;
  end;
end;

function TNewFreedomObjectUnit.GetNextPageIndex: Integer;
begin
  Result := pgcSteps.ActivePageIndex + 1;
  if (pgcSteps.ActivePageIndex = cMapping) and not chkMappedAt.Checked then
  begin
    Result := cProperties;
  end;
end;

procedure TNewFreedomObjectUnit.RefreshButtons;
begin
  btnFinish.Enabled := pgcSteps.ActivePageIndex = cProperties;
  btnFinish.Default := btnFinish.Enabled;
  btnNext.Enabled := not btnFinish.Enabled;
  btnNext.Default := btnNext.Enabled;
  btnBack.Enabled := pgcSteps.ActivePageIndex <> cUnitClassBaseClass;
end;


function TNewFreedomObjectUnit.GetNameFromDescriptor(pDescriptor: TFieldPropertyDescriptor; pIsField: Boolean): string;
var
  lColumn: TCustomColumnMapper;
begin
  lColumn := pDescriptor.PropertyMapper;
  Result := lColumn.Name + ':' + lColumn.ColumnType.ToString;
  case lColumn.ColumnType of
    ctyChar, ctyString:
      Result := Result + '(' + IntToStr(lColumn.Size) + ')';
    ctySingle, ctyDouble, ctyCurrency, ctyExtended:
      Result := Result + ifthen(lColumn.Size > 0, '(' + IntToStr(lColumn.Size) + ifthen(lColumn.Scale > 0, ', ' + IntToStr(lColumn.Scale)) + ')');
    ctyBlob:
      Result := lColumn.Name + ': TStream';
    ctyMemo:
      Result := lColumn.Name + ': TStrings';
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

function TNewFreedomObjectUnit.GetMapperText(pColumn: TCustomColumnMapper): string;
var
  lChars: string;
  lIndex: Integer;
begin
  Result := pColumn.Name + ': ';
  if (pColumn.ColumnType = ctyBoolean) then
  begin
    if (TBooleanCOlumnMapper(pColumn).InternalColumnType <> ctyBoolean) then
    begin
      Result := Format('%s%s(%s,%s)', [Result, TBooleanCOlumnMapper(pColumn).InternalColumnType.ToString, TBooleanCOlumnMapper(pColumn).ValueTrue, TBooleanCOlumnMapper(pColumn).ValueFalse]);
    end
    else
    begin
      Result := Result + pColumn.ColumnType.ToString;
    end;
  end
  else
    if (pColumn.ColumnType = ctyEnumerator) then
    begin
      case TEnumerationColumnMapper(pColumn).EnumType of
        emByte:
          result := Result + 'Byte';
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

function TNewFreedomObjectUnit.GetMapperColumnOptions(pColumn: TCustomCOlumnMapper): string;
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

function TNewFreedomObjectUnit.GetMapperIDOptions(pColumn: TCustomCOlumnMapper): string;
begin
  Result := '';
  if pColumn.IdOptions.IsId then
  begin
    if pColumn.IdOptions.IdOption = IDentity then
    begin
      Result := 'Identity'
    end
    else
      if pColumn.IdOptions.IdOption = Sequence then
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

procedure TNewFreedomObjectUnit.RefreshTreeview;
var
  lParentNode, lClassNode: TTreeNode;
  lName: string;
  lDescriptor: TFieldPropertyDescriptor;
  lFieldNode: TTreeNode;
  lSchema: TSchemaItem;
begin
  tvProperties.Items.Clear;
  lClassNode := tvProperties.Items.Add(nil, FFreedomObjectDescriptor.CompleteClassName);
  if FFreedomObjectDescriptor.FieldPropertyList.Count > 0 then
  begin
    if FFreedomObjectDescriptor.ObjectMapper.Columns.COunt > 0 then
    begin
      lParentNode := tvProperties.Items.AddChild(lClassNode, 'Fields');
      for lDescriptor in FFreedomObjectDescriptor.FieldPropertyList do
      begin
        lName := GetNameFromDescriptor(lDescriptor, True);
        lFieldNode := tvProperties.Items.AddChildObject(lParentNode, lName, lDescriptor);
        if (lDescriptor.FieldMapper <> nil) then
        begin
          lName := GetMapperText(lDescriptor.FieldMapper);
          tvProperties.Items.AddChild(lFieldNode, lName);
          lName := GetMapperColumnOptions(lDescriptor.FieldMapper);
          tvProperties.Items.AddChild(lFieldNode, lName);
          lName := GetMapperIDOptions(lDescriptor.FieldMapper);
          if lName <> '' then
          begin
            tvProperties.Items.AddChild(lFieldNode, lName);
          end;
          if lDescriptor.FieldMapper.Schemas.Count > 0 then
          begin
            for lSchema in lDescriptor.FieldMapper.Schemas do
            begin
              lName := 'Schema: ' + lSchema.Name;
              tvProperties.Items.AddChild(lFieldNode, lName);
            end;
          end;
        end;
      end;
    end;
    lParentNode := tvProperties.Items.AddChild(lClassNode, 'Properties');
    for lDescriptor in FFreedomObjectDescriptor.FieldPropertyList do
    begin
      lName := GetNameFromDescriptor(lDescriptor, False);
      tvProperties.Items.AddChildObject(lParentNode, lName, lDescriptor);
    end;
  end;
  tvProperties.FullExpand;
end;

procedure TNewFreedomObjectUnit.ValidateIfMapping;
begin
  if chkMappedAt.Checked then
  begin
    if Trim(edtEntityName.Text) = '' then
    begin
      ShowBalloHintForControl(edtEntityName, 'The name of mapping entity is required!');
      Abort;
    end;
  end;
end;

procedure TNewFreedomObjectUnit.ValidateStepOne;
begin
  if (Trim(edtUnitName.Text) = '') then
  begin
    ShowBalloHintForControl(edtUnitName, 'The name of unit is required!');
    Abort;
  end;
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

  FFreedomObjectDescriptor.NewUnitName := edtUnitName.Text;
  FFreedomObjectDescriptor.NewClassName := edtClassName.Text;
  FFreedomObjectDescriptor.NewBaseClass := cbxBaseClasses.Text;

  FFreedomObjectDescriptor.ListClassName := edtClassList.Text;
  FFreedomObjectDescriptor.BaseClassListName := cbxBaseClassList.Text;

  RefreshTreeview;
end;

procedure TNewFreedomObjectUnit.InitializeHistory;
var
  lIndex: Integer;
  lSections: TStrings;
  lIdent: String;
begin
  lSections := TStringList.Create;
  try
    for lIndex := 0 to ComponentCount - 1 do
    begin
      if (Components[lIndex].InheritsFrom(TComboBox)) and (TComboBox(Components[lIndex]).Style <> csDropDownList) then
      begin
        lSections.Clear;
        TComboBox(Components[lIndex]).Items.Clear;
        IniFile.ReadSection(TComboBox(Components[lIndex]).Name, lSections);
        for lIdent in lSections do
        begin
          TComboBox(Components[lIndex]).Items.Add(IniFile.ReadString(TComboBox(Components[lIndex]).Name, lIdent, ''));
        end;
      end;
    end;
  finally
    lSections.Free;
  end;
end;

procedure TNewFreedomObjectUnit.InitializeOptions;
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

procedure TNewFreedomObjectUnit.InitializeTabSheets;
begin
  tsUnitClassBaseClass.TabVisible := False;
  tsMapping.TabVisible := False;
  tsPrimary.TabVisible := False;
  tsUniques.TabVisible := False;
  tsSchemas.TabVisible := False;
  tsProperties.TabVisible := False;
  tsUnitClassBaseClass.Show;
end;

procedure TNewFreedomObjectUnit.FinalizeHistory;
var
  lIndex: Integer;
  lCombobox: TComboBox;
  lComboIndex: Integer;
begin
  for lIndex := 0 to ComponentCount - 1 do
  begin
    if (Components[lIndex].InheritsFrom(TComboBox)) and (TComboBox(Components[lIndex]).Style <> csDropDownList) then
    begin
      lCombobox := TComboBox(Components[lIndex]);
      if (lCombobox.Text <> '') and (lCombobox.Items.IndexOf(lCombobox.Text) < 0) then
      begin
        IniFile.WriteString(lCombobox.Name, IntToStr(lCombobox.Items.Count), lCombobox.Text);
      end;
      for lComboIndex := 0 to lCombobox.Items.Count - 1 do
      begin
        IniFile.WriteString(lCombobox.Name, IntToStr(lComboIndex), lCombobox.Items.Strings[lComboIndex]);
      end;
    end;
  end;
end;



end.
