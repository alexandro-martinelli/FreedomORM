unit AM.Freedom.frmNewProperty;

interface

uses
  {$REGION 'Uses'}
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.Messages,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  AM.Freedom.EnumerationTypes,
  Vcl.ComCtrls,
  Data.Bind.EngExt,
  Vcl.Bind.DBEngExt,
  System.Rtti,
  System.Bindings.Outputs,
  Vcl.Bind.Editors,
  Data.Bind.Components,
  Vcl.ExtCtrls,
  Vcl.Samples.Spin,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.FreedomObjectDescriptor,
  AM.Freedom.frmBase,
  Vcl.ImgList;
 {$ENDREGION}

type
  TfrmNewProperty = class(TfrmBase)
    {$REGION 'Fields'}
    lblDescription: TLabel;
    pgcPropertySteps: TPageControl;
    tsName: TTabSheet;
    tsType: TTabSheet;
    tsColumnOptions: TTabSheet;
    tsEnumOptions: TTabSheet;
    tsBoolOptions: TTabSheet;
    tsIdOptions: TTabSheet;
    tsSchemas: TTabSheet;
    lblPropertyName: TLabel;
    edtPropertyName: TComboBox;
    lblPropertyType: TLabel;
    cbxType: TComboBox;
    chkRequired: TCheckBox;
    chkNoUpdate: TCheckBox;
    chkNoInsert: TCheckBox;
    chkNoDelete: TCheckBox;
    lblBoolType: TLabel;
    cbxBoolType: TComboBox;
    lblBoolTrue: TLabel;
    edtBoolTrue: TComboBox;
    lblEnumName: TLabel;
    cbxEnumName: TComboBox;
    lblEnumType: TLabel;
    cbxEnumType: TComboBox;
    lblEnumCharValues: TLabel;
    edtEnumCharValues: TComboBox;
    chkIsId: TCheckBox;
    pnlIDOptions: TPanel;
    lblIDType: TLabel;
    lblSequenceName: TLabel;
    lblOnSchemas: TLabel;
    lblSchemas: TLabel;
    FlowPanel1: TFlowPanel;
    btnCancel: TButton;
    btnFinish: TButton;
    btnNext: TButton;
    BindingsList1: TBindingsList;
    LinkControlToPropertyEnabled: TLinkControlToProperty;
    edtSchema1: TComboBox;
    edtSchema2: TComboBox;
    lblBoolFalse: TLabel;
    edtBoolFalse: TComboBox;
    cbxIDType: TComboBox;
    edtSequenceName: TComboBox;
    edtOnSchemas: TComboBox;
    edtOnSchema2: TComboBox;
    tsMappedField: TTabSheet;
    chkMappedAt: TCheckBox;
    btnBack: TButton;
    LinkControlToPropertyEnabled10: TLinkControlToProperty;
    LinkControlToPropertyEnabled11: TLinkControlToProperty;
    LinkControlToPropertyEnabled12: TLinkControlToProperty;
    LinkControlToPropertyEnabled13: TLinkControlToProperty;
    LinkControlToPropertyEnabled14: TLinkControlToProperty;
    LinkControlToPropertyEnabled15: TLinkControlToProperty;
    LinkControlToPropertyEnabled16: TLinkControlToProperty;
    btnOptions: TButton;
    tsDetailOptions: TTabSheet;
    tsJoinOptions: TTabSheet;
    lblDetailColumnRefName: TLabel;
    edtDetailColumRefName: TComboBox;
    lblClassName: TLabel;
    cbxDetailClassName: TComboBox;
    chkDetailLazyLoad: TCheckBox;
    lblUpdateAction: TLabel;
    lblDeleteAction: TLabel;
    lblJoinRefColumnName: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    edtJoinRefColumnName: TComboBox;
    cbxJoinClassName: TComboBox;
    cbxJoinKind: TComboBox;
    chkJoinLazyLoad: TCheckBox;
    cbxUpdateAction: TComboBox;
    cbxDeleteAction: TComboBox;
    lblClassList: TLabel;
    edtDetailListClassName: TComboBox;
    pnlMappedAt: TPanel;
    lblMapping: TLabel;
    edtMapping: TComboBox;
    lblSize: TLabel;
    spedtSize: TSpinEdit;
    lblScale: TLabel;
    spedtScale: TSpinEdit;
    lblAlias: TLabel;
    edtAlias: TComboBox;
    lblDomain: TLabel;
    cbxDomain: TComboBox;
    lblDefaultValue: TLabel;
    cbxDefaultValue: TComboBox;
    gbxOrder: TGroupBox;
    lblOrderIndex: TLabel;
    lblOrderType: TLabel;
    lblOrderDirective: TLabel;
    spedtOrderIndex: TSpinEdit;
    cbxOrderType: TComboBox;
    cbxOrderDirective: TComboBox;
    chxIsNullable: TCheckBox;
    chxHideNullable: TCheckBox;
    chxUseNullableTypes: TCheckBox;
    tsExtension: TTabSheet;
    lblExtensionClassName: TLabel;
    cbxExtensionClassName: TComboBox;
    {$ENDREGION}
    procedure btnNextClick(Sender: TObject);
    procedure cbxIDTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbxBoolTypeChange(Sender: TObject);
    procedure cbxEnumTypeChange(Sender: TObject);
    procedure chkMappedAtClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure edtPropertyNameExit(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure chkIsIdClick(Sender: TObject);
    procedure chxIsNullableClick(Sender: TObject);
    procedure cbxExtensionClassNameExit(Sender: TObject);
    procedure cbxExtensionClassNameEnter(Sender: TObject);
    procedure tsEnumOptionsShow(Sender: TObject);
    procedure tsBoolOptionsShow(Sender: TObject);
  private const
    cName = 0;
    cType = 1;
    cMapping = 2;
    cEnumOptions = 3;
    cBoolOptions = 4;
    cDetailOptions = 5;
    cJoinOptions = 6;
    cColumnOptions = 7;
    cIdOptions = 8;
    cSchemas = 9;
    cExtension = 10;
  private
    FDescriptor: TFreedomClassDescriptor;
    FElapsedSteps: TList<Integer>;
    FExtensionClassDescriptor: TFreedomClassDescriptor;
    FOldExtensionClassName: String;
    FInsertingProperty: Boolean;
    procedure VerifyEnumTypeComponents;
    procedure VerifyBoolTypeComponents;

    procedure DoNext;
    procedure ValidatePropertyName;
    procedure ValidatePropertyType;
    procedure DoVisibileComponentsCaseTypes;
    procedure DoVerifyNullableComponents;

    procedure ValidateMappingField;
    procedure ValidatePropertyEnumOptions;
    procedure ValidatePropertyBoolOptions;
    procedure ValidatePropertyIdOptions;
    procedure ValidatePropertyDetailOptions;
    procedure ValidatePropertyJoinOptions;
    function GetNextPageIndex: Integer;
    function GetFieldPropertyDescriptor: TFieldPropertyDescriptor;
    function GetColumnMapperClass: TColumnMapperClass;
    procedure AssignDefaultValue(pColumn: TCustomColumnMapper);
    procedure AssignOrderOptions(pColumn: TCustomColumnMapper);
    procedure AssignColumnType(pColumn: TCustomColumnMapper);
    procedure AssignColumnOptions(pColumn: TCustomColumnMapper);
    procedure AssignIDOptions(pColumn: TCustomColumnMapper);
    procedure AssignSchemas(pColumn: TCustomColumnMapper);
    procedure AssignNullable(pColumn: TCustomColumnMapper);
    procedure AssignPropertyMapper(pPropertyMapper: TCustomColumnMapper);
    function HasID: Boolean;
    procedure RefreshButtons;
    procedure SetDescriptionText;
    procedure InitializeTabSheets;
    procedure InitializeDesignComponents;
    procedure InitializeHistory;
    procedure FinalizeHistory;
    procedure LoadPropertyCombinationFromIniFile;
    procedure LoadPropertyCombinationFromProperty(pFieldForEdit: TFieldPropertyDescriptor);
    procedure DoPanelIdEnabled(pEnabled: Boolean);
    procedure InitializeOptions;
    function InsertingProperty: Boolean;
  protected
    function CanAskForClose: Boolean; override;
    property FreedomObjectDescriptor: TFreedomClassDescriptor read FDescriptor write FDescriptor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function FieldProperty(pFreedomObjectDescriptor: TFreedomClassDescriptor): TFieldPropertyDescriptor;
    class function EditFieldProperty(pFreedomObjectDescriptor: TFreedomClassDescriptor;
        pFieldForEdit: TFieldPropertyDescriptor): TFieldPropertyDescriptor;
    property FieldPropertyDescriptor: TFieldPropertyDescriptor read GetFieldPropertyDescriptor;
  end;

implementation

uses
  System.StrUtils,
  AM.Freedom.frmOptions,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.Helper.JoinKind,
  AM.Freedom.Helper.ForeignOption,
  System.SysUtils,
  AM.Freedom.Helpers.WinControl,
  AM.Freedom.Helper.OrderType, AM.Freedom.frmClassUnitName, AM.Freedom.frmNewClass;

{$R *.dfm}

{ TfrmNewProperty }

class function TfrmNewProperty.FieldProperty(pFreedomObjectDescriptor: TFreedomClassDescriptor): TFieldPropertyDescriptor;
var
  lForm: TfrmNewProperty;
begin
  lForm := TfrmNewProperty.Create(nil);
  Result := nil;
  try
    lForm.FreedomObjectDescriptor := pFreedomObjectDescriptor;
    if lForm.ShowModal = mrOK then
    begin
      Result := lForm.FieldPropertyDescriptor;
    end;
  finally
    FreeAndNil(lForm);
  end;
end;

procedure TfrmNewProperty.FinalizeHistory;
var
  lIndex: Integer;
  lCombobox: TComboBox;
  lComboIndex: Integer;
  lCompSection: string;
begin
  if (ModalResult = mrOk) then
  begin
    for lIndex := 0 to ComponentCount - 1 do
    begin
      if (Components[lIndex].InheritsFrom(TComboBox)) and (TComboBox(Components[lIndex]).Style <> csDropDownList) then
      begin
        lCombobox := TComboBox(Components[lIndex]);
        if (lCombobox.Text <> '') and (lCombobox.Items.IndexOf(lCombobox.Text) < 0) then
        begin
          IniFile.WriteString(TComboBox(Components[lIndex]).Name, IntToStr(lCombobox.Items.Count), lCombobox.Text);
        end;
        for lComboIndex := 0 to lCombobox.Items.Count - 1 do
        begin
          IniFile.WriteString(TComboBox(Components[lIndex]).Name, IntToStr(lComboIndex), lCombobox.Items.Strings[lComboIndex]);
        end;
      end;
    end;
    lCompSection := 'Comb_' + edtPropertyName.Text;
    IniFile.WriteInteger(lCompSection, 'Type', cbxType.ItemIndex);
    IniFile.WriteBool(lCompSection, 'MappedAt', chkMappedAt.Checked);
    IniFile.WriteString(lCompSection, 'ColumnName', edtMapping.Text);
    IniFile.WriteInteger(lCompSection, 'Size', spedtSize.Value);
    IniFile.WriteInteger(lCompSection, 'Scale', spedtScale.Value);
    IniFile.WriteString(lCompSection, 'ColumnAlias', edtAlias.Text);
    IniFile.WriteString(lCompSection, 'Domain', cbxDomain.Text);
    IniFile.WriteString(lCompSection, 'DefaultValue', cbxDefaultValue.Text);
    IniFile.WriteInteger(lCompSection, 'OrderIndex', spedtOrderIndex.Value);
    IniFile.WriteInteger(lCompSection, 'OrderType', cbxOrderType.ItemIndex);
    IniFile.WriteString(lCompSection, 'OrderDirective', cbxOrderDirective.Text);
    IniFile.WriteString(lCompSection, 'EnumName', cbxEnumName.Text);
    IniFile.WriteInteger(lCompSection, 'EnumType', cbxEnumType.ItemIndex);
    IniFile.WriteString(lCompSection, 'EnumValues', edtEnumCharValues.Text);
    IniFile.WriteInteger(lCompSection, 'BoolType', cbxBoolType.ItemIndex);
    IniFile.WriteString(lCompSection, 'BoolTrue', edtBoolTrue.Text);
    IniFile.WriteString(lCompSection, 'BoolFalse', edtBoolFalse.Text);
    IniFile.WriteBool(lCompSection, 'Required', chkRequired.Checked);
    IniFile.WriteBool(lCompSection, 'NoInsert', chkNoInsert.Checked);
    IniFile.WriteBool(lCompSection, 'NoUpdate', chkNoUpdate.Checked);
    IniFile.WriteBool(lCompSection, 'NoDelete', chkNoDelete.Checked);
    IniFile.WriteBool(lCompSection, 'IsID', chkIsId.Checked);
    IniFile.WriteInteger(lCompSection, 'IdType', cbxIDType.ItemIndex);
    IniFile.WriteString(lCompSection, 'SequenceName', edtSequenceName.Text);
    IniFile.WriteString(lCompSection, 'IdSchema1', edtOnSchemas.Text);
    IniFile.WriteString(lCompSection, 'IdSchema2', edtOnSchema2.Text);
    IniFile.WriteString(lCompSection, 'ColumnSchema1', edtSchema1.Text);
    IniFile.WriteString(lCompSection, 'ColumnSchema2', edtSchema2.Text);

    IniFile.WriteString(lCompSection, 'DetailColumRefName', edtDetailColumRefName.Text);
    IniFile.WriteString(lCompSection, 'DetailClassName', cbxDetailClassName.Text);
    IniFile.WriteBool(lCompSection, 'DetailLazyLoad', chkDetailLazyLoad.Checked);
    IniFile.WriteString(lCompSection, 'DetailClassList', edtDetailListClassName.Text);
    IniFile.WriteString(lCompSection, 'JoinColumRefName', edtJoinRefColumnName.Text);
    IniFile.WriteString(lCompSection, 'JoinClassName', cbxJoinClassName.Text);
    IniFile.WriteBool(lCompSection, 'JoinLazyLoad', chkJoinLazyLoad.Checked);
    IniFile.WriteInteger(lCompSection, 'JoinKind', cbxJoinKind.ItemIndex);
    IniFile.WriteInteger(lCompSection, 'UpdateAction', cbxUpdateAction.ItemIndex);
    IniFile.WriteInteger(lCompSection, 'DeleteAction', cbxDeleteAction.ItemIndex);
    IniFile.WriteBool(lCompSection, 'IsNullable', chxIsNullable.Checked);
    IniFile.WriteBool(lCompSection, 'HideNullable', chxHideNullable.Checked);
    IniFile.WriteBool(lCompSection, 'UseNullableTypes', chxUseNullableTypes.Checked);
  end;
end;

procedure TfrmNewProperty.AssignColumnOptions(pColumn: TCustomColumnMapper);
var
  lColumnOptions: TColumnOptions;
begin
  lColumnOptions := [];
  if chkRequired.Checked then
  begin
    lColumnOptions := lColumnOptions + [Required];
  end;
  if chkNoInsert.Checked then
  begin
    lColumnOptions := lColumnOptions + [NoInsert];
  end;
  if chkNoUpdate.Checked then
  begin
    lColumnOptions := lColumnOptions + [NoUpdate];
  end;
  if chkNoDelete.Checked then
  begin
    lColumnOptions := lColumnOptions + [NoDelete];
  end;
  pColumn.ColumnOptions := lColumnOptions;
end;

procedure TfrmNewProperty.AssignColumnType(pColumn: TCustomColumnMapper);
var
  lStrings: TStrings;
  lIndex: Integer;
  lBoolTrue: Integer;
  lCharOf: TArray<Char>;
begin
  if SameText('Enumerator', cbxType.Text) then
  begin
    case cbxEnumType.ItemIndex of
      0: TEnumerationColumnMapper(pColumn).EnumType := emByte;
      1:
         begin
           TEnumerationColumnMapper(pColumn).EnumType := emChar;
           lStrings := TStringList.Create;
           try
             ExtractStrings([',', ';'], [' '], PWideChar(edtEnumCharValues.Text), lStrings);
             SetLength(lCharOf, lStrings.Count);
             for lIndex := 0 to lStrings.Count - 1 do
             begin
               lCharOf[lIndex] := lStrings.Strings[lIndex][1];
             end;
             TEnumerationColumnMapper(pColumn).EnumCharOf := lCharOf;
           finally
             lStrings.Free;
           end;
         end;
    end;
  end
  else if SameText('Boolean', cbxType.Text) then
  begin
    case cbxBoolType.ItemIndex of
      0: TBooleanColumnMapper(pColumn).InternalColumnType := ctyBoolean;
      1:
        begin
          if TryStrToInt(edtBoolTrue.Text, lBoolTrue) then
          begin
            TBooleanColumnMapper(pCOlumn).InternalColumnType := ctyByte;
          end else
          begin
            TBooleanColumnMapper(pColumn).InternalColumnType := ctyChar;
          end;
          TBooleanColumnMapper(pColumn).ValueTrue := edtBoolTrue.Text;
          TBooleanColumnMapper(pColumn).ValueFalse := edtBoolFalse.Text;
        end;
    end;
  end
  else if SameText('Detail', cbxType.Text) then
  begin
    TDetailColumnMapper(pColumn).LazyOptions.IsLazy := chkDetailLazyLoad.Checked;
    TDetailColumnMapper(pColumn).LazyOptions.LazyType := List;
    TDetailColumnMapper(pColumn).RefColumnName := edtDetailColumRefName.Text;
  end
  else if SameText('Join', cbxType.Text) then
  begin
    TJoinedColumnMapper(pColumn).LazyOptions.IsLazy := chkJoinLazyLoad.Checked;
    TJoinedColumnMapper(pColumn).LazyOptions.LazyType := Simple;
    TJoinedColumnMapper(pColumn).RefColumnName := edtJoinRefColumnName.Text;
  end
  else if SameText('Blob', cbxType.Text) then
  begin
    TBlobColumnMapper(pColumn).BlobType := Binary;
  end
  else if SameText('Memo', cbxType.Text) then
  begin
    TBlobColumnMapper(pColumn).BlobType := TBlobType.Text;
  end
  else if SameText('XML', cbxType.Text) then
  begin
    TBlobColumnMapper(pColumn).BlobType := TBlobType.XML;
  end
  else
  begin
    if spedtSize.Value > 0 then
    begin
      pColumn.Size := spedtSize.Value;
    end;
    if spedtScale.Value > 0 then
    begin
      pColumn.Scale := spedtScale.Value;
    end;
  end;
end;

procedure TfrmNewProperty.AssignDefaultValue(pColumn: TCustomColumnMapper);
var
  lIntValue: Integer;
begin
  pColumn.DefaultValueOptions.IsNow := SameText('Now', cbxDefaultValue.Text) or SameText('Date', cbxDefaultValue.Text)
    or SameText('Time', cbxDefaultValue.Text);
  if not pColumn.DefaultValueOptions.IsNow and (Trim(cbxDefaultValue.Text) <> '') then
  begin
    if (TryStrToInt(cbxDefaultValue.Text, lIntValue)) then
    begin
      pColumn.DefaultValueOptions.Value := lIntValue;
    end else
    begin
      pColumn.DefaultValueOptions.Value := cbxDefaultValue.Text;
    end;
  end;
end;

procedure TfrmNewProperty.AssignIDOptions(pColumn: TCustomColumnMapper);
begin
  pColumn.IdOptions.IsId := chkIsId.Checked;
  if chkIsId.Checked then
  begin
    if cbxIDType.ItemIndex = 0 then
    begin
      pColumn.IdOptions.IdOption := Identity;
    end
    else if cbxIDType.ItemIndex = 1 then
    begin
      pColumn.IdOptions.IdOption := Sequence;
      pColumn.IdOptions.SequenceName := edtSequenceName.Text;
    end
    else if cbxIDType.ItemIndex = 2 then
    begin
      pColumn.IdOptions.IdOption := TIdOption.None;
    end;
    if Trim(edtOnSchemas.Text) <> '' then
    begin
      pColumn.IdOptions.Schemas.AddSchema(edtOnSchemas.Text, False)
    end;
    if Trim(edtOnSchema2.Text) <> '' then
    begin
      pColumn.IdOptions.Schemas.AddSchema(edtOnSchema2.Text, False)
    end;
  end;
end;

procedure TfrmNewProperty.AssignNullable(pColumn: TCustomColumnMapper);
begin
  pColumn.IsNullable := chxIsNullable.Visible and chxIsNullable.Checked;
end;

procedure TfrmNewProperty.AssignOrderOptions(pColumn: TCustomColumnMapper);
begin
  if (spedtOrderIndex.Value > 0) then
  begin
    pColumn.OrderOptions.Index := spedtOrderIndex.Value;
    pColumn.OrderOptions.OrderType := TOrderType(cbxOrderType.ItemIndex);
    pColumn.OrderOptions.Directive := Trim(cbxOrderDirective.Text);
  end;
end;

procedure TfrmNewProperty.AssignPropertyMapper(pPropertyMapper: TCustomColumnMapper);
begin
  pPropertyMapper.Name := edtPropertyName.Text;
  pPropertyMapper.ColumnType := TColumnTypeMethods.ColumnTypeFromString(cbxType.Text);
end;

procedure TfrmNewProperty.AssignSchemas(pColumn: TCustomColumnMapper);
begin
  if Trim(edtSchema1.Text) <> '' then
  begin
    pColumn.Schemas.AddSchema(edtSchema1.Text, False);
  end;
  if Trim(edtSchema2.Text) <> '' then
  begin
    pColumn.Schemas.AddSchema(edtSchema2.Text, False);
  end;
end;

procedure TfrmNewProperty.btnBackClick(Sender: TObject);
var
  lPageIndex: Integer;
begin
  if (FElapsedSteps.Count > 0) then
  begin
    lPageIndex := FElapsedSteps.Items[FElapsedSteps.Count - 1];
    FElapsedSteps.Extract(lPageIndex);
    pgcPropertySteps.ActivePageIndex := lPageIndex;
  end;
  RefreshButtons;
end;

procedure TfrmNewProperty.btnNextClick(Sender: TObject);
begin
  DoNext;
end;

procedure TfrmNewProperty.btnOptionsClick(Sender: TObject);
begin
  TfrmOptions.ShowOptions;
  InitializeOptions;
end;

function TfrmNewProperty.CanAskForClose: Boolean;
begin
  Result := pgcPropertySteps.ActivePageIndex > 0;
end;

procedure TfrmNewProperty.cbxBoolTypeChange(Sender: TObject);
begin
  VerifyBoolTypeComponents;
end;

procedure TfrmNewProperty.cbxEnumTypeChange(Sender: TObject);
begin
  VerifyEnumTypeComponents;
end;

procedure TfrmNewProperty.cbxExtensionClassNameEnter(Sender: TObject);
begin
  inherited;
  FOldExtensionClassName := cbxExtensionClassName.Text;
end;

procedure TfrmNewProperty.cbxExtensionClassNameExit(Sender: TObject);
var
  lUnitName: string;
begin
  inherited;
  if (cbxExtensionClassName.Text <> '') and (FOldExtensionClassName <> cbxExtensionClassName.Text) then
  begin
    TfrmClassUnitName.InitializeClassHistory(TfrmNewProperty);
    try
      lUnitName := TfrmClassUnitName.GetUnitNameForClassName(cbxExtensionClassName.Text);
    finally
      TfrmClassUnitName.FinalizeClassHistory(TfrmNewProperty);
    end;
    if (lUnitName = '') then
    begin
      FreeAndNil(FExtensionClassDescriptor);
      FExtensionClassDescriptor := TfrmNewClass.CreateNewClass(cbxExtensionClassName.Text);
    end;
  end;
end;

procedure TfrmNewProperty.cbxIDTypeChange(Sender: TObject);
begin
  lblSequenceName.Visible := cbxIDType.ItemIndex = 1;
  edtSequenceName.Visible := lblSequenceName.Visible;
end;

procedure TfrmNewProperty.chkIsIdClick(Sender: TObject);
begin
  inherited;
  pnlIDOptions.EnabledInternalControls(chkIsId.Checked);
end;

procedure TfrmNewProperty.chkMappedAtClick(Sender: TObject);
begin
  btnNext.Enabled := chkMappedAt.Checked or SameText(cbxType.Text, 'Enumerator');
  btnNext.Default := btnNext.Enabled;
  btnFinish.Enabled := not btnNext.Enabled;
  btnFinish.Default := btnFinish.Enabled;
  lblEnumType.Visible := btnNext.Enabled;
  cbxEnumType.Visible := lblEnumType.Visible;

  pnlMappedAt.EnabledInternalControls(chkMappedAt.Checked);
end;

procedure TfrmNewProperty.chxIsNullableClick(Sender: TObject);
begin
  inherited;
  DoVerifyNullableComponents;
end;

constructor TfrmNewProperty.Create(AOwner: TComponent);
var
  lIndex: Integer;
begin
  inherited;
  FElapsedSteps := TList<Integer>.Create;
  cbxOrderType.Items.Clear;
  for lIndex := Ord(Low(TOrderType)) to Ord(High(TOrderType)) do
  begin
    cbxOrderType.Items.Add(TOrderType(lIndex).ToString);
  end;
  InitializeTabSheets;
  InitializeDesignComponents;
  InitializeHistory;
  InitializeOptions;
  FInsertingProperty := True;
end;

destructor TfrmNewProperty.Destroy;
begin
  FinalizeHistory;
  FElapsedSteps.Free;
  inherited;
end;

procedure TfrmNewProperty.DoNext;
begin
  case pgcPropertySteps.ActivePageIndex of
    cName:
      begin
        ValidatePropertyName;
      end;
    cType:
      begin
        ValidatePropertyType;
        DoVisibileComponentsCaseTypes;
      end;
    cMapping:
      begin
        ValidateMappingField;
      end;
    cEnumOptions:
      begin
        ValidatePropertyEnumOptions;
      end;
    cBoolOptions:
      begin
        ValidatePropertyBoolOptions;
      end;
    cDetailOptions:
      begin
        ValidatePropertyDetailOptions;
      end;
    cJoinOptions:
      begin
        ValidatePropertyJoinOptions;
      end;
    cIdOptions:
      begin
        ValidatePropertyIdOptions;
      end;
  end;
  FElapsedSteps.Add(pgcPropertySteps.ActivePageIndex);
  if (pgcPropertySteps.ActivePageIndex = cName) and InsertingProperty then
  begin
    LoadPropertyCombinationFromIniFile;
  end;
  pgcPropertySteps.ActivePageIndex := GetNextPageIndex;
  RefreshButtons;
end;

procedure TfrmNewProperty.DoPanelIdEnabled(pEnabled: Boolean);
begin
  pnlIDOptions.EnabledInternalControls(pEnabled);
end;

procedure TfrmNewProperty.DoVerifyNullableComponents;
begin
  chxHideNullable.Enabled := chxIsNullable.Checked;
  chxUseNullableTypes.Enabled := chxIsNullable.Checked;
  if not chxHideNullable.Enabled then
  begin
    chxHideNullable.Checked := False;
  end;
  if not chxUseNullableTypes.Enabled then
  begin
    chxUseNullableTypes.Checked := False;
  end;
end;

procedure TfrmNewProperty.DoVisibileComponentsCaseTypes;
begin
  spedtSize.Visible := ContainsText('String;Double;Currency;Single;Extended', cbxType.Text);
  spedtScale.Visible := spedtSize.Visible and ContainsText('Currency;Extended', cbxType.Text);
  lblSize.Visible := spedtSize.Visible;
  lblScale.Visible := spedtScale.Visible;
  chxIsNullable.Visible := ContainsText('Smallint;Integer;Int64;Char;String;Single;Double;Currency;Extended;TDate;TTime;TDateTime;Boolean;Byte', cbxType.Text);
  chxHideNullable.Visible := chxIsNullable.Visible;
  chxUseNullableTypes.Visible := chxIsNullable.Visible;
  if (not chxIsNullable.Visible) then
  begin
    chxIsNullable.Checked := False;
    chxHideNullable.Checked := False;
    chxUseNullableTypes.Checked := False;
  end;
end;

class function TfrmNewProperty.EditFieldProperty(pFreedomObjectDescriptor: TFreedomClassDescriptor;
  pFieldForEdit: TFieldPropertyDescriptor): TFieldPropertyDescriptor;
var
  lForm: TfrmNewProperty;
begin
  lForm := TfrmNewProperty.Create(nil);
  Result := nil;
  try
    lForm.FreedomObjectDescriptor := pFreedomObjectDescriptor;
    lForm.LoadPropertyCombinationFromProperty(pFieldForEdit);
    if lForm.ShowModal = mrOK then
    begin
      Result := lForm.FieldPropertyDescriptor;
    end;
  finally
    FreeAndNil(lForm);
  end;
end;

procedure TfrmNewProperty.VerifyEnumTypeComponents;
begin
  lblEnumCharValues.Visible := cbxEnumType.ItemIndex = 1;
  edtEnumCharValues.Visible := lblEnumCharValues.Visible;
end;

procedure TfrmNewProperty.VerifyBoolTypeComponents;
begin
  lblBoolTrue.Visible := cbxBoolType.ItemIndex <> 0;
  lblBoolFalse.Visible := lblBoolTrue.Visible;
  edtBoolFalse.Visible := lblBoolTrue.Visible;
  edtBoolTrue.Visible := lblBoolTrue.Visible;
end;

procedure TfrmNewProperty.edtPropertyNameExit(Sender: TObject);
begin
  SetDescriptionText;
end;

procedure TfrmNewProperty.FormShow(Sender: TObject);
begin
  edtPropertyName.SetFocus;
end;

function TfrmNewProperty.GetFieldPropertyDescriptor: TFieldPropertyDescriptor;
begin
  Result := TFieldPropertyDescriptor.Create;
  if chkMappedAt.Checked then
  begin
    Result.FieldMapper := GetColumnMapperClass.Create;
    Result.FieldMapper.ColumnType := TColumnTypeMethods.ColumnTypeFromString(cbxType.Text);
    Result.FieldMapper.Name := Trim(edtMapping.Text);
    Result.FieldMapper.Alias := Trim(edtAlias.Text);
    Result.FieldMapper.Domain := Trim(cbxDomain.Text);
    AssignDefaultValue(Result.FieldMapper);
    AssignOrderOptions(Result.FieldMapper);
    AssignColumnType(Result.FieldMapper);
    AssignColumnOptions(Result.FieldMapper);
    AssignIDOptions(Result.FieldMapper);
    AssignSchemas(Result.FieldMapper);
    AssignNullable(Result.FieldMapper);
    Result.HideNullable := chxHideNullable.Visible and chxHideNullable.Checked;
    Result.UseNullableTypes := chxUseNullableTypes.Visible and chxUseNullableTypes.Checked;
  end;
  Result.PropertyMapper := GetColumnMapperClass.Create;
  AssignColumnType(Result.PropertyMapper);
  if SameText(cbxType.Text, 'Enumerator') then
  begin
    Result.TypeName := cbxEnumName.Text;
    Result.TypeNameUnit := ExtractClassUnit(cbxEnumName.Text);
  end
  else
  if SameText(cbxType.Text, 'Extension') then
  begin
    Result.TypeName := cbxExtensionClassName.Text;
    if (Assigned(FExtensionClassDescriptor)) then
    begin
      Result.Extension := FExtensionClassDescriptor;
    end;
  end
  else if SameText(cbxType.Text, 'Memo') then
  begin
    Result.TypeName := 'TStrings';
  end
  else if SameText(cbxType.Text, 'Blob') then
  begin
    Result.TypeName := 'TStream';
  end
  else if SameText(cbxType.Text, 'XML') then
  begin
    Result.TypeName := 'TXML';
  end
  else if SameText(cbxType.Text, 'Join') then
  begin
    if (chkJoinLazyLoad.Checked) then
    begin
      Result.TypeName := Format('TLazy<%s>', [cbxJoinClassName.Text]);
      Result.LazyClassName := cbxJoinClassName.Text;
    end
    else
    begin
      Result.TypeName := cbxJoinClassName.Text;
    end;
    Result.TypeNameUnit := ExtractClassUnit(cbxJoinClassName.Text);
    Result.LazyClassName := cbxJoinClassName.Text;
  end
  else if SameText(cbxType.Text, 'Detail') then
  begin
    if (chkDetailLazyLoad.Checked) then
    begin
      Result.TypeName := Format('TLazyList<%s>', [cbxDetailClassName.Text]);
    end
    else
    begin
      Result.TypeName := cbxDetailClassName.Text;
    end;
    Result.TypeNameUnit := ExtractClassUnit(cbxDetailClassName.Text);
    Result.LazyClassName := edtDetailListClassName.Text;
  end
  else
  begin
    Result.TypeName := TColumnType(cbxType.ItemIndex).ToString;
  end;
  AssignPropertyMapper(Result.PropertyMapper);
end;

function TfrmNewProperty.GetColumnMapperClass: TColumnMapperClass;
begin
  if SameText('Enumerator', cbxType.Text) then
  begin
    Result := TEnumerationColumnMapper;
  end
  else if SameText('Boolean', cbxType.Text) then
  begin
    Result := TBooleanColumnMapper;
  end
  else if SameText('Detail', cbxType.Text) then
  begin
    Result := TDetailColumnMapper;
  end
  else if SameText('Join', cbxType.Text) then
  begin
    Result := TJoinedColumnMapper;
  end
  else if ContainsText('Blob,Memo,XML', cbxType.Text) then
  begin
    Result := TBlobColumnMapper;
  end
  else if ContainsText('String,Char,Double,Single,Extended,Currency', cbxType.Text) then
  begin
    Result := TCustomColumnMapper;
  end
  else
  begin
    Result := TCustomColumnMapper;
  end;
end;

function TfrmNewProperty.GetNextPageIndex: Integer;
begin
  Result := pgcPropertySteps.ActivePageIndex + 1;
  if (pgcPropertySteps.ActivePageIndex = cMapping) then
  begin
    if SameText('Boolean', cbxType.Text) and chkMappedAt.Checked then
    begin
      Result := cBoolOptions;
    end
    else if SameText('Enumerator', cbxType.Text) then
    begin
      Result := cEnumOptions;
    end
    else if SameText('Detail', cbxType.Text) and chkMappedAt.Checked then
    begin
      Result := cDetailOptions;
    end
    else if SameText('Join', cbxType.Text) and chkMappedAt.Checked then
    begin
      Result := cJoinOptions;
    end
    else if chkMappedAt.Checked then
    begin
      Result := cColumnOptions;
    end;
  end
  else if (pgcPropertySteps.ActivePageIndex in [cBoolOptions, cEnumOptions, cDetailOptions, cJoinOptions]) then
  begin
    Result := cColumnOptions;
  end
  else if (pgcPropertySteps.ActivePageIndex = cColumnOptions) and HasID then
  begin
    Result := cSchemas;
  end
  else if SameText('Extension', cbxType.Text) then
  begin
    Result := cExtension;
  end
end;

function TfrmNewProperty.HasID: Boolean;
begin
  Result := FDescriptor.ObjectMapper.Columns.IDColumn <> nil;
end;

procedure TfrmNewProperty.InitializeDesignComponents;
var
  lIndex: Integer;
begin
  chkMappedAt.Checked := True;
  cbxType.Items.Clear;
  for lIndex := Ord(Low(TColumnType)) to Ord(High(TColumnType)) do
  begin
    if (cbxType.Items.IndexOf(TColumnType(lIndex).ToString) = -1) then
    begin
      cbxType.Items.Add(TColumnType(lIndex).ToString);
    end;
  end;
  cbxType.ItemIndex := 0;
  for lIndex := Ord(Low(TJoinKind)) to Ord(High(TJoinKind)) do
  begin
    cbxJoinKind.Items.Add(TJoinKind(lIndex).ToSQL);
  end;
  cbxJoinKind.ItemIndex := 1;
  for lIndex := Ord(Low(TForeignOption)) to Ord(High(TForeignOption)) do
  begin
    cbxUpdateAction.Items.Add(TForeignOption(lIndex).ToString);
  end;
  cbxUpdateAction.ItemIndex := 1;
  cbxDeleteAction.Items.AddStrings(cbxUpdateAction.Items);
  cbxDeleteAction.ItemIndex := 0;

  RefreshButtons;
  SetDescriptionText;
end;

procedure TfrmNewProperty.InitializeHistory;
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

procedure TfrmNewProperty.InitializeOptions;
begin
  edtMapping.CharCase := TEditCharCase(IniFile.ReadInteger('Options', 'MappingCharCase', 1));
  edtAlias.CharCase := edtMapping.CharCase;
  edtDetailColumRefName.CharCase := edtMapping.CharCase;
  edtJoinRefColumnName.CharCase := edtMapping.CharCase;
  cbxDomain.CharCase := edtMapping.CharCase;
  cbxOrderDirective.CharCase := edtMapping.CharCase;

  edtSequenceName.CharCase := TEditCharCase(IniFile.ReadInteger('Options', 'SchemaCharCase', 2));
  edtOnSchemas.CharCase := edtSequenceName.CharCase;
  edtOnSchema2.CharCase := edtSequenceName.CharCase;
  edtSchema1.CharCase := edtSequenceName.CharCase;
  edtSchema2.CharCase := edtSequenceName.CharCase;
end;

procedure TfrmNewProperty.InitializeTabSheets;
var
  lIndex: Integer;
begin
  for lIndex := 0 to ComponentCount - 1 do
  begin
    if (Components[lIndex] is TTabSheet) then
    begin
      TTabSheet(Components[lIndex]).TabVisible := False;
    end;
  end;
  tsName.Show;
end;

function TfrmNewProperty.InsertingProperty: Boolean;
begin
  Result := FInsertingProperty;
end;

procedure TfrmNewProperty.LoadPropertyCombinationFromIniFile;
var
  lCompSection: string;
begin
  lCompSection := 'Comb_' + edtPropertyName.Text;
  cbxType.ItemIndex := IniFile.ReadInteger(lCompSection, 'Type', 0);
  chkMappedAt.Checked := IniFile.ReadBool(lCompSection, 'MappedAt', True);
  edtMapping.Text := IniFile.ReadString(lCompSection, 'ColumnName', '');
  spedtSize.Value := IniFile.ReadInteger(lCompSection, 'Size', 0);
  spedtScale.Value := IniFile.ReadInteger(lCompSection, 'Scale', 0);
  edtAlias.Text := IniFile.ReadString(lCompSection, 'ColumnAlias', '');
  cbxDomain.Text := IniFile.ReadString(lCompSection, 'Domain', '');
  cbxDefaultValue.Text := IniFile.ReadString(lCompSection, 'DefaultValue', '');
  spedtOrderIndex.Value := IniFile.ReadInteger(lCompSection, 'OrderIndex', 0);
  cbxOrderType.ItemIndex := IniFile.ReadInteger(lCompSection, 'OrderType', 0);
  cbxOrderDirective.Text := IniFile.ReadString(lCompSection, 'OrderDirective', '');
  cbxEnumName.Text := IniFile.ReadString(lCompSection, 'EnumName', '');
  cbxEnumType.ItemIndex := IniFile.ReadInteger(lCompSection, 'EnumType', 0);
  edtEnumCharValues.Text := IniFile.ReadString(lCompSection, 'EnumValues', '');
  cbxBoolType.ItemIndex := IniFile.ReadInteger(lCompSection, 'BoolType', 0);
  edtBoolTrue.Text := IniFile.ReadString(lCompSection, 'BoolTrue', '');
  edtBoolFalse.Text := IniFile.ReadString(lCompSection, 'BoolFalse', '');
  chkRequired.Checked := IniFile.ReadBool(lCompSection, 'Required', False);
  chkNoInsert.Checked := IniFile.ReadBool(lCompSection, 'NoInsert', False);
  chkNoUpdate.Checked := IniFile.ReadBool(lCompSection, 'NoUpdate', False);
  chkNoDelete.Checked := IniFile.ReadBool(lCompSection, 'NoDelete', False);
  chxIsNullable.Checked := IniFile.ReadBool(lCompSection, 'IsNullable', False);
  chxHideNullable.Checked := IniFile.ReadBool(lCompSection, 'HideNullable', False);
  chxUseNullableTypes.Checked := IniFile.ReadBool(lCompSection, 'UseNullableTypes', False);
  chkIsId.Checked := IniFile.ReadBool(lCompSection, 'IsID', False);
  DoPanelIdEnabled(chkIsId.Checked);
  DoVerifyNullableComponents;
  if (chkIsId.Checked) and (FreedomObjectDescriptor.ObjectMapper.Columns.IDColumn = nil) then
  begin
    cbxIDType.ItemIndex := IniFile.ReadInteger(lCompSection, 'IdType', 0);
    edtSequenceName.Text := IniFile.ReadString(lCompSection, 'SequenceName', '');
    edtOnSchemas.Text := IniFile.ReadString(lCompSection, 'IdSchema1', '');
    edtOnSchema2.Text := IniFile.ReadString(lCompSection, 'IdSchema2', '');
  end
  else
  begin
    chkIsId.Checked := False;
  end;
  edtSchema1.Text := IniFile.ReadString(lCompSection, 'ColumnSchema1', '');
  edtSchema2.Text := IniFile.ReadString(lCompSection, 'ColumnSchema2', '');

  if SameText(cbxType.Text, 'Detail') then
  begin
    edtDetailColumRefName.Text := IniFile.ReadString(lCompSection, 'DetailColumRefName', '');
    cbxDetailClassName.Text := IniFile.ReadString(lCompSection, 'DetailClassName', '');
    chkDetailLazyLoad.Checked := IniFile.ReadBool(lCompSection, 'DetailLazyLoad', True);
    edtDetailListClassName.Text := IniFile.ReadString(lCompSection, 'DetailClassList', '');
  end
  else if SameText(cbxType.Text, 'Join') then
  begin
    edtJoinRefColumnName.Text := IniFile.ReadString(lCompSection, 'JoinColumRefName', '');
    cbxJoinClassName.Text := IniFile.ReadString(lCompSection, 'JoinClassName', '');
    chkJoinLazyLoad.Checked := IniFile.ReadBool(lCompSection, 'JoinLazyLoad', True);
    cbxJoinKind.ItemIndex := IniFile.ReadInteger(lCompSection, 'JoinKind', 1);
    cbxUpdateAction.ItemIndex := IniFile.ReadInteger(lCompSection, 'UpdateAction', 0);
    cbxDeleteAction.ItemIndex := IniFile.ReadInteger(lCompSection, 'DeleteAction', 0);
  end;
  VerifyBoolTypeComponents;
  VerifyEnumTypeComponents;
end;

procedure TfrmNewProperty.LoadPropertyCombinationFromProperty(pFieldForEdit: TFieldPropertyDescriptor);
var
  lIndex: Integer;
begin
  FInsertingProperty := False;
  edtPropertyName.Text := pFieldForEdit.PropertyMapper.Name;
  cbxType.ItemIndex := cbxType.Items.IndexOf(pFieldForEdit.FieldMapper.ColumnType.ToString);
  chkMappedAt.Checked := True;
  edtMapping.Text := pFieldForEdit.FieldMapper.Name;
  spedtSize.Value := pFieldForEdit.FieldMapper.Size;
  spedtScale.Value := pFieldForEdit.FieldMapper.Scale;
  edtAlias.Text := pFieldForEdit.FieldMapper.Alias;
  cbxDomain.Text := pFieldForEdit.FieldMapper.Domain;
  cbxDefaultValue.Text := VarToStr(pFieldForEdit.FieldMapper.DefaultValueOptions.Value);
  spedtOrderIndex.Value := pFieldForEdit.FieldMapper.OrderOptions.Index;
  cbxOrderType.Text := pFieldForEdit.FieldMapper.OrderOptions.OrderType.ToString;
  cbxOrderDirective.Text := pFieldForEdit.FieldMapper.OrderOptions.Directive;
  chkRequired.Checked := Required in pFieldForEdit.FieldMapper.ColumnOptions;
  chkNoInsert.Checked := NoInsert in pFieldForEdit.FieldMapper.ColumnOptions;
  chkNoUpdate.Checked := NoUpdate in pFieldForEdit.FieldMapper.ColumnOptions;
  chkNoDelete.Checked := NoDelete in pFieldForEdit.FieldMapper.ColumnOptions;
  chxIsNullable.Checked := pFieldForEdit.FieldMapper.IsNullable;
  chxHideNullable.Checked := pFieldForEdit.HideNullable;
  chxUseNullableTypes.Checked := pFieldForEdit.UseNullableTypes;
  chkIsId.Checked := pFieldForEdit.FieldMapper.IdOptions.IsId;
  DoPanelIdEnabled(chkIsId.Checked);
  DoVerifyNullableComponents;
  if (chkIsId.Checked) then
  begin
    cbxIDType.ItemIndex := Ord(pFieldForEdit.FieldMapper.IdOptions.IdOption);
    edtSequenceName.Text := pFieldForEdit.FieldMapper.IdOptions.SequenceName;
    if pFieldForEdit.FieldMapper.IdOptions.Schemas.Count > 0 then
    begin
      edtOnSchemas.Text := pFieldForEdit.FieldMapper.IdOptions.Schemas.Items[0].Name;
      if (pFieldForEdit.FieldMapper.IdOptions.Schemas.Count > 1) then
      begin
        edtOnSchema2.Text := pFieldForEdit.FieldMapper.IdOptions.Schemas.Items[1].Name;
      end;
    end;
  end;

  if pFieldForEdit.FieldMapper.Schemas.Count > 0 then
  begin
    edtSchema1.Text := pFieldForEdit.FieldMapper.Schemas.Items[0].Name;
    if (pFieldForEdit.FieldMapper.Schemas.Count > 1) then
    begin
      edtSchema2.Text := pFieldForEdit.FieldMapper.Schemas.Items[1].Name;
    end;
  end;

  if (pFieldForEdit.FieldMapper.InheritsFrom(TEnumerationColumnMapper)) then
  begin
    cbxEnumName.Text := pFieldForEdit.TypeName;
    cbxEnumType.ItemIndex := Ord(TEnumerationColumnMapper(pFieldForEdit.FieldMapper).EnumType);
    for lIndex := Low(TEnumerationColumnMapper(pFieldForEdit.FieldMapper).EnumCharOf) to High(TEnumerationColumnMapper(pFieldForEdit.FieldMapper).EnumCharOf) do
    begin
      edtEnumCharValues.Text := edtEnumCharValues.Text  + ifthen(edtEnumCharValues.Text  <> '', ';') +
         TEnumerationColumnMapper(pFieldForEdit.FieldMapper).EnumCharOf[lIndex]
    end;
    VerifyEnumTypeComponents;
  end
  else if (pFieldForEdit.FieldMapper.InheritsFrom(TBooleanColumnMapper)) then
  begin
    cbxBoolType.Text := TBooleanColumnMapper(pFieldForEdit.FieldMapper).InternalColumnType.ToString;
    edtBoolTrue.Text := VarToStr(TBooleanColumnMapper(pFieldForEdit.FieldMapper).ValueTrue);
    edtBoolFalse.Text := VarToStr(TBooleanColumnMapper(pFieldForEdit.FieldMapper).ValueFalse);
    VerifyBoolTypeComponents;
  end;
  if pFieldForEdit.FieldMapper.InheritsFrom(TDetailColumnMapper) then
  begin
    edtDetailColumRefName.Text := TDetailColumnMapper(pFieldForEdit.FieldMapper).RefColumnName;
    chkDetailLazyLoad.Checked := pFieldForEdit.FieldMapper.LazyOptions.IsLazy;
    cbxDetailClassName.Text := pFieldForEdit.TypeName;
    if (chkDetailLazyLoad.Checked) then
    begin
      cbxDetailClassName.Text  := Copy(pFieldForEdit.TypeName, 10, Length(pFieldForEdit.TypeName) - 11);
    end;
    edtDetailListClassName.Text := pFieldForEdit.LazyClassName;
  end
  else if pFieldForEdit.FieldMapper.InheritsFrom(TJoinedColumnMapper) then
  begin
    edtJoinRefColumnName.Text := TJoinedColumnMapper(pFieldForEdit.FieldMapper).RefColumnName;
    cbxJoinClassName.Text := pFieldForEdit.LazyClassName;
    chkJoinLazyLoad.Checked := pFieldForEdit.FieldMapper.LazyOptions.IsLazy;
    cbxJoinKind.Text := TJoinedColumnMapper(pFieldForEdit.FieldMapper).JoinKind.ToSQL;
    cbxUpdateAction.Text := TJoinedColumnMapper(pFieldForEdit.FieldMapper).UpdateAction.ToDescription;
    cbxDeleteAction.Text := TJoinedColumnMapper(pFieldForEdit.FieldMapper).DeleteAction.ToDescription;
  end;
end;

procedure TfrmNewProperty.RefreshButtons;
begin
  btnFinish.Enabled := (pgcPropertySteps.ActivePageIndex = cSchemas) or
      ((pgcPropertySteps.ActivePageIndex = cEnumOptions) and not chkMappedAt.Checked);
  btnFinish.Default := btnFinish.Enabled;
  btnNext.Enabled := not btnFinish.Enabled;
  btnNext.Default := btnNext.Enabled;
  btnBack.Enabled := (pgcPropertySteps.ActivePageIndex > cName);
  btnFinish.Enabled := (pgcPropertySteps.ActivePageIndex >= cColumnOptions)
end;

procedure TfrmNewProperty.SetDescriptionText;
begin
  lblDescription.Caption := 'Specify the property definition';
  if (edtPropertyName.Text <> '') then
  begin
    lblDescription.Caption := lblDescription.Caption  + ': ' + edtPropertyName.Text;
  end;
end;

procedure TfrmNewProperty.tsBoolOptionsShow(Sender: TObject);
begin
  inherited;
  VerifyBoolTypeComponents;
end;

procedure TfrmNewProperty.tsEnumOptionsShow(Sender: TObject);
begin
  inherited;
  VerifyEnumTypeComponents;
end;

procedure TfrmNewProperty.ValidateMappingField;
begin
  if chkMappedAt.Checked then
  begin
    if Trim(edtMapping.Text) = '' then
    begin
      ShowBalloHintForControl(edtMapping, 'The ' + edtMapping.TextHint + ' is required!');
      Abort;
    end;
    if SameText(cbxType.Text, 'String') and (spedtSize.Value < 1) then
    begin
      ShowBalloHintForControl(spedtSize, 'The size value for this field is required!');
      Abort;
    end;
  end;
end;

procedure TfrmNewProperty.ValidatePropertyBoolOptions;
var
  lBool: Integer;
begin
  if (cbxBoolType.ItemIndex = 1) then
  begin
    if ((Trim(edtBoolTrue.Text) = '') or (Trim(edtBoolFalse.Text) = '')) then
    begin
      ShowBalloHintForControl(cbxBoolType, 'The value True and False for this field is required!');
      Abort;
    end
    else if TryStrToInt(edtBoolTrue.Text, lBool) <>  TryStrToInt(edtBoolFalse.Text, lBool) then
    begin
      ShowBalloHintForControl(cbxBoolType, 'The value True and False for this field is need to be at the same type!');
      Abort;
    end;
  end;
end;

procedure TfrmNewProperty.ValidatePropertyDetailOptions;
begin
  if (Trim(cbxDetailClassName.Text) = '') then
  begin
    ShowBalloHintForControl(cbxDetailClassName, 'The name of Detail Class is required!');
    Abort;
  end;
  if (Trim(edtDetailListClassName.Text) = '') then
  begin
    ShowBalloHintForControl(edtDetailListClassName, 'The name of Detail Class list is required!');
    Abort;
  end;
end;

procedure TfrmNewProperty.ValidatePropertyEnumOptions;
begin
  if Trim(cbxEnumName.Text) = '' then
  begin
    ShowBalloHintForControl(cbxEnumName, 'The name of Enumerator for this field is required!');
    Abort;
  end;
  if (cbxEnumType.ItemIndex = 1) and (Trim(edtEnumCharValues.Text) = '') then
  begin
    ShowBalloHintForControl(edtEnumCharValues, 'The Char values for this type of enumerator is required!');
    Abort;
  end;
end;

procedure TfrmNewProperty.ValidatePropertyIdOptions;
begin
  if chkIsId.Checked then
  begin
    if SameText(cbxIDType.Text, 'Sequence') and (Trim(edtSequenceName.Text) = '') then
    begin
      ShowBalloHintForControl(edtSequenceName, 'The sequence name is required!');
      Abort;
    end;
  end;
end;

procedure TfrmNewProperty.ValidatePropertyJoinOptions;
begin
  if (Trim(cbxJoinClassName.Text) = '') then
  begin
    ShowBalloHintForControl(cbxJoinClassName, 'The name of Join Class is required!');
    Abort;
  end;
end;

procedure TfrmNewProperty.ValidatePropertyName;
begin
  if (Trim(edtPropertyName.Text) = '') then
  begin
    ShowBalloHintForControl(edtPropertyName, 'The property name is required!');
    Abort;
  end;
  if (FDescriptor.FieldPropertyList.ContainsProperty(edtPropertyName.Text)) then
  begin
    ShowBalloHintForControl(edtPropertyName, Format('This class alread have a property named: %s!', [edtPropertyName.Text]));
    Abort;
  end;
end;

procedure TfrmNewProperty.ValidatePropertyType;
begin
  if SameText('Unknow', cbxType.Text) then
  begin
    ShowBalloHintForControl(cbxType, 'This data type is not valid for property/field definition!');
    Abort;
  end;
end;

end.
