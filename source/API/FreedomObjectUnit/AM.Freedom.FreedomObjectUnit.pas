unit AM.Freedom.FreedomObjectUnit;

interface

uses
  System.Classes,
  ToolsAPI,
  IniFiles,
  AM.Freedom.FreedomObjectDescriptor,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Attributes,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.ObjectMapper.ConstraintMapper,
  AM.Freedom.ObjectMapper.Schemas,
  AM.Freedom.ObjectMapper.ColumnOptions;

type
  TFreedomObjectUnit = class(TInterfacedObject, IOTAFile)
  private
    FFreedomSource: string;
    function GetAge: TDateTime;
    function GetSource: string;
  public
    property FreedomSource: string read FFreedomSource write FFreedomSource;
  end;

  TFreedomObjectUnitCreator = class(TInterfacedObject, IOTAModuleCreator)
  strict private const
    sDoubleLineBreak = sLineBreak + sLineBreak;
  strict private
    FUnitDescriptor: TFreedomUnitDescriptor;
    FObjectDescriptor: TFreedomClassDescriptor;
    FIniFile: TIniFile;
    function ToMappingCharCase(pString: string): string;
    function ToSchemaCharCase(pString: string): string;
    function HasLazy: Boolean;
    function HasLazyList: Boolean;
    function HasBlob: Boolean;
    function HasXML: Boolean;
    function HasNullable: Boolean;
    function HasTypedNullable: Boolean;
    function GenerateSource: string;
    function GenerateUsesList: string;
    function GenerateExternalUses: string;
    function GetObjectCursorClassIntefaceDeclaration: String;
    function GetClassAttributes: string;
    function GetEntityDeclaration: string;
    function GetPrimaryDefinition(pPrimary: TPrimaryMapper): string;
    function GetUniqueDefinition(pUnique: TUniqueMapper): string;
    function GetForeignDefinition(pForeign: TForeignMapper): string;

    function GetSchemaDefinition(pSchemaItem: TSchemaItem): string;
    function GenerateClassBody: string;
    function GenerateFields: string;
    function GetAttributeName(pColumn: TCustomColumnMapper): string;
    function GetAttributeValues(pColumn: TCustomColumnMapper): string;
    function GetValuesFromBooleanColumn(pColumn: TBooleanColumnMapper): string;
    function GetValuesFromEnumerationColumn(pColumn: TEnumerationColumnMapper): string;
    function GetValuesFromJoinedColumn(pColumn: TJoinedColumnMapper): string;
    function GetValuesFromDetailColumn(pColumn: TDetailColumnMapper): string;
    function GetValuesFromColumn(pColumn: TCustomColumnMapper): string;
    function GetIdOptions(pColumn: TCustomColumnMapper): string;
    function GetDomain(pColumn: TCustomColumnMapper): string;
    function GetDefaultValue(pColumn: TCustomColumnMapper): string;
    function GetOrder(pColumn: TCustomColumnMapper): string;
    function GenerateOrderDefinition(pOrderOptions: TOrderOptions): string;
    function GetSchemas(pColumn: TCustomColumnMapper): string;
    function ColumnOptionsToString(pColumnOptions: TColumnOptions): string;
    function GetPropertyReadDeclarations: string;
    function GenerateProperties: string;
    function GetPropertyDeclaration(pDescriptor: TFieldPropertyDescriptor): string;
    function GetConstructorDeclaration: string;
    function HasConstructor: Boolean;
    function GetObjectCursorClassImplemetation: String;
    function GetClassImplemetation: string;
    function GetConstructorImplementation: string;
    function GetDestructorImplementation: string;
    function GetLazyClassDeclaration(pDescriptor: TFieldPropertyDescriptor): string;
    function GetNullableFunctionImplementation(pDescriptor: TFieldPropertyDescriptor): string;
    function GetNullableProcedureImplementation(pDescriptor: TFieldPropertyDescriptor): string;
    function GetFieldFunctionImplementation(pDescriptor: TFieldPropertyDescriptor): string;
    procedure WriteSectionUnitName;
    procedure SetUnitDescriptor(const Value: TFreedomUnitDescriptor);
  private
    function GetAncestorName: string;
    function GetFormName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    function NewFormFile(const FormIdent: string; const AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent: string; const FormIdent: string; const AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent: string; const FormIdent: string; const AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  public
    constructor Create;
    destructor Destroy; override;
    property UnitDescriptor: TFreedomUnitDescriptor read FUnitDescriptor write SetUnitDescriptor;
  end;

implementation

uses
  System.Variants,
  System.StrUtils,
  System.SysUtils,
  AM.Freedom.Helper.OrderType,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.Helper.ForeignOption,
  AM.Freedom.dclFreedomORMConfig;

{ TFreedomObjectUnitCreator }

function TFreedomObjectUnitCreator.ColumnOptionsToString(pColumnOptions: TColumnOptions): string;
begin
  Result := '';
  if Required in pColumnOptions then
  begin
    Result := 'Required';
  end;
  if NoInsert in pColumnOptions then
  begin
    Result := Result + ifthen(Result <> '', ', ') + 'NoInsert';
  end;
  if NoUpdate in pColumnOptions then
  begin
    Result := Result + ifthen(Result <> '', ', ') + 'NoUpdate';
  end;
  if NoDelete in pColumnOptions then
  begin
    Result := Result + ifthen(Result <> '', ', ') + 'NoDelete';
  end;
  Result := '[' + Result + ']';
end;

constructor TFreedomObjectUnitCreator.Create;
var
  lCurrentProject: IOTAProject;
begin
  if (Assigned(FIniFile)) then
  begin
    FIniFile.Free;
  end;
  lCurrentProject := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
  FIniFile := TIniFile.Create(TdclFreedomORMConfig.GetInstance.IniFileDirectory + 'FreedomORM.cfg');
end;

destructor TFreedomObjectUnitCreator.Destroy;
begin
  WriteSectionUnitName;
  FIniFile.Free;
  inherited;
end;

procedure TFreedomObjectUnitCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

function TFreedomObjectUnitCreator.GenerateClassBody: string;
begin
  Result := GenerateFields;
  if Result <> '' then
  begin
    Result := Result + sLineBreak;
  end;
  Result := Result + GenerateProperties;
end;

function TFreedomObjectUnitCreator.GenerateExternalUses: string;
var
  lClass: TFreedomClassDescriptor;
  lProperty: TFieldPropertyDescriptor;
  procedure AddToResult(pUnitName: string);
  begin
    if (Trim(pUnitName) <> '') and not ContainsText(Result, pUnitName) and
       (not SameText(FUnitDescriptor.NewUnitName, pUnitName)) then
    begin
      Result := Result + ifthen(Result <> '', ',' + sLineBreak + '  ') + pUnitName;
    end;
  end;
begin
  Result := '';
  for lClass in FUnitDescriptor do
  begin
    AddToResult(lClass.NewBaseClassUnit);
    AddToResult(lClass.BaseClassListUnit);
    AddToResult(lClass.ObjectCursorClassUnitName);
    if (lClass.InheritsObjectCursorClassName = 'TCustomObjectCursor') then
    begin
      AddToResult('AM.Freedom.ObjectMapper.CustomObjectCursor');
      AddToResult('AM.Freedom.ICursor');
      AddToResult('AM.Freedom.IPersistent');
    end
    else if (lClass.InheritsObjectCursorClassName = 'TCustomDBObjectCursor') then
    begin
      AddToResult('AM.Freedom.ObjectMapper.CustomDBObjectCursor');
      AddToResult('AM.Freedom.ICursor');
      AddToResult('AM.Freedom.SQLMapper.CustomArgument');
    end;
    for lProperty in lClass.FieldPropertyList do
    begin
      AddToResult(lProperty.TypeNameUnit);
    end;
  end;
end;

function TFreedomObjectUnitCreator.GenerateFields: string;
var
  lDescriptor: TFieldPropertyDescriptor;
  lAttribute: string;
  lColumnAttributes: string;
  procedure AddColumnAttribute(lAttribute: string);
  begin
    if lAttribute <> '' then
    begin
      lColumnAttributes := lColumnAttributes + ifthen(lColumnAttributes <> '', sLineBreak + '    ') + lAttribute;
    end;
  end;

begin
  Result := '';
  if FObjectDescriptor.ObjectMapper.Columns.Count > 0 then
  begin
    for lDescriptor in FObjectDescriptor.FieldPropertyList do
    begin
      lColumnAttributes := '';
      if lDescriptor.FieldMapper <> nil then
      begin
        lAttribute := Format('    [%s(%s)]', [GetAttributeName(lDescriptor.FieldMapper), GetAttributeValues(lDescriptor.FieldMapper)]);
        AddColumnAttribute(lAttribute);
        lAttribute := GetIdOptions(lDescriptor.FieldMapper);
        AddColumnAttribute(lAttribute);
        lAttribute := GetDomain(lDescriptor.FieldMapper);
        AddColumnAttribute(lAttribute);
        lAttribute := GetDefaultValue(lDescriptor.FieldMapper);
        AddColumnAttribute(lAttribute);
        lAttribute := GetOrder(lDescriptor.FieldMapper);
        AddColumnAttribute(lAttribute);
        lAttribute := GetSchemas(lDescriptor.FieldMapper);
        AddColumnAttribute(lAttribute);
      end;
      Result := Result + ifthen(Result <> '', sLineBreak) + ifthen(lColumnAttributes <> '', lColumnAttributes + sLineBreak);
      if (lDescriptor.FieldMapper.IsNullable) then
      begin
        if (lDescriptor.UseNullableTypes) then
        begin
          Result := Result + Format('    F%s: T%sNullable;', [lDescriptor.PropertyMapper.Name, lDescriptor.TypeName]);
        end
        else
        begin
          Result := Result + Format('    F%s: TNullable<%s>;', [lDescriptor.PropertyMapper.Name, lDescriptor.TypeName]);
        end;
      end
      else
      begin
        Result := Result + Format('    F%s: %s;', [lDescriptor.PropertyMapper.Name, lDescriptor.TypeName]);
      end;
    end;
    Result := Result + GetPropertyReadDeclarations;
    if Result <> '' then
    begin
      Result := '  strict private' + sLineBreak + Result;
    end;
  end;
end;

function TFreedomObjectUnitCreator.GenerateOrderDefinition(pOrderOptions: TOrderOptions): string;
begin
  Result := IntToStr(pOrderOptions.Index);
  if (pOrderOptions.OrderType <> TOrderType.Asc) or (Trim(pOrderOptions.Directive) <> '') then
  begin
    Result := Result + ', ' + pOrderOptions.OrderType.ToString;
  end;
  if (Trim(pOrderOptions.Directive) <> '') then
  begin
    Result := Result + ', ' + QuotedStr(pOrderOptions.Directive);
  end;
end;

function TFreedomObjectUnitCreator.GenerateProperties: string;
var
  lDescriptor: TFieldPropertyDescriptor;
  lProperties, lConstructor: string;
begin
  for lDescriptor in FObjectDescriptor.FieldPropertyList do
  begin
    lProperties := lProperties + ifthen(lProperties <> '', sLineBreak) + GetPropertyDeclaration(lDescriptor);
  end;
  lConstructor := GetConstructorDeclaration;
  Result := '  public' + sLineBreak + ifthen(lConstructor <> '', lConstructor + sLineBreak) + lProperties;
end;

function TFreedomObjectUnitCreator.GenerateSource: string;
var
  lObjectDescriptor: TFreedomClassDescriptor;
  lCursorClassDeclaration: string;
begin
  Result := 'unit ' + FUnitDescriptor.NewUnitName + ';' + sDoubleLineBreak +
    'interface' + sDoubleLineBreak +
    'uses' + sLineBreak + '  ' + GenerateUsesList + ';' + sDoubleLineBreak +
    'type' + sLineBreak;
  for lObjectDescriptor in FUnitDescriptor do
  begin
    FObjectDescriptor := lObjectDescriptor;
    lCursorClassDeclaration := GetObjectCursorClassIntefaceDeclaration;
    if (lCursorClassDeclaration <> '') then
    begin
      Result := Result + lCursorClassDeclaration;
    end;
  end;
  for lObjectDescriptor in FUnitDescriptor do
  begin
    FObjectDescriptor := lObjectDescriptor;
    Result := Result + GetClassAttributes + '  ' +
        FObjectDescriptor.NewClassName + ' = class(' + FObjectDescriptor.NewBaseClass + ')' + sLineBreak +
        GenerateClassBody + sLineBreak +
        '  end;' + sDoubleLineBreak +
        ifthen(FObjectDescriptor.ListClassName <> '', '  ' +
        FObjectDescriptor.ListClassName + ' = class(' + FObjectDescriptor.BaseClassListName + '<' +
        FObjectDescriptor.NewClassName + '>);' + sDoubleLineBreak);
  end;
  Result := Result + 'implementation' + sLineBreak;
  FObjectDescriptor := nil;
  for lObjectDescriptor in FUnitDescriptor do
  begin
    if (not Assigned(FObjectDescriptor)) or (FObjectDescriptor.ObjectCursorClassName <> lObjectDescriptor.ObjectCursorClassName) then
    begin
      FObjectDescriptor := lObjectDescriptor;
      lCursorClassDeclaration := GetObjectCursorClassImplemetation;
      if (lCursorClassDeclaration <> '') then
      begin
        Result := Result + lCursorClassDeclaration;
      end;
    end;
  end;
  FObjectDescriptor := nil;
  for lObjectDescriptor in FUnitDescriptor do
  begin
    if (not Assigned(FObjectDescriptor)) or (FObjectDescriptor.NewClassName <> lObjectDescriptor.NewClassName) then
    begin
      FObjectDescriptor := lObjectDescriptor;
      Result := Result + GetClassImplemetation;
    end;
  end;
  Result := Result + sLineBreak + 'end.';
end;

function TFreedomObjectUnitCreator.GenerateUsesList: string;
var
  lExternal: string;
begin
  Result := 'System.SysUtils,' + sLineBreak +
    '  AM.Freedom.Attributes,' + sLineBreak +
    '  AM.Freedom.EnumerationTypes' +
    ifthen(HasLazy, ',' + sLineBreak + '  AM.Freedom.Lazy') +
    ifthen(HasLazyList, ',' + sLineBreak + '  AM.Freedom.LazyList') +
    ifthen(HasNullable, ',' + sLineBreak + '  AM.Freedom.Nullable') +
    ifthen(HasTypedNullable, ',' + sLineBreak + '  AM.Freedom.Nullable.Types') +
    ifthen(HasBlob, ',' + sLineBreak + '  System.Classes') +
    ifthen(HasXML, ',' + sLineBreak + '  AM.Freedom.XML');
  lExternal := GenerateExternalUses;
  if (lExternal <> '') then
  begin
    Result := Result + ',' + sLineBreak + '  ' + lExternal;
  end;
end;

function TFreedomObjectUnitCreator.GetAncestorName: string;
begin
  Result := FUnitDescriptor.First.NewBaseClass;
end;

function TFreedomObjectUnitCreator.GetAttributeName(pColumn: TCustomColumnMapper): string;
begin
  Result := Column.ClassName;
  if pColumn.InheritsFrom(TBooleanColumnMapper) then
  begin
    Result := BooleanColumn.ClassName;
  end
  else if pColumn.InheritsFrom(TEnumerationColumnMapper) then
  begin
    Result := EnumerationColumn.ClassName;
  end
  else if pColumn.InheritsFrom(TBlobColumnMapper) then
  begin
    Result := BlobColumn.ClassName;
  end
  else if pColumn.InheritsFrom(TJoinedColumnMapper) then
  begin
    Result := JoinedColumn.ClassName;
  end
  else if pColumn.InheritsFrom(TDetailColumnMapper) then
  begin
    Result := DetailColumn.ClassName;
  end;
end;

function TFreedomObjectUnitCreator.GetAttributeValues(pColumn: TCustomColumnMapper): string;
begin
  if pColumn.InheritsFrom(TBooleanColumnMapper) then
  begin
    Result := GetValuesFromBooleanColumn(TBooleanColumnMapper(pColumn));
  end
  else if pColumn.InheritsFrom(TEnumerationColumnMapper) then
  begin
    Result := GetValuesFromEnumerationColumn(TEnumerationColumnMapper(pColumn));
  end
  else if pColumn.InheritsFrom(TJoinedColumnMapper) then
  begin
    Result := GetValuesFromJoinedColumn(TJoinedColumnMapper(pColumn));
  end
  else if pColumn.InheritsFrom(TDetailColumnMapper) then
  begin
    Result := GetValuesFromDetailColumn(TDetailColumnMapper(pColumn));
  end
  else
  begin
    Result := GetValuesFromColumn(pColumn);
  end;
  Result := QuotedStr(pColumn.Name) + ifthen(Result <> '', ', ') + Result;
  if pColumn.Alias <> '' then
  begin
    Result := Result + ifthen(Result <> '', ', ') + QuotedStr(ToMappingCharCase(pColumn.Alias));
  end;
end;

function TFreedomObjectUnitCreator.GetClassAttributes: string;
var
  lUnique: TUniqueMapper;
  lSchemaItem: TSchemaItem;
  lForeign: TForeignMapper;
begin
  Result := '';
  if (FObjectDescriptor.ObjectMapper.Name <> '') then
  begin
    Result := Format('  [Entity(%s)]', [GetEntityDeclaration]);
    if (FObjectDescriptor.ObjectMapper.Primarys.Count > 0) then
    begin
      Result := Result + sLineBreak + Format('  [Primary(%s)]', [GetPrimaryDefinition(FObjectDescriptor.ObjectMapper.Primarys.Items[0])]);
    end;
    if (FObjectDescriptor.ObjectMapper.Uniques.Count > 0) then
    begin
      for lUnique in FObjectDescriptor.ObjectMapper.Uniques do
      begin
        Result := Result + sLineBreak + Format('  [Unique(%s)]', [GetUniqueDefinition(lUnique)]);
      end;
    end;
    if (FObjectDescriptor.ObjectMapper.Foreigns.Count > 0) then
    begin
      for lForeign in FObjectDescriptor.ObjectMapper.Foreigns do
      begin
        Result := Result + sLineBreak + Format('  [Foreign(%s)]', [GetForeignDefinition(lForeign)]);
      end;
    end;
    if (FObjectDescriptor.ObjectMapper.Schemas.Count > 0) then
    begin
      for lSchemaItem in FObjectDescriptor.ObjectMapper.Schemas do
      begin
        Result := Result + sLineBreak + Format('  [Schema(%s)]', [GetSchemaDefinition(lSchemaItem)]);
      end;
    end;
  end
  else if (FObjectDescriptor.ObjectCursorClassName <> '') then
  begin
    Result := Format('  [Cursor(%s)]', [FObjectDescriptor.ObjectCursorClassName]);
  end;
  if Result <> '' then
  begin
    Result := Result + sLineBreak;
  end;
end;

function TFreedomObjectUnitCreator.GetClassImplemetation: string;
var
  lDescriptor: TFieldPropertyDescriptor;
  lTypeName: string;
begin
  if HasConstructor then
  begin
    Result := Format('%s{ %s }%s%s', [sLineBreak, FObjectDescriptor.NewClassName, sLineBreak, sLineBreak]);
    Result := Result + Format('constructor %s.Create;%sbegin%s%s%send;%s', [FObjectDescriptor.NewClassName, sLineBreak, sLineBreak, GetConstructorImplementation, sLineBreak, sLineBreak]);
    Result := Result + sLineBreak + Format('destructor %s.Destroy;%sbegin%s%s%send;%s', [FObjectDescriptor.NewClassName, sLineBreak, sLineBreak, GetDestructorImplementation, sLineBreak, sLineBreak]);
    for lDescriptor in FObjectDescriptor.FieldPropertyList do
    begin
      if (lDescriptor.LazyClassName <> '') then
      begin
        Result := Result + sLineBreak + Format('function %s.Get%s: %s;%sbegin%s%s%send;%s', [FObjectDescriptor.NewClassName,
            lDescriptor.PropertyMapper.Name, lDescriptor.LazyClassName, sLineBreak, sLineBreak, GetLazyClassDeclaration(lDescriptor),
          sLineBreak, sLineBreak]);
      end
      else if (lDescriptor.HideNullable) then
      begin
        Result := Result + sLineBreak + Format('function %s.Get%s: %s;%sbegin%s%s%send;%s', [FObjectDescriptor.NewClassName,
            lDescriptor.PropertyMapper.Name, lDescriptor.TypeName, sLineBreak, sLineBreak, GetNullableFunctionImplementation(lDescriptor),
          sLineBreak, sLineBreak]);
        Result := Result + sLineBreak + Format('procedure %s.Set%s(p%s: %s);%sbegin%s%s%send;%s', [FObjectDescriptor.NewClassName,
            lDescriptor.PropertyMapper.Name, lDescriptor.PropertyMapper.Name, lDescriptor.TypeName,
            sLineBreak, sLineBreak, GetNullableProcedureImplementation(lDescriptor),
          sLineBreak, sLineBreak]);
      end
      else if (lDescriptor.FieldMapper.ColumnType.IsBlob) then
      begin
        lTypeName := lDescriptor.TypeName;
        if (lDescriptor.FieldMapper.ColumnType = ctyMemo) then
        begin
          lTypeName := 'TStrings';
        end;
        Result := Result + sLineBreak + Format('function %s.Get%s: %s;%sbegin%s%s%send;%s', [FObjectDescriptor.NewClassName,
            lDescriptor.PropertyMapper.Name, lTypeName, sLineBreak, sLineBreak, GetFieldFunctionImplementation(lDescriptor),
          sLineBreak, sLineBreak]);
      end;
    end;
  end
  else
  begin
    Result := '';
  end;
end;

function TFreedomObjectUnitCreator.GetConstructorDeclaration: string;
begin
  Result := '';
  if HasConstructor then
  begin
    Result := '    constructor Create; override;';
    Result := Result + sLineBreak + '    destructor Destroy; override;';
  end;
end;

function TFreedomObjectUnitCreator.GetConstructorImplementation: string;
var
  lProperty: TFieldPropertyDescriptor;
  lTypeName: string;
begin
  Result := '  inherited;';
  for lProperty in FObjectDescriptor.FieldPropertyList do
  begin
    if (lProperty.FieldMapper.ColumnType in [ctyXML, ctyExtension, ctyDetail, ctyBlob, ctyMemo, ctyJoin]) or lProperty.FieldMapper.IsNullable then
    begin
      Result := Result + ifthen(Result <> '', sLineBreak);
      if lProperty.TypeName = 'TStrings' then
      begin
        lTypeName := 'TStringList';
      end
      else if lProperty.TypeName = 'TStream' then
      begin
        lTypeName := 'TMemoryStream';
      end
      else if lProperty.TypeName = 'TXML' then
      begin
        lTypeName := 'TXML';
      end
      else
      begin
        lTypeName := lProperty.TypeName;
      end;
      if (lProperty.FieldMapper.IsNullable) then
      begin
        if lProperty.UseNullableTypes then
        begin
          Result := Result + Format('  F%s := T%sNullable.Create;', [lProperty.PropertyMapper.Name, lTypeName]);
        end
        else
        begin
          Result := Result + Format('  F%s := TNullable<%s>.Create;', [lProperty.PropertyMapper.Name, lTypeName]);
        end;
      end
      else
      begin
        Result := Result + Format('  F%s := %s.Create;', [lProperty.PropertyMapper.Name, lTypeName]);
      end;
    end;
  end;
end;

function TFreedomObjectUnitCreator.GetCreatorType: string;
begin
end;

function TFreedomObjectUnitCreator.GetDefaultValue(pColumn: TCustomColumnMapper): string;
var
  lIntValue: Integer;
begin
  Result := '';
  if (pColumn.DefaultValueOptions.IsNow) then
  begin
    Result := '[DefaultNowValue]';
  end
  else if not(VarIsNull(pColumn.DefaultValueOptions.Value)) then
  begin
    if (TryStrToInt(pColumn.DefaultValueOptions.Value, lIntValue)) then
    begin
      Result := Format('[DefaultValue(%s)]', [pColumn.DefaultValueOptions.Value]);
    end
    else
    begin
      Result := Format('[DefaultValue(%s)]', [QuotedStr(pColumn.DefaultValueOptions.Value)]);
    end;
  end;
end;

function TFreedomObjectUnitCreator.GetDestructorImplementation: string;
var
  lProperty: TFieldPropertyDescriptor;
begin
  Result := '';
  for lProperty in FObjectDescriptor.FieldPropertyList do
  begin
    if (lProperty.FieldMapper.ColumnType in [ctyDetail, ctyXML, ctyExtension, ctyBlob, ctyMemo, ctyJoin]) or lProperty.FieldMapper.IsNullable then
    begin
      Result := Result + ifthen(Result <> '', sLineBreak) + Format('  F%s.Free;', [lProperty.PropertyMapper.Name]);
    end;
  end;
  Result := Result + ifthen(Result <> '', sLineBreak) + '  inherited;';
end;

function TFreedomObjectUnitCreator.GetDomain(pColumn: TCustomColumnMapper): string;
begin
  Result := '';
  if (pColumn.Domain <> '') then
  begin
    Result := Format('[Domain(%s)]', [QuotedStr(ToMappingCharCase(pColumn.Domain))]);
  end;
end;

function TFreedomObjectUnitCreator.GetEntityDeclaration: string;
begin
  Result := QuotedStr(FObjectDescriptor.ObjectMapper.Name);
  if FObjectDescriptor.ObjectMapper.Alias <> '' then
  begin
    Result := Result + ', ' + QuotedStr(FObjectDescriptor.ObjectMapper.Alias);
  end;
end;

function TFreedomObjectUnitCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TFreedomObjectUnitCreator.GetFieldFunctionImplementation(pDescriptor: TFieldPropertyDescriptor): string;
begin
  Result := Format('  Result := F%s;', [pDescriptor.PropertyMapper.Name]);
end;

function TFreedomObjectUnitCreator.GetFileSystem: string;
begin
end;

function TFreedomObjectUnitCreator.GetForeignDefinition(pForeign: TForeignMapper): string;
var
  lInternalResult: String;
  lString, lSchemas: string;
begin
  Result := '';
  lInternalResult := '';
  lSchemas := '';

  for lString in pForeign.Columns do
  begin
    lInternalResult := lInternalResult + ifthen(lInternalResult <> '', ';') + lString;
  end;
  Result := QuotedStr(lInternalResult);
  Result := Result + ', ' + QuotedStr(pForeign.ReferencesTo);

  lInternalResult := '';
  for lString in pForeign.ReferencesColumns do
  begin
    lInternalResult := lInternalResult + ifthen(lInternalResult <> '', ';') + lString;
  end;

  if pForeign.Schemas.Count > 0 then
  begin
    for lString in pForeign.Schemas do
    begin
      lSchemas := lSchemas + ifthen(lSchemas <> '', ';') + ToSchemaCharCase(lString);
    end;
  end;

  if (lInternalResult <> '') or (pForeign.OnUpdate <> NoAction) or (pForeign.OnDelete <> NoAction) or (lSchemas <> '') then
  begin
    Result := Result + ', ' + QuotedStr(lInternalResult);
  end;

  if (pForeign.OnUpdate <> NoAction) or (pForeign.OnDelete <> NoAction) or (lSchemas <> '') then
  begin
    Result := Result + ', ' + pForeign.OnUpdate.ToDescription;
  end;

  if (pForeign.OnDelete <> NoAction) or (lSchemas <> '') then
  begin
    Result := Result + ', ' + pForeign.OnDelete.ToDescription;
  end;

  if (lSchemas <> '') then
  begin
    Result := Result + ', ' + QuotedStr(lSchemas);
  end;
end;

function TFreedomObjectUnitCreator.GetFormName: string;
begin
  Result := '';
end;

function TFreedomObjectUnitCreator.GetIdOptions(pColumn: TCustomColumnMapper): string;
  function GetInternalIdOptions: string;
  var
    lSchema: TSchemaItem;
    lSchemas: string;
  begin
    Result := '';
    case pColumn.IdOptions.IdOption of
      TIdOption.None:
        Result := '(None, ''''';
      TIdOption.Identity:
        if pColumn.IdOptions.Schemas.Count > 0 then
        begin
          Result := '(Identity, ''''';
        end;
      TIdOption.Sequence:
        Result := '(Sequence, ' + QuotedStr(ToMappingCharCase(pColumn.IdOptions.SequenceName));
    end;
    if pColumn.IdOptions.Schemas.Count > 0 then
    begin
      for lSchema in pColumn.IdOptions.Schemas do
      begin
        lSchemas := lSchemas + ifthen(lSchemas <> '', ';') + ToSchemaCharCase(lSchema.Name);
      end;
      Result := Result + ', ' + QuotedStr(lSchemas);
    end;
    case pColumn.IdOptions.IdOption of
      TIdOption.None, TIdOption.Sequence:
        Result := Result + ')';
      TIdOption.Identity:
        if pColumn.IdOptions.Schemas.Count > 0 then
        begin
          Result := Result + ')';
        end;
    end;
  end;

begin
  Result := '';
  if pColumn.IdOptions.IsId then
  begin
    Result := Format('[Id%s]', [GetInternalIdOptions]);
  end;
end;

function TFreedomObjectUnitCreator.GetImplFileName: string;
var
  lCurrentProject: IOTAProject;
begin
  Result := FUnitDescriptor.NewUnitName;
  if not EndsText('.pas', Result) then
  begin
    Result := Result + '.pas';
  end;
  if ExtractFilePath(Result) = '' then
  begin
    lCurrentProject := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
    Result := ExtractFilePath(lCurrentProject.FileName) + Result;
  end;
end;

function TFreedomObjectUnitCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TFreedomObjectUnitCreator.GetLazyClassDeclaration(pDescriptor: TFieldPropertyDescriptor): string;
begin
  case pDescriptor.FieldMapper.LazyOptions.LazyType of
    List:
      Result := Format('  Result := %s(F%s.Value);', [pDescriptor.LazyClassName, pDescriptor.PropertyMapper.Name]);
  else
    Result := Format('  Result := F%s.Value;', [pDescriptor.PropertyMapper.Name]);
  end;
end;

function TFreedomObjectUnitCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TFreedomObjectUnitCreator.GetNullableFunctionImplementation(pDescriptor: TFieldPropertyDescriptor): string;
begin
  Result := Format('  Result := F%s.Value;', [pDescriptor.PropertyMapper.Name]);
end;

function TFreedomObjectUnitCreator.GetNullableProcedureImplementation(pDescriptor: TFieldPropertyDescriptor): string;
begin
  Result := Format('  F%s.Value := p%s;', [pDescriptor.PropertyMapper.Name, pDescriptor.PropertyMapper.Name]);
end;

function TFreedomObjectUnitCreator.GetObjectCursorClassImplemetation: String;
begin
  Result := '';
  if (FObjectDescriptor.InheritsObjectCursorClassName <> '') and
     ContainsText('TCustomObjectCursor;TCustomDBObjectCursor', FObjectDescriptor.InheritsObjectCursorClassName) then
  begin
    Result := Format('%s{%s}%s', [sLineBreak, FObjectDescriptor.ObjectCursorClassName, sDoubleLineBreak]);
    if SameText('TCustomObjectCursor', FObjectDescriptor.InheritsObjectCursorClassName) then
    begin
      Result := Result + Format('function %s:0.ExtractCursor(pPersistent: IPersistent): ICursor;' + sLineBreak +
          'begin' + sLineBreak +
          '  do that;' + sLineBreak +
          'end;' + sDoubleLineBreak +
          'function %s:0.ExtractCursorList(pPersistent: IPersistent): TCursorList;' + sLineBreak +
          'begin' + sLineBreak +
          '  do that;' + sLineBreak +
          'end;', [FObjectDescriptor.ObjectCursorClassName]);
    end
    else
    if SameText('TCustomDBObjectCursor', FObjectDescriptor.InheritsObjectCursorClassName) then
    begin
      Result := Result + Format('function %s.GetSQLCommand: TCustomCommand;' + sLineBreak +
          'begin' + sLineBreak +
          '  do that;' + sLineBreak +
          'end;', [FObjectDescriptor.ObjectCursorClassName]);
    end;
    Result := Result + sDoubleLineBreak;
  end;
end;

function TFreedomObjectUnitCreator.GetObjectCursorClassIntefaceDeclaration: String;
begin
  Result := '';
  if (FObjectDescriptor.InheritsObjectCursorClassName <> '') and
     ContainsText('TCustomObjectCursor;TCustomDBObjectCursor', FObjectDescriptor.InheritsObjectCursorClassName) then
  begin
    Result := Format('  %s = class(%s)%s', [FObjectDescriptor.ObjectCursorClassName,
        FObjectDescriptor.InheritsObjectCursorClassName, sLineBreak]);
    if SameText('TCustomObjectCursor', FObjectDescriptor.InheritsObjectCursorClassName) then
    begin
      Result := Result + '  public' +
          '    function ExtractCursor(pPersistent: IPersistent): ICursor; override;' +
          '    function ExtractCursorList(pPersistent: IPersistent): TCursorList; override;' +
          '  end;';
    end
    else
    if SameText('TCustomDBObjectCursor', FObjectDescriptor.InheritsObjectCursorClassName) then
    begin
      Result := Result + '  strict protected' + sLineBreak +
          '    function GetSQLCommand: TCustomCommand; override;' + sLineBreak +
          '  end;';
    end;
    Result := Result + sDoubleLineBreak;
  end;
end;

function TFreedomObjectUnitCreator.GetOrder(pColumn: TCustomColumnMapper): string;
begin
  Result := '';
  if (pColumn.OrderOptions.Index > 0) then
  begin
    Result := Format('[Order(%s)]', [GenerateOrderDefinition(pColumn.OrderOptions)]);
  end;
end;

function TFreedomObjectUnitCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TFreedomObjectUnitCreator.GetPrimaryDefinition(pPrimary: TPrimaryMapper): string;
var
  lString: string;
  lSchemas: string;
begin
  Result := '';
  for lString in pPrimary.Columns do
  begin
    Result := Result + ifthen(Result <> '', ';') + lString;
  end;
  Result := QuotedStr(Result);
  lSchemas := '';
  if pPrimary.Schemas.Count > 0 then
  begin
    for lString in pPrimary.Schemas do
    begin
      lSchemas := lSchemas + ifthen(lSchemas <> '', ';') + ToSchemaCharCase(lString);
    end;
    Result := Result + ', ' + QuotedStr(lSchemas);
  end;
end;

function TFreedomObjectUnitCreator.GetPropertyDeclaration(pDescriptor: TFieldPropertyDescriptor): string;
begin
  Result := '    property ' + pDescriptor.PropertyMapper.Name + ': ';
  if (pDescriptor.LazyClassName = '') and (not pDescriptor.FieldMapper.ColumnType.IsBlob) then
  begin
    if (not pDescriptor.FieldMapper.IsNullable) then
    begin
      Result := Result + pDescriptor.TypeName + ' read F' + pDescriptor.PropertyMapper.Name + ' write F' + pDescriptor.PropertyMapper.Name + ';';
    end
    else
    begin
      if (pDescriptor.HideNullable) then
      begin
        Result := Result + pDescriptor.TypeName + ' read Get' + pDescriptor.PropertyMapper.Name + ' write Set' + pDescriptor.PropertyMapper.Name + ';';
      end
      else
      begin
        if pDescriptor.UseNullableTypes then
        begin
          Result := Result + Format('T%sNullable read F%s;', [pDescriptor.TypeName, pDescriptor.PropertyMapper.Name]);
        end
        else
        begin
          Result := Result + Format('TNullable<%s> read F%s;', [pDescriptor.TypeName, pDescriptor.PropertyMapper.Name]);
        end;
      end;
    end;
  end
  else
  begin
    if pDescriptor.FieldMapper.ColumnType.IsBlob then
    begin
      Result := Result + pDescriptor.TypeName + ' read Get' + pDescriptor.PropertyMapper.Name + ';';
    end
    else
    begin
      Result := Result + pDescriptor.LazyClassName + ' read Get' + pDescriptor.PropertyMapper.Name + ';';
    end;

  end;
end;

function TFreedomObjectUnitCreator.GetPropertyReadDeclarations: string;
var
  lDescriptor: TFieldPropertyDescriptor;
  lTypeName: String;
begin
  Result := '';
  for lDescriptor in FObjectDescriptor.FieldPropertyList do
  begin
    if (lDescriptor.LazyClassName <> '') then
    begin
      Result := Result + ifthen(Result <> '', sLineBreak) + '    function Get' + lDescriptor.PropertyMapper.Name + ': ' +
          lDescriptor.LazyClassName + ';';
    end
    else if lDescriptor.HideNullable then
    begin
      Result := Result + ifthen(Result <> '', sLineBreak) +
          Format('    function Get%s: %s;', [lDescriptor.PropertyMapper.Name, lDescriptor.TypeName]);
      Result := Result + ifthen(Result <> '', sLineBreak) +
          Format('    procedure Set%s(p%s: %s);', [lDescriptor.PropertyMapper.Name, lDescriptor.PropertyMapper.Name,
          lDescriptor.TypeName]);
    end
    else if lDescriptor.FieldMapper.ColumnType.IsBlob then
    begin
      lTypeName := lDescriptor.TypeName;
      if (lDescriptor.FieldMapper.ColumnType = ctyMemo) then
      begin
        lTypeName := 'TStrings';
      end;
      Result := Result + ifthen(Result <> '', sLineBreak) +
          Format('    function Get%s: %s;', [lDescriptor.PropertyMapper.Name, lTypeName]);
    end;
  end;
  if (Result <> '') then
  begin
    Result := sLineBreak + Result;
  end;
end;

function TFreedomObjectUnitCreator.GetSchemaDefinition(pSchemaItem: TSchemaItem): string;
begin
  Result := QuotedStr(ToSchemaCharCase(pSchemaItem.Name)) + ', ' + ifthen(pSchemaItem.Default, 'True', 'False');
end;

function TFreedomObjectUnitCreator.GetSchemas(pColumn: TCustomColumnMapper): string;
var
  lSchema: TSchemaItem;
begin
  Result := '';
  if pColumn.Schemas.Count > 0 then
  begin
    for lSchema in pColumn.Schemas do
    begin
      Result := Result + ifthen(Result <> '', sLineBreak + '    ') + Format('[Schema(%s)]', [GetSchemaDefinition(lSchema)]);
    end;
  end;
end;

function TFreedomObjectUnitCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TFreedomObjectUnitCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TFreedomObjectUnitCreator.GetUniqueDefinition(pUnique: TUniqueMapper): string;
var
  lString: string;
  lSchemas: string;
begin
  Result := '';
  for lString in pUnique.Columns do
  begin
    Result := Result + ifthen(Result <> '', ';') + lString;
  end;
  Result := QuotedStr(Result);
  lSchemas := '';
  if pUnique.Schemas.Count > 0 then
  begin
    for lString in pUnique.Schemas do
    begin
      lSchemas := lSchemas + ifthen(lSchemas <> '', ';') + ToSchemaCharCase(lString);
    end;
    Result := Result + ', ' + QuotedStr(lSchemas);
  end;
end;

function TFreedomObjectUnitCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TFreedomObjectUnitCreator.GetValuesFromBooleanColumn(pColumn: TBooleanColumnMapper): string;
var
  lValueTrue: string;
  lValueFalse: string;
begin
  Result := '';
  if not(TBooleanColumnMapper(pColumn).InternalColumnType in [ctyBoolean, ctyUnknow]) then
  begin
    if VarType(TBooleanColumnMapper(pColumn).ValueTrue) = vtInteger then
    begin
      lValueTrue := TBooleanColumnMapper(pColumn).ValueTrue;
      lValueFalse := TBooleanColumnMapper(pColumn).ValueFalse;
    end
    else
    begin
      lValueTrue := QuotedStr(TBooleanColumnMapper(pColumn).ValueTrue);
      lValueFalse := QuotedStr(TBooleanColumnMapper(pColumn).ValueFalse);
    end;
    Result := lValueTrue + ', ' + lValueFalse;
  end;
  if (pColumn.ColumnOptions <> []) or (pColumn.Alias <> '') then
  begin
    Result := Result + ifthen(Result <> '', ', ') + ColumnOptionsToString(pColumn.ColumnOptions);
  end;
end;

function TFreedomObjectUnitCreator.GetValuesFromColumn(pColumn: TCustomColumnMapper): string;
begin
  Result := '';
  if (pColumn.ColumnOptions <> []) or (pColumn.Size > 0) or (pColumn.Scale > 0) or (pColumn.Alias <> '') then
  begin
    Result := Result + ifthen(Result <> '', ', ') + ColumnOptionsToString(pColumn.ColumnOptions);
  end;
  if (pColumn.Size > 0) or (pColumn.Scale > 0) or (pColumn.Alias <> '') then
  begin
    Result := Result + ifthen(Result <> '', ', ') + IntToStr(pColumn.Size);
  end;
  if (pColumn.Scale > 0) or (pColumn.Alias <> '') then
  begin
    Result := Result + ifthen(Result <> '', ', ') + IntToStr(pColumn.Scale);
  end;
end;

function TFreedomObjectUnitCreator.GetValuesFromDetailColumn(pColumn: TDetailColumnMapper): string;
begin
  Result := '';
  if (pColumn.RefColumnName <> '') or (pColumn.ColumnOptions <> []) or (pColumn.Alias <> '') then
  begin
    Result := QuotedStr(ToMappingCharCase(pColumn.RefColumnName));
  end;
  if (pColumn.ColumnOptions <> []) or (pColumn.Alias <> '') then
  begin
    Result := Result + ifthen(Result <> '', ', ') + ColumnOptionsToString(pColumn.ColumnOptions);
  end;
end;

function TFreedomObjectUnitCreator.GetValuesFromEnumerationColumn(pColumn: TEnumerationColumnMapper): string;
var
  lEnumValue: string;
  lIndex: Integer;
begin
  Result := '';
  if pColumn.EnumType <> emByte then
  begin
    for lIndex := low(pColumn.EnumCharOf) to high(pColumn.EnumCharOf) do
    begin
      lEnumValue := lEnumValue + ifthen(lEnumValue <> '', ';') + pColumn.EnumCharOf[lIndex];
    end;
    Result := QuotedStr(lEnumValue);
  end;
  if (pColumn.ColumnOptions <> []) or (pColumn.Alias <> '') then
  begin
    Result := Result + ifthen(Result <> '', ', ') + ColumnOptionsToString(pColumn.ColumnOptions);
  end;
end;

function TFreedomObjectUnitCreator.GetValuesFromJoinedColumn(pColumn: TJoinedColumnMapper): string;
begin
  Result := '';
  if (pColumn.RefColumnName <> '') or (pColumn.ColumnOptions <> []) or (pColumn.Alias <> '') then
  begin
    Result := QuotedStr(ToMappingCharCase(pColumn.RefColumnName));
  end;
  if (pColumn.ColumnOptions <> []) or (pColumn.Alias <> '') then
  begin
    Result := Result + ifthen(Result <> '', ', ') + ColumnOptionsToString(pColumn.ColumnOptions);
  end;
end;

function TFreedomObjectUnitCreator.HasBlob: Boolean;
var
  lColumn: TCustomColumnMapper;
  lObjectDescriptor: TFreedomClassDescriptor;
begin
  Result := False;
  for lObjectDescriptor in FUnitDescriptor do
  begin
    for lColumn in lObjectDescriptor.ObjectMapper.Columns do
    begin
      Result := (lColumn.ColumnType in [ctyMemo, ctyBlob]);
      if (Result) then
      begin
        Break;
      end;
    end;
    if (Result) then
    begin
      Break;
    end;
  end;
end;

function TFreedomObjectUnitCreator.HasConstructor: Boolean;
var
  lProperty: TFieldPropertyDescriptor;
begin
  Result := False;
  for lProperty in FObjectDescriptor.FieldPropertyList do
  begin
    Result := (lProperty.FieldMapper.ColumnType in [ctyXML, ctyExtension, ctyDetail, ctyBlob, ctyMemo, ctyJoin]) or lProperty.FieldMapper.IsNullable;
    if Result then
    begin
      Break;
    end;
  end;
end;

function TFreedomObjectUnitCreator.HasLazy: Boolean;
var
  lColumn: TCustomColumnMapper;
  lObjectDescriptor: TFreedomClassDescriptor;
begin
  Result := False;
  for lObjectDescriptor in FUnitDescriptor do
  begin
    for lColumn in lObjectDescriptor.ObjectMapper.Columns do
    begin
      Result := (lColumn.LazyOptions.IsLazy) and (lColumn.LazyOptions.LazyType = Simple);
      if (Result) then
      begin
        Break;
      end;
    end;
    if (Result) then
    begin
      Break;
    end;
  end;
end;

function TFreedomObjectUnitCreator.HasLazyList: Boolean;
var
  lColumn: TCustomColumnMapper;
  lObjectDescriptor: TFreedomClassDescriptor;
begin
  Result := False;
  for lObjectDescriptor in FUnitDescriptor do
  begin
    for lColumn in lObjectDescriptor.ObjectMapper.Columns do
    begin
      Result := (lColumn.LazyOptions.IsLazy) and (lColumn.LazyOptions.LazyType = List);
      if (Result) then
      begin
        Break;
      end;
    end;
    if (Result) then
    begin
      Break;
    end;
  end;
end;

function TFreedomObjectUnitCreator.HasNullable: Boolean;
var
  lColumn: TCustomColumnMapper;
  lObjectDescriptor: TFreedomClassDescriptor;
begin
  Result := False;
  for lObjectDescriptor in FUnitDescriptor do
  begin
    for lColumn in lObjectDescriptor.ObjectMapper.Columns do
    begin
      Result := lColumn.IsNullable;
      if (Result) then
      begin
        Break;
      end;
    end;
    if (Result) then
    begin
      Break;
    end;
  end;
end;

function TFreedomObjectUnitCreator.HasTypedNullable: Boolean;
var
  lObjectDescriptor: TFreedomClassDescriptor;
  lDescriptor: TFieldPropertyDescriptor;
begin
  Result := False;
  for lObjectDescriptor in FUnitDescriptor do
  begin
    for lDescriptor in lObjectDescriptor.FieldPropertyList do
    begin
      Result := lDescriptor.FieldMapper.IsNullable and lDescriptor.UseNullableTypes;
      if (Result) then
      begin
        Break;
      end;
    end;
    if (Result) then
    begin
      Break;
    end;
  end;
end;

function TFreedomObjectUnitCreator.HasXML: Boolean;
var
  lColumn: TCustomColumnMapper;
  lObjectDescriptor: TFreedomClassDescriptor;
begin
  Result := False;
  for lObjectDescriptor in FUnitDescriptor do
  begin
    for lColumn in lObjectDescriptor.ObjectMapper.Columns do
    begin
      Result := (lColumn.ColumnType in [ctyXML]);
      if (Result) then
      begin
        Break;
      end;
    end;
    if (Result) then
    begin
      Break;
    end;
  end;
end;

function TFreedomObjectUnitCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TFreedomObjectUnitCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TFreedomObjectUnit.Create;
  TFreedomObjectUnit(Result).FreedomSource := GenerateSource;
end;

function TFreedomObjectUnitCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

procedure TFreedomObjectUnitCreator.SetUnitDescriptor(const Value: TFreedomUnitDescriptor);
var
  lClass: TFreedomClassDescriptor;
begin
  FUnitDescriptor := Value;
  for lClass in FUnitDescriptor do
  begin
    FIniFile.WriteString('ClassToUnitName', lClass.NewClassName, FUnitDescriptor.NewUnitName);
    FIniFile.WriteString('ClassToUnitName', lClass.NewBaseClass, lClass.NewBaseClassUnit);
    if (lClass.ListClassName <> '') then
    begin
      FIniFile.WriteString('ClassToUnitName', lClass.ListClassName, FUnitDescriptor.NewUnitName);
      FIniFile.WriteString('ClassToUnitName', lClass.BaseClassListName, lClass.BaseClassListUnit);
    end;
  end;
end;

function TFreedomObjectUnitCreator.ToMappingCharCase(pString: string): string;
begin
  case FIniFile.ReadInteger('Options', 'MappingCharCase', 1) of
    1:
      Result := UpperCase(pString);
    2:
      Result := LowerCase(pString);
  else
    Result := pString;
  end;
end;

function TFreedomObjectUnitCreator.ToSchemaCharCase(pString: string): string;
begin
  case FIniFile.ReadInteger('Options', 'SchemaCharCase', 2) of
    1:
      Result := UpperCase(pString);
    2:
      Result := LowerCase(pString);
  else
    Result := pString;
  end;
end;

procedure TFreedomObjectUnitCreator.WriteSectionUnitName;
var
  lClass: TFreedomClassDescriptor;
  lPropField: TFieldPropertyDescriptor;
  lTypeName: string;
  procedure AddClassUnit(pClassName, pClassUnitName: String);
  begin
    if (pClassUnitName <> '') and (pClassName <> '') then
    begin
      FIniFile.WriteString('ClassToUnitName', pClassName, pClassUnitName);
    end;
  end;
begin
  for lClass in FUnitDescriptor do
  begin
    AddClassUnit(lClass.NewBaseClass, lClass.NewBaseClassUnit);
    AddClassUnit(lClass.BaseClassListName, lClass.BaseClassListUnit);
    AddClassUnit(lClass.ListClassName, FUnitDescriptor.NewUnitName);
    AddClassUnit(lClass.NewClassName, FUnitDescriptor.NewUnitName);
    for lPropField in lClass.FieldPropertyList do
    begin
      if (lPropField.TypeNameUnit <> '') then
      begin
        lTypeName := lPropField.TypeName;
        if (Pos('<', lTypeName) > 0) or (Pos('>', lTypeName) > 0) then
        begin
          Delete(lTypeName, 1, Pos('<', lTypeName));
          lTypeName := Copy(lTypeName, 1, Pos('>', lTypeName) - 1)
        end;
        AddClassUnit(lTypeName, lPropField.TypeNameUnit);
      end
    end;
  end;
  FIniFile.UpdateFile;
end;

{ TFreedomObjectUnit }

function TFreedomObjectUnit.GetAge: TDateTime;
begin
  Result := -1;
end;

function TFreedomObjectUnit.GetSource: string;
begin
  Result := FFreedomSource;
end;

end.
