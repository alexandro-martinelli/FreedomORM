unit AM.Freedom.ObjectMapper.ObjectToMapper;

interface

uses
  System.Rtti,
  System.Generics.Collections,
  AM.Freedom.ObjectMapper.CustomMapper,
  AM.Freedom.ObjectMapper,
  AM.Freedom.Attributes,
  AM.Freedom.ObjectMapper.CustomColumnReader,
  AM.Freedom.ColumnReaderRegister,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.Exceptions,
  AM.Freedom.Helper.RttiType,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.FreedomAttributes,
  AM.Freedom.ObjectMapper.ColumnReaders,
  AM.Freedom.ObjectMapper.ConstraintMapper,
  AM.Freedom.ObjectMapper.Schemas,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.ObjectMapper.ObjectMappingExplorer,
  System.SyncObjs;

type
  TObjectToMapperParams = class sealed
  private
    FOptions: TObjectToMapperOptions;
    FInstance: TObject;
    FIsExtension: Boolean;
    FMetaClassType: TClass;
    procedure SetInstance(const Value: TObject);
  public
    constructor Create;
    property MetaClassType: TClass read FMetaClassType write FMetaClassType;
    property ObjectInstance: TObject read FInstance write SetInstance;
    property Options: TObjectToMapperOptions read FOptions write FOptions default [SubLevels];
    property IsExtension: Boolean read FIsExtension write FIsExtension;
  end;

  TFieldAttributeParams = class sealed
  strict private
    FField: TRttiField;
    FAttribute: FreedomAttribute;
    function IsValidClass(pColumnClass: ColumnClass; pInClass: Array of ColumnClass): Boolean;
    procedure ValidateTypeToAttribute(pField: TRttiField; pColumnClass: ColumnClass);
  public
    constructor Create(pField: TRttiField; pAttribute: FreedomAttribute);
    property Field: TRttiField read FField write FField;
    property Attribute: FreedomAttribute read FAttribute write FAttribute;
  end;

  TObjectToMapper = class
  private
    class var FRttiContext: TRttiContext;
  strict private
    FRttiType: TRttiType;
    FObjectMapper: TObjectMapper;
    FParams: TObjectToMapperParams;
    function GetColumnMapperFromColumnReader(pParams: TFieldAttributeParams): TCustomColumnMapper;
    function FieldAttributeCanBeRead(pField: TRttiField; pAttribute: TCustomAttribute): Boolean;
    function GetReaderFromFieldAttribute(pParams: TFieldAttributeParams): TCustomColumnMapper;
    function GetObjectNameFromClassName(pObjectName: String): String;
    function GetObjectAliasFromClassName(pObjectName: String): String;
    procedure ReadAttributesOfClass;
    procedure AddAllSchemasToConstraints;
    procedure AddAllSchemasToUniques;
    procedure AddAllSchemasToPrimarys;
    procedure AddAllSchemasToForeigns;
    procedure ReadColumns;
    procedure ReadColumnsFromFields;
    procedure ReadColumnFromProperties;
    procedure AddColumn(pAttribute: FreedomAttribute; pField: TRttiField);
    procedure AddAllSchemasToColumn(pColumn: TCustomColumnMapper);
    procedure InternalAddAllSchemasToColumn(pColumn: TCustomColumnMapper);
    procedure VerifyAliasFromReferenceColumn(pColumn: TCustomColumnMapper);
    procedure FixAliasOnChildExtensionColumns(pColumn: TCustomColumnMapper);
    procedure FixColumnOptionsIfIsId(pColumn: TCustomColumnMapper);
    procedure AddAutoMappingColumn(pField: TRttiField);
    procedure ReadMethods;
    procedure ReadForeignFromJoinedColumns;
    function CreateListFromString(pString: String): TList<String>;
    procedure FixAliasFromReferenceColumns;
  strict protected
    function MakeMapper: TObjectMapper;
    function DoExtractSchemas(pClassInfo: Pointer): TSchemas;
  public
    destructor Destroy; override;
    class function ObjectToMapper(pObjectToMapperParams: TObjectToMapperParams): TObjectMapper;
    class procedure AssignObjectToMapper(pObjectMapper: TObjectMapper; pObjectInstance: TObject);
    class procedure UnlockMapper(pMapperHash: TMapperHash);
    class procedure RemoveIncompleteMapper(pMapperHash: TMapperHash);
    class function ExtractSchemas(pClassInfo: Pointer): TSchemas;
    class function ExtractDefaultSchema(pClassInfo: Pointer): String;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.ObjectMapper.TriggerMapper,
  AM.Freedom.ObjectMapper.MethodMapper,
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.INullable,
  AM.Freedom.Helper.RttiField,
  AM.Freedom.Helper.RttiProperty,
  AM.Freedom.ILazy,
  AM.Freedom.LazyList,
  AM.Freedom.ObjectMapper.ObjectToMapperAssigner;

{ TObjectReader }

procedure TObjectToMapper.ReadColumnFromProperties;
var
  lProperty: TRttiProperty;
  lColumn: TCustomColumnMapper;
  lColumnReader: TCustomColumnReader;
begin
  for lProperty in FRttiType.GetProperties do
  begin
    if lProperty.IsReadable then
    begin
      lColumnReader := TCustomColumnReader.Create;
      try
        lColumn := lColumnReader.PropertyToMapper(lProperty, FParams.ObjectInstance, False);
        if Assigned(lColumn) then
        begin
          lColumn.RttiOptions.RttiObject := FParams.ObjectInstance;
          lColumn.RttiOptions.RttiType := FRttiType;
          lColumn.RttiOptions.RttiProperty := lProperty;
          FObjectMapper.Columns.Add(lColumn);
        end;
      finally
        lColumnReader.Free;
      end;
    end;
  end;
end;

procedure TObjectToMapper.ReadColumns;
begin
  if Properties in FParams.Options then
  begin
    ReadColumnFromProperties;
  end
  else
  begin
    ReadColumnsFromFields;
  end;
end;

procedure TObjectToMapper.ReadColumnsFromFields;
var
  lField: TRttiField;
  lMappedColumn: Boolean;
  lAttribute: TCustomAttribute;
begin
  for lField in FRttiType.GetFields do
  begin
    lMappedColumn := False;
    for lAttribute in lField.GetAttributes do
    begin
      if FieldAttributeCanBeRead(lField, lAttribute) then
      begin
        AddColumn(FreedomAttribute(lAttribute), lField);
        lMappedColumn := True;
        Break;
      end
      else if lAttribute.ClassType = NoMapping then
      begin
        lMappedColumn := True;
        Break;
      end;
    end;
    if not lMappedColumn then
    begin
      AddAutoMappingColumn(lField);
    end;
  end;
end;

procedure TObjectToMapper.ReadForeignFromJoinedColumns;
var
  lColumn: TCustomColumnMapper;
  lReferencesTo: string;
  lConstraintName: string;
  lColumnNames, lReferenceColumns, lSchemas: TList<string>;
begin
  for lColumn in FObjectMapper.Columns.JoinColumns do
  begin
    if lColumn.ColumnType.IsObject then
    begin
      lReferencesTo := TJoinedColumnMapper(lColumn).RefObjectName;
      lConstraintName := 'FK_%s_' + lReferencesTo;
      if FObjectMapper.Foreigns.FindForeign(lConstraintName) = nil then
      begin
        lColumnNames := CreateListFromString(TJoinedColumnMapper(lColumn).Name);
        lReferenceColumns := CreateListFromString(TJoinedColumnMapper(lColumn).RefColumnName);
        lSchemas := lColumn.Schemas.ToListOfString;
        try
          FObjectMapper.Foreigns.Add(TForeignMapper.Create(lColumnNames, lSchemas, lReferenceColumns, lReferencesTo,
              TJoinedColumnMapper(lColumn).UpdateAction, TJoinedColumnMapper(lColumn).DeleteAction));
        finally
          lColumnNames.Free;
          lReferenceColumns.Free;
          lSchemas.Free;
        end;
      end;
    end;
  end;
end;

procedure TObjectToMapper.ReadMethods;
var
  lMethod: TRttiMethod;
  lAttribute: TCustomAttribute;
begin
  for lMethod in FRttiType.GetMethods do
  begin
    for lAttribute in lMethod.GetAttributes do
    begin
      if lAttribute is VirtualMethodControl then
      begin
        FObjectMapper.Methods.Add(TMethodMapper.Create(lMethod.Name, VirtualMethodControl(lAttribute).MetaClass, VirtualMethodControl(lAttribute).Options))
      end;
    end;
  end;
end;

class procedure TObjectToMapper.RemoveIncompleteMapper(pMapperHash: TMapperHash);
begin
  TObjectMappingExplorer.RemoveIncompleteMapper(pMapperHash);
end;

procedure TObjectToMapper.VerifyAliasFromReferenceColumn(pColumn: TCustomColumnMapper);
var
  lColumn: TReferenceColumnMapper;
begin
  if pColumn.InheritsFrom(TReferenceColumnMapper) then
  begin
    lColumn := TReferenceColumnMapper(pColumn);
    if (lColumn.RefObjectAlias <> '') and (lColumn.RefObjectAlias = FObjectMapper.Alias) then
    begin
      lColumn.RefObjectAlias := lColumn.RefObjectAlias + '_1';
      lColumn.DuplicatedAlias := True;
    end;
  end;
end;

procedure TObjectToMapper.AddAllSchemasToColumn(pColumn: TCustomColumnMapper);
var
  lExtensionColumn: TExtensionColumnMapper;
  lColumn: TCustomColumnMapper;
begin
  if pColumn.ColumnType <> ctyExtension then
  begin
    InternalAddAllSchemasToColumn(pColumn);
  end
  else
  begin
    lExtensionColumn := TExtensionColumnMapper(pColumn);
    for lColumn in lExtensionColumn.ChildColumns do
    begin
      if lColumn.ColumnType <> ctyExtension then
      begin
        InternalAddAllSchemasToColumn(lColumn);
      end
      else
      begin
        AddAllSchemasToColumn(lColumn);
      end;
    end;
  end;
end;

procedure TObjectToMapper.AddAllSchemasToConstraints;
begin
  AddAllSchemasToUniques;
  AddAllSchemasToPrimarys;
  AddAllSchemasToForeigns;
end;

procedure TObjectToMapper.AddAllSchemasToForeigns;
var
  lSchema: TSchemaItem;
  lForeign: TForeignMapper;
begin
  if FObjectMapper.Schemas.Count > 0 then
  begin
    for lForeign in FObjectMapper.Foreigns do
    begin
      if lForeign.Schemas.Count = 0 then
      begin
        for lSchema in FObjectMapper.Schemas do
        begin
          lForeign.Schemas.Add(lSchema.Name);
        end;
      end;
    end;
  end;
end;

procedure TObjectToMapper.AddAllSchemasToPrimarys;
var
  lSchema: TSchemaItem;
  lPrimary: TPrimaryMapper;
begin
  if FObjectMapper.Schemas.Count > 0 then
  begin
    for lPrimary in FObjectMapper.Primarys do
    begin
      if lPrimary.Schemas.Count = 0 then
      begin
        for lSchema in FObjectMapper.Schemas do
        begin
          lPrimary.Schemas.Add(lSchema.Name);
        end;
      end;
    end;
  end;
end;

procedure TObjectToMapper.AddAllSchemasToUniques;
var
  lSchema: TSchemaItem;
  lUnique: TUniqueMapper;
begin
  if FObjectMapper.Schemas.Count > 0 then
  begin
    for lUnique in FObjectMapper.Uniques do
    begin
      if lUnique.Schemas.Count = 0 then
      begin
        for lSchema in FObjectMapper.Schemas do
        begin
          lUnique.Schemas.Add(lSchema.Name);
        end;
      end;
    end;
  end;
end;

procedure TObjectToMapper.AddAutoMappingColumn(pField: TRttiField);
var
  lAutoMapping: AutoMapping;
begin
  if FObjectMapper.IsAutoMapping and pField.FieldType.ToColumnType.CanBeAutoMapped then
  begin
    lAutoMapping := AutoMapping.Create;
    try
      AddColumn(lAutoMapping, pField);
    finally
      FreeAndNil(lAutoMapping);
    end;
  end;
end;

procedure TObjectToMapper.AddColumn(pAttribute: FreedomAttribute; pField: TRttiField);
var
  lColumn: TCustomColumnMapper;
begin
  lColumn := GetReaderFromFieldAttribute(TFieldAttributeParams.Create(pField, pAttribute));
  lColumn.RttiOptions.RttiType := FRttiType;
  lColumn.RttiOptions.RttiField := pField;
  lColumn.RttiOptions.RttiObject := FParams.ObjectInstance;
  FObjectMapper.Columns.Add(lColumn);
  AddAllSchemasToColumn(lColumn);
  VerifyAliasFromReferenceColumn(lColumn);
  FixAliasOnChildExtensionColumns(lColumn);
  FixColumnOptionsIfIsId(lColumn);
end;

class procedure TObjectToMapper.AssignObjectToMapper(pObjectMapper: TObjectMapper; pObjectInstance: TObject);
var
  lObjectToMapperAssigner: TObjectToMapperAssigner;
begin
  lObjectToMapperAssigner := TObjectToMapperAssigner.Create;
  try
    lObjectToMapperAssigner.AssignObjectToMapper(pObjectMapper, pObjectInstance);
  finally
    lObjectToMapperAssigner.Free;
  end;
end;

function TObjectToMapper.CreateListFromString(pString: String): TList<String>;
var
  lStrings: TStrings;
  lString: String;
begin
  Result := TList<String>.Create;
  lStrings := TStringList.Create;
  try
    ExtractStrings([',', ';', '.'], [' '], PWideChar(pString), lStrings);
    for lString in lStrings do
    begin
      Result.Add(lString);
    end;
  finally
    lStrings.Free;
  end;
end;

destructor TObjectToMapper.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

function TObjectToMapper.DoExtractSchemas(pClassInfo: Pointer): TSchemas;
var
  lAttribute: TCustomAttribute;
  lRttiType: TRttiType;
begin
  Result := TSchemas.Create;
  FRttiType := FRttiContext.GetType(pClassInfo);
  lRttiType := FRttiType;
  while lRttiType <> nil do
  begin
    for lAttribute in lRttiType.GetAttributes do
    begin
      if lAttribute.InheritsFrom(Schema) then
      begin
        Result.AddSchema(Schema(lAttribute).Name, Schema(lAttribute).Default);
      end;
    end;
    lRttiType := lRttiType.BaseType;
  end;
  Result.AdjustSchemas;
end;

class function TObjectToMapper.ExtractDefaultSchema(pClassInfo: Pointer): String;
var
  lSchemas: TSchemas;
  lDefault: TSchemaItem;
begin
  lSchemas := TObjectToMapper.ExtractSchemas(pClassInfo);
  Result := '';
  try
    lDefault := lSchemas.DefaultSchema;
    if Assigned(lDefault) then
    begin
      Result := lDefault.Name;
    end;
  finally
    lSchemas.Free;
  end;
end;

class function TObjectToMapper.ExtractSchemas(pClassInfo: Pointer): TSchemas;
var
  lObjectReader: TObjectToMapper;
begin
  lObjectReader := TObjectToMapper.Create;
  try
    Result := lObjectReader.DoExtractSchemas(pClassInfo);
  finally
    FreeAndNil(lObjectReader);
  end;
end;

function TObjectToMapper.FieldAttributeCanBeRead(pField: TRttiField; pAttribute: TCustomAttribute): Boolean;
begin
  Result := pAttribute.InheritsFrom(CustomColumn) or
      ((pAttribute.ClassType = AutoMapping) and pField.FieldType.ToColumnType.CanBeAutoMapped) or
      (pAttribute.ClassType = Extension);
end;

class procedure TObjectToMapper.UnlockMapper(pMapperHash: TMapperHash);
begin
  TObjectMappingExplorer.UnlockMapper(pMapperHash);
end;

procedure TObjectToMapper.FixAliasOnChildExtensionColumns(pColumn: TCustomColumnMapper);
var
  lChild: TCustomColumnMapper;
begin
  if (pColumn.ColumnType = ctyExtension) then
  begin
    for lChild in TExtensionColumnMapper(pColumn).ChildColumns do
    begin
      if (lChild.IsExtension) then
      begin
        FixAliasOnChildExtensionColumns(lChild);
      end
      else
      begin
        VerifyAliasFromReferenceColumn(lChild);
      end;
    end;
  end;
end;

procedure TObjectToMapper.FixColumnOptionsIfIsId(pColumn: TCustomColumnMapper);
begin
  if (pColumn.IdOptions.IsId) then
  begin
    if (not (Required in pColumn.ColumnOptions)) then
    begin
      pColumn.ColumnOptions := pColumn.ColumnOptions + [Required];
    end;
  end;
end;

function TObjectToMapper.GetColumnMapperFromColumnReader(pParams: TFieldAttributeParams): TCustomColumnMapper;
var
  lReaderClass: TColumnReaderClass;
  lReader: TCustomColumnReader;
  lFieldToMapperParams: TFieldToMapperParams;
begin
  lReaderClass := TColumnReaderRegister.FindColumnReader(TAttributeClass(pParams.Attribute.ClassType));
  if Assigned(lReaderClass) then
  begin
    lReader := lReaderClass.Create;
    try
      lFieldToMapperParams := TFieldToMapperParams.Create;
      lFieldToMapperParams.RttiField := pParams.Field;
      lFieldToMapperParams.ObjectInstance := FParams.ObjectInstance;
      lFieldToMapperParams.ObjectMapper := FObjectMapper;
      lFieldToMapperParams.SubLevels := (SubLevels in FParams.Options);
      lFieldToMapperParams.IsChild := FParams.IsExtension;
      Result := lReader.FieldToMapper(lFieldToMapperParams);
    finally
      FreeAndNil(lReader);
    end;
  end
  else
  begin
    raise EInvalidReaderClass.Create(pParams.Attribute.ClassName);
  end;
end;

function TObjectToMapper.GetObjectAliasFromClassName(pObjectName: String): String;
begin
  Result := GetObjectNameFromClassName(pObjectName);
  Result := Copy(Result, 1, 4);
end;

function TObjectToMapper.GetObjectNameFromClassName(pObjectName: String): String;
begin
  Result := Copy(pObjectName, 2, Length(pObjectName));
end;

function TObjectToMapper.GetReaderFromFieldAttribute(pParams: TFieldAttributeParams): TCustomColumnMapper;
begin
  try
    Result := GetColumnMapperFromColumnReader(pParams);
  finally
    FreeAndNil(pParams);
  end;
end;

procedure TObjectToMapper.InternalAddAllSchemasToColumn(pColumn: TCustomColumnMapper);
var
  lSchema: TSchemaItem;
begin
  if pColumn.Schemas.Count = 0 then
  begin
    for lSchema in FObjectMapper.Schemas do
    begin
      pColumn.Schemas.AddSchema(lSchema.Name, False);
    end;
  end;
  if (pColumn.IdOptions.IsId) and (pColumn.IdOptions.Schemas.Count = 0) then
  begin
    for lSchema in pColumn.Schemas do
    begin
      if (pColumn.Schemas.FindSchema(lSchema.Name) <> nil) then
      begin
        pColumn.IdOptions.Schemas.AddSchema(lSchema.Name, False);
      end;
    end;
  end;
end;

function TObjectToMapper.MakeMapper: TObjectMapper;
begin
  FObjectMapper := TObjectMapper.Create;
  FRttiType := FRttiContext.GetType(FParams.MetaClassType.ClassInfo);
  try
    FObjectMapper.RttiOptions.RttiObject := FParams.ObjectInstance;
    FObjectMapper.RttiOptions.RttiType := FRttiType;
    if (not FParams.IsExtension) then
    begin
      ReadAttributesOfClass;
      AddAllSchemasToConstraints;
      if FObjectMapper.Unmapped then
      begin
        raise EUnmappedClass.Create(FRttiType.Name);
      end;
    end;
    FObjectMapper.MetaClassType := FRttiType.AsInstance.MetaclassType;
    FObjectMapper.IsList := Supports(FRttiType.AsInstance.MetaclassType, IFreedomObjectList);
    ReadMethods;
    ReadColumns;
    ReadForeignFromJoinedColumns;
    FixAliasFromReferenceColumns;
  except
    On E:Exception do
    begin
      FreeAndNil(FObjectMapper);
      raise;
    end;
  end;
  Result := FObjectMapper;
end;

class function TObjectToMapper.ObjectToMapper(pObjectToMapperParams: TObjectToMapperParams): TObjectMapper;
var
  lObjectReader: TObjectToMapper;
  lMapperMaked: Boolean;
  lOptions: TObjectToMapperOptions;
begin
  lMapperMaked := False;
  lOptions := pObjectToMapperParams.Options;
  Result := TObjectMappingExplorer.FindMappingByMetaClassType(pObjectToMapperParams.MetaClassType, 0, lOptions);
  if (not Assigned(Result)) then
  begin
    lObjectReader := TObjectToMapper.Create;
    try
      lObjectReader.FParams := pObjectToMapperParams;
      Result := lObjectReader.MakeMapper;
      lMapperMaked := True;
      TObjectMappingExplorer.AddMapping(Result.MetaClassType, Result, lOptions);
    finally
      FreeAndNil(lObjectReader);
    end;
  end
  else
  begin
    pObjectToMapperParams.Free;
    TObjectMappingExplorer.LockMapper(Result);
  end;
  if (pObjectToMapperParams.ObjectInstance <> nil) and not lMapperMaked then
  begin
    AssignObjectToMapper(Result, pObjectToMapperParams.ObjectInstance);
  end;
end;

procedure TObjectToMapper.ReadAttributesOfClass;
var
  lAttribute: TCustomAttribute;
  lRttiType: TRttiType;
begin
  FObjectMapper.IsAutoMapping := False;
  lRttiType := FRttiType;
  while lRttiType <> nil do
  begin
    for lAttribute in lRttiType.GetAttributes do
    begin
      if lAttribute.ClassType = AutoMapping then
      begin
        FObjectMapper.IsAutoMapping := True;
        FObjectMapper.Name := GetObjectNameFromClassName(FRttiType.Name);
        FObjectMapper.Alias := GetObjectAliasFromClassName(FRttiType.Name);
        FObjectMapper.Unmapped := False;
      end
      else if (lAttribute.ClassType = Entity) then
      begin
        FObjectMapper.Name := Entity(lAttribute).Name;
        FObjectMapper.Alias := Entity(lAttribute).Alias;
        FObjectMapper.Unmapped := False;
      end
      else if (lAttribute.ClassType = Cursor) and FObjectMapper.Unmapped then
      begin
        FObjectMapper.ObjectCursorClass := Cursor(lAttribute).MetaClass;
        FObjectMapper.Unmapped := False;
      end
      else if lAttribute.ClassType = Trigger then
      begin
        FObjectMapper.Triggers.Add(TTriggerMapper.Create(Trigger(lAttribute).MetaClass, Trigger(lAttribute).Options));
      end
      else if lAttribute.ClassType = Unique then
      begin
        FObjectMapper.Uniques.Add(TUniqueMapper.Create(Unique(lAttribute).Columns, Unique(lAttribute).Schemas));
      end
      else if lAttribute.ClassType = Primary then
      begin
        FObjectMapper.Primarys.Add(TPrimaryMapper.Create(Primary(lAttribute).Columns, Primary(lAttribute).Schemas));
      end
      else if lAttribute.ClassType = Foreign then
      begin
        FObjectMapper.Foreigns.Add(TForeignMapper.Create(Foreign(lAttribute).Columns, Foreign(lAttribute).Schemas,
          Foreign(lAttribute).ReferenceColumns, Foreign(lAttribute).ReferencesTo, Foreign(lAttribute).OnUpdate,
          Foreign(lAttribute).OnDelete));
      end
      else if lAttribute.ClassType.InheritsFrom(Schema) then
      begin
        FObjectMapper.Schemas.AddSchema(Schema(lAttribute).Name, Schema(lAttribute).Default);
      end;
    end;
    lRttiType := lRttiType.BaseType;
  end;
  FObjectMapper.Schemas.AdjustSchemas;
end;

{ TObjectFieldAttributeParam }

constructor TFieldAttributeParams.Create(pField: TRttiField; pAttribute: FreedomAttribute);
begin
  if pAttribute.InheritsFrom(CustomColumn) then
  begin
    ValidateTypeToAttribute(pField, ColumnClass(pAttribute.ClassType));
  end
  else if (pAttribute.ClassType = Extension) and (not pField.FieldType.IsInstance) then
  begin
    raise EInvalidFieldExtensionAttribute.Create(pField.ToString, pField.Parent.Name);
  end;
  FField := pField;
  FAttribute := pAttribute;
end;

function TFieldAttributeParams.IsValidClass(pColumnClass: ColumnClass; pInClass: array of ColumnClass): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := Low(pInClass) to High(pInClass) do
  begin
    Result := pColumnClass = pInClass[I];
    if Result then
    begin
      Break;
    end;
  end;
end;

procedure TFieldAttributeParams.ValidateTypeToAttribute(pField: TRttiField; pColumnClass: ColumnClass);
var
  lColumnType: TColumnType;
  lIsNullable: Boolean;
begin
  lColumnType := pField.FieldType.ToColumnType;
  lIsNullable := False;
  if pField.FieldType.IsInstance then
  begin
    lIsNullable := Supports(pField.FieldType.AsInstance.MetaclassType, INullable)
  end;
  if (lIsNullable) then
  begin
    if not (lColumnType.IsSimpleType or lColumnType.IsBoolean) or not IsValidClass(pColumnClass, [Column, BooleanColumn]) then
    begin
      raise EInvalidFieldAttribute.Create(pColumnClass.ClassName, pField.ToString, Column.ClassName);
    end
  end
  else
  begin
    if lColumnType.IsSimpleType and not IsValidClass(pColumnClass, [Column, JoinedColumn]) then
    begin
      raise EInvalidFieldAttribute.Create(pColumnClass.ClassName, pField.ToString, Column.ClassName);
    end
    else if (lColumnType.IsBlob) and (pColumnClass <> BlobColumn) then
    begin
      raise EInvalidFieldAttribute.Create(pColumnClass.ClassName, pField.ToString, BlobColumn.ClassName);
    end
    else if (pField.FieldType.IsInstance) and not IsValidClass(pColumnClass, [BlobColumn, JoinedColumn, DetailColumn]) then
    begin
      raise EInvalidFieldAttribute.Create(pColumnClass.ClassName, pField.ToString, BlobColumn.ClassName + ', ' +
          JoinedColumn.ClassName + ', ' + DetailColumn.ClassName);
    end
    else if (lColumnType = ctyBoolean) and (pColumnClass <> BooleanColumn) then
    begin
      raise EInvalidFieldAttribute.Create(pColumnClass.ClassName, pField.ToString, BooleanColumn.ClassName);
    end
    else if (lColumnType = ctyEnumerator) and (not IsValidClass(pColumnClass, [EnumerationColumn, JoinedColumn])) then
    begin
      raise EInvalidFieldAttribute.Create(pColumnClass.ClassName, pField.ToString, EnumerationColumn.ClassName + ', ' +
        JoinedColumn.ClassName);
    end;
  end;
end;
{ TObjectToMapperParams }

constructor TObjectToMapperParams.Create;
begin
  FOptions := [SubLevels];
end;

procedure TObjectToMapperParams.SetInstance(const Value: TObject);
begin
  FInstance := Value;
  if Assigned(FInstance) then
  begin
    FMetaClassType := FInstance.ClassType;
  end;
end;

procedure TObjectToMapper.FixAliasFromReferenceColumns;
var
  lIndex, lInnerIndex: Integer;
  lColumn, lInnerColumn: TReferenceColumnMapper;
begin
  for lIndex := 0 to FObjectMapper.Columns.JoinColumns.Count - 2 do
  begin
    lColumn := TReferenceColumnMapper(FObjectMapper.Columns.JoinColumns.Items[lIndex]);
    for lInnerIndex := lIndex + 1 to FObjectMapper.Columns.JoinColumns.Count - 1 do
    begin
      lInnerColumn := TReferenceColumnMapper(FObjectMapper.Columns.JoinColumns.Items[lInnerIndex]);
      if SameText(lColumn.RefObjectAlias, lInnerColumn.RefObjectAlias) and (lColumn.RefObjectAlias <> '') and
         (lInnerColumn.RefObjectAlias <> '') then
      begin
        lInnerColumn.RefObjectAlias := Format('%s_%d', [lInnerColumn.RefObjectAlias, lIndex]);
      end;
    end;
  end;
end;

initialization

finalization
  TObjectToMapper.FRttiContext.Free;

end.
