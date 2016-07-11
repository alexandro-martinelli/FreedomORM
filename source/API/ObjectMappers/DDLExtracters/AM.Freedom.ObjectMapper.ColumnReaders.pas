unit AM.Freedom.ObjectMapper.ColumnReaders;

interface

uses
  System.Rtti,
  System.Classes,
  AM.Freedom.ObjectMapper.CustomColumnReader,
  AM.Freedom.Attributes,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.DefaultsClassRegister,
  AM.Freedom.FreedomAttributes,
  AM.Freedom.ObjectMapper,
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.Exceptions;

type
  TColumnReader = class(TCustomNamedColumnReader)
  strict protected
    procedure DoGetFieldAttributeValuesFromColumnAttribute(pAttribute: CustomColumn); override;
    function GetColumnMapperClass: TColumnMapperClass; override;
  end;

  TBooleanColumnReader = class(TCustomNamedColumnReader)
  strict protected
    procedure DoGetFieldAttributeValuesFromColumnAttribute(pAttribute: CustomColumn); override;
    function GetColumnMapperClass: TColumnMapperClass; override;
    function DoGetColumnValue: Variant; override;
  end;

  TEnumerationColumnReader = class(TCustomNamedColumnReader)
  strict protected
    procedure DoGetFieldAttributeValuesFromColumnAttribute(pAttribute: CustomColumn); override;
    function GetColumnMapperClass: TColumnMapperClass; override;
    function DoGetColumnValue: Variant; override;
  end;

  TBlobColumnReader = class(TCustomNamedColumnReader)
  strict protected
    function GetColumnMapperClass: TColumnMapperClass; override;
    function DoGetColumnValue: Variant; override;
  end;

  TReferenceColumnReader = class(TCustomNamedColumnReader)
  strict private
    procedure GetReferences;
  strict protected
    procedure GetReferencesFromClass(pClass: TClass); virtual;
    function GetColumnReaderColumnType: TColumnType; virtual; abstract;
    procedure DoGetFieldAttributeValuesFromColumnAttribute(pAttribute: CustomColumn); override; final;
    procedure DoGetFieldAttributeValuesFromReferenceColumnAttribute(pAttribute: CustomColumn); virtual;
    procedure DoGetReferences; virtual;
    function DoGetColumnValue: Variant; override;
  end;

  TJoinedColumnReader = class(TReferenceColumnReader)
  strict protected
    function GetColumnReaderColumnType: TColumnType; override;
    function GetColumnMapperClass: TColumnMapperClass; override;
    procedure DoGetFieldAttributeValuesFromReferenceColumnAttribute(pAttribute: CustomColumn); override;
    procedure DoGetReferences; override;
  end;

  TDetailColumnReader = class(TReferenceColumnReader)
  strict protected
    function GetColumnReaderColumnType: TColumnType; override;
    function GetColumnMapperClass: TColumnMapperClass; override;
    procedure GetReferencesFromClass(pClass: TClass); override;
  end;

  TAutoColumnReader = class(TCustomNamedColumnReader)
  strict protected
    procedure DoGetFieldAttributeValuesFromAutoMappingAttribute(pField: TRttiField); override;
    function GetColumnMapperClass: TColumnMapperClass; override;
  end;

  TExtensionColumnReader = class(TCustomColumnReader)
  private
    procedure ReadExtensionClass;
  strict protected
    procedure GetFieldAttributeValues(pField: TRttiField); override;
    function GetColumnMapperClass: TColumnMapperClass; override;
  end;


implementation

uses
  AM.Freedom.ObjectMapper.CustomAutoMappingValueGetter,
  System.SysUtils,
  AM.Freedom.Helper.RttiField,
  AM.Freedom.ILazy,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  System.Variants,
  AM.Freedom.Helper.Variant, AM.Freedom.XML;

{ TColumnReader }

procedure TColumnReader.DoGetFieldAttributeValuesFromColumnAttribute(pAttribute: CustomColumn);
var
  lColumn: TCustomColumnMapper;
begin
  inherited;
  lColumn := TCustomColumnMapper(InternalColumn);
  if lColumn.ColumnType.Sizeable then
  begin
    lColumn.Size := Column(pAttribute).Size;
  end;
  if lColumn.ColumnType.Scalable then
  begin
    lColumn.Size := Column(pAttribute).Scale;
  end;
end;

function TColumnReader.GetColumnMapperClass: TColumnMapperClass;
begin
  Result := TCustomColumnMapper;
end;
{ TBooleanColumnReader }

function TBooleanColumnReader.DoGetColumnValue: Variant;
var
  lVarType: TVarType;
begin
  if (InternalColumn.IsNullable) then
  begin
    Result := InternalFieldHelper.GetNullableVariantValue(InternalInstance);
    if (not VarIsNull(Result)) then
    begin
      lVarType := VarType(Result);
      case lVarType of
        varString, varOleStr, varUString: Result := Result;
        else
        begin
          if (Result) then
          begin
            Result := TBooleanColumnMapper(InternalColumn).ValueTrue;
          end
          else
          begin
            Result := TBooleanColumnMapper(InternalColumn).ValueFalse;
          end;
        end;
      end;
    end;
  end
  else
  begin
    if InternalField.GetValue(InternalInstance).AsBoolean then
    begin
      Result := TBooleanColumnMapper(InternalColumn).ValueTrue;
    end
    else
    begin
      Result := TBooleanColumnMapper(InternalColumn).ValueFalse;
    end;
  end;
end;

procedure TBooleanColumnReader.DoGetFieldAttributeValuesFromColumnAttribute(pAttribute: CustomColumn);
var
  lColumn: TBooleanColumnMapper;
begin
  inherited;
  lColumn := TBooleanColumnMapper(InternalColumn);
  lColumn.ValueTrue := BooleanColumn(pAttribute).ValueTrue;
  lColumn.ValueFalse := BooleanColumn(pAttribute).ValueFalse;
  lColumn.InternalColumnType := BooleanColumn(pAttribute).InternalColumnType;
end;

function TBooleanColumnReader.GetColumnMapperClass: TColumnMapperClass;
begin
  Result := TBooleanColumnMapper;
end;
{ TEnumerationColumnReader }

function TEnumerationColumnReader.DoGetColumnValue: Variant;
begin
  case TEnumerationColumnMapper(InternalColumn).EnumType of
    emChar: Result := TEnumerationColumnMapper(InternalColumn).EnumCharOf[InternalField.GetValue(InternalInstance).AsOrdinal];
    emByte: Result := InternalField.GetValue(InternalInstance).AsOrdinal;
  end;
end;

procedure TEnumerationColumnReader.DoGetFieldAttributeValuesFromColumnAttribute(pAttribute: CustomColumn);
var
  lColumn: TEnumerationColumnMapper;
begin
  inherited;
  lColumn := TEnumerationColumnMapper(InternalColumn);
  lColumn.EnumType := EnumerationColumn(pAttribute).EnumType;
  lColumn.EnumCharOf := EnumerationColumn(pAttribute).EnumCharOf;
end;

function TEnumerationColumnReader.GetColumnMapperClass: TColumnMapperClass;
begin
  Result := TEnumerationColumnMapper;
end;
{ TJoinColumnReader }

procedure TJoinedColumnReader.DoGetFieldAttributeValuesFromReferenceColumnAttribute(pAttribute: CustomColumn);
var
  lJoinedColumnMapper: TJoinedColumnMapper;
begin
  lJoinedColumnMapper := TJoinedColumnMapper(InternalColumn);
  lJoinedColumnMapper.JoinKind := JoinedColumn(pAttribute).JoinKind;
  lJoinedColumnMapper.RefMetaClass := JoinedColumn(pAttribute).RefMetaClass;
  lJoinedColumnMapper.RefObjectName := JoinedColumn(pAttribute).RefTableName;
  lJoinedColumnMapper.RefObjectAlias := JoinedColumn(pAttribute).RefTableAlias;
  lJoinedColumnMapper.OriginalRefObjectAlias := JoinedColumn(pAttribute).RefTableAlias;
  lJoinedColumnMapper.Alias := JoinedColumn(pAttribute).Alias;
  lJoinedColumnMapper.RefResultColumnName := JoinedColumn(pAttribute).RefResultColumnName;
  lJoinedColumnMapper.UpdateAction := JoinedColumn(pAttribute).UpdateAction;
  lJoinedColumnMapper.DeleteAction := JoinedColumn(pAttribute).DeleteAction;
  if (ReadSubLevels) then
  begin
    DoGetReferences;
  end;
end;

procedure TJoinedColumnReader.DoGetReferences;
var
  lColumn: TJoinedColumnMapper;
begin
  lColumn := TJoinedColumnMapper(InternalColumn);
  if lColumn.RefMetaClass <> nil then
  begin
    GetReferencesFromClass(lColumn.RefMetaClass);
  end;
end;

function TJoinedColumnReader.GetColumnMapperClass: TColumnMapperClass;
begin
  Result := TJoinedColumnMapper;
end;

function TJoinedColumnReader.GetColumnReaderColumnType: TColumnType;
begin
  Result := ctyJoin;
end;
{ TListColumnReader }

function TDetailColumnReader.GetColumnMapperClass: TColumnMapperClass;
begin
  Result := TDetailColumnMapper;
end;

function TDetailColumnReader.GetColumnReaderColumnType: TColumnType;
begin
  Result := ctyDetail;
end;

procedure TDetailColumnReader.GetReferencesFromClass(pClass: TClass);
begin
  inherited;
  TReferenceColumnMapper(InternalColumn).RefMetaClass := pClass;
end;

{ TBlobColumnReader }

function TBlobColumnReader.DoGetColumnValue: Variant;
var
  lObject: TObject;
begin
  Result := Null;
  lObject := InternalFieldHelper.GetObjectValue(InternalInstance);
  if Assigned(lObject) then
  begin
    if lObject.InheritsFrom(TStrings) then
    begin
      TStrings(lObject).SaveToStream(TBlobColumnMapper(InternalColumn).StreamColumnValue);
    end
    else if lObject.InheritsFrom(TXML) then
    begin
      TXML(lObject).SaveToStream(TBlobColumnMapper(InternalColumn).StreamColumnValue);
    end
    else if lObject.InheritsFrom(TStream) then
    begin
      TStream(lObject).Position := 0;
      TBlobColumnMapper(InternalColumn).StreamColumnValue.CopyFrom(TStream(lObject), 0);
    end;
  end;
end;

function TBlobColumnReader.GetColumnMapperClass: TColumnMapperClass;
begin
  Result := TBlobColumnMapper;
end;
{ TAutoColumnReader }

procedure TAutoColumnReader.DoGetFieldAttributeValuesFromAutoMappingAttribute(pField: TRttiField);
var
  lAutoMappingValueGetterClass: TAutoMappingValueGetterClass;
  lValueGetter: TCustomAutoMappingValueGetter;
begin
  lAutoMappingValueGetterClass := TDefaultsClassRegister.DefaultAutoMappingValueGetter;
  lValueGetter := lAutoMappingValueGetterClass.Create;
  try
    InternalColumn.Name := lValueGetter.GetFieldNameFromColumnName(pField.Name);
    InternalColumn.Alias := lValueGetter.GetObjectAliasFromClassName(pField.Parent.Name);
    InternalColumn.ColumnOptions := lValueGetter.GetFieldOptionsFromColumnName(pField.Name);
    if InternalColumn.ClassType = TBooleanColumnMapper then
    begin
      lValueGetter.SetValuesToBooleanColumn(TBooleanColumnMapper(InternalColumn));
    end
    else if InternalColumn.ClassType = TEnumerationColumnMapper then
    begin
      lValueGetter.SetValuesToEnumerationColumn(TEnumerationColumnMapper(InternalColumn));
    end
    else
    begin
      lValueGetter.SetValuesToColumn(TCustomColumnMapper(InternalColumn));
    end;
  finally
    FreeAndNil(lValueGetter);
  end;
end;

function TAutoColumnReader.GetColumnMapperClass: TColumnMapperClass;
begin
  case ColumnType of
    ctyBoolean:
      Result := TBooleanColumnMapper;
    ctyEnumerator:
      Result := TEnumerationColumnMapper;
    ctyBlob, ctyMemo:
      Result := TBlobColumnMapper;
  else
    Result := TCustomColumnMapper;
  end;
end;

{ TReferencedColumnReader }

function TReferenceColumnReader.DoGetColumnValue: Variant;
var
  lObject: TObject;
  lLazy: ILazy;
  lLazyObject: TObject;
  lMapper: TObjectMapper;
  lParams: TObjectToMapperParams;
  procedure ExtractLazyObject;
  begin
    lLazyObject := nil;
    if Supports(lObject, ILazy, lLazy) then
    begin
      if lLazy.GetLoaded then
      begin
        lLazyObject := lLazy.GetObject;
        if Supports(lLazyObject, IFreedomObjectList) then
        begin
          lLazyObject := nil;
          Result := lLazy.GetLazyID;
        end;
      end
      else
      begin
        Result := lLazy.GetLazyID;
      end;
    end
    else
    begin
      lLazyObject := lObject;
    end;
  end;

begin
  Result := Null;
  if InternalInstance <> nil then
  begin
    if InternalField.FieldType.IsInstance then
    begin
      lObject := InternalFieldHelper.GetObjectValue(InternalInstance);
      if Assigned(lObject) then
      begin
        ExtractLazyObject;
        if Assigned(lLazyObject) then
        begin
          lParams := TObjectToMapperParams.Create;
          lParams.ObjectInstance := lLazyObject;
          lParams.Options := [];
          lMapper := TObjectToMapper.ObjectToMapper(lParams);
          try
            Result := lMapper.Columns.FindColumn(TReferenceColumnMapper(InternalColumn).RefColumnName).ColumnValue;
          finally
            TObjectToMapper.RemoveIncompleteMapper(lMapper.GetHashCode);
          end;
        end;
      end;
    end
    else
    begin
      InternalColumn.ColumnValue := InternalField.GetValue(InternalInstance).AsVariant
    end;
  end;
end;

procedure TReferenceColumnReader.DoGetFieldAttributeValuesFromColumnAttribute(pAttribute: CustomColumn);
var
  lColumn: TReferenceColumnMapper;
begin
  inherited;
  lColumn := TReferenceColumnMapper(InternalColumn);
  lColumn.RefColumnName := ReferenceColumn(pAttribute).RefColumnName;
  DoGetFieldAttributeValuesFromReferenceColumnAttribute(pAttribute);
  lColumn.ColumnType := GetColumnReaderColumnType;
  if GetSubLevels or IsChild then
  begin
    GetReferences;
  end;
end;

procedure TReferenceColumnReader.DoGetFieldAttributeValuesFromReferenceColumnAttribute(pAttribute: CustomColumn);
begin
  //  do when necessary
end;

procedure TReferenceColumnReader.DoGetReferences;
begin
  // do when necessary
end;

procedure TReferenceColumnReader.GetReferences;
var
  lClass: TClass;
begin
  if (InternalField <> nil) then
  begin
    if InternalField.FieldType.IsInstance then
    begin
      if InternalColumn.LazyOptions.IsLazy then
      begin
        lClass := InternalColumn.LazyOptions.LazyClassType;
      end
      else
      begin
        lClass := InternalField.FieldType.AsInstance.MetaclassType;
      end;
     TReferenceColumnMapper(InternalColumn).RefMetaClass := lClass;
     GetReferencesFromClass(lClass);
    end else
    begin
      DoGetReferences;
    end;
  end;
end;

procedure TReferenceColumnReader.GetReferencesFromClass(pClass: TClass);
var
  lMapper: TObjectMapper;
  lParams: TObjectToMapperParams;
  lRefColumn: TReferenceColumnMapper;
  lColumn: TCustomColumnMapper;
begin
  lRefColumn := TReferenceColumnMapper(InternalColumn);
  lParams := TObjectToMapperParams.Create;
  lParams.MetaClassType := pClass;
  lParams.Options := [];
  lMapper := TObjectToMapper.ObjectToMapper(lParams);
  try
    TReferenceColumnMapper(InternalColumn).RefObjectAlias := lMapper.Alias;
    TReferenceColumnMapper(InternalColumn).OriginalRefObjectAlias := lMapper.Alias;
    TReferenceColumnMapper(InternalColumn).RefObjectName := lMapper.Name;
    if lMapper.Columns.FindColumn(lRefColumn.RefColumnName) <> nil then
    begin
      lColumn := lMapper.Columns.FindColumn(lRefColumn.RefColumnName, ctyUnknow, [ctyDetail, ctyJoin]);
      lRefColumn.RefColumnType := lColumn.ColumnType;
    end
    else
    begin
      raise EInvalidReferenceClass.Create(lRefColumn.RefColumnName, InternalField.Parent.Name, InternalField.Name);
    end;
  finally
    TObjectToMapper.RemoveIncompleteMapper(lMapper.GetHashCode);
  end;
end;

{ TExtensioColumnReader }

function TExtensionColumnReader.GetColumnMapperClass: TColumnMapperClass;
begin
  Result := TExtensionColumnMapper;
end;

procedure TExtensionColumnReader.GetFieldAttributeValues(pField: TRttiField);
begin
  InternalColumn.ColumnType := ctyExtension;
  InternalColumn.Name := pField.Name;
  ReadExtensionClass;
end;

procedure TExtensionColumnReader.ReadExtensionClass;
var
  lObjectToMapperParams: TObjectToMapperParams;
  lObjectMapper: TObjectMapper;
  lColumn: TCustomColumnMapper;
  lExtensionColumn: TExtensionColumnMapper;
  lIndex: Integer;
begin
  if (Assigned(InternalField) and Assigned(ObjectMapper)) and (ReadSubLevels) then
  begin
    lExtensionColumn := TExtensionColumnMapper(InternalColumn);
    lObjectToMapperParams := TObjectToMapperParams.Create;
    lObjectToMapperParams.MetaClassType := InternalField.FieldType.AsInstance.MetaclassType;
    if (InternalInstance <> nil) then
    begin
      lObjectToMapperParams.ObjectInstance := InternalFieldHelper.GetObjectValue(InternalInstance);
    end;
    lObjectToMapperParams.IsExtension := True;
    lObjectMapper := TObjectToMapper.ObjectToMapper(lObjectToMapperParams);
    try
      for lIndex := lObjectMapper.Columns.Count - 1 downto 0 do
      begin
        lCOlumn := lObjectMapper.Columns.Items[lIndex];
        if SameText(InternalField.FieldType.AsInstance.MetaclassType.ClassName, lColumn.RttiOptions.RttiField.Parent.AsInstance.MetaclassType.ClassName) then
        begin
          lExtensionColumn.ChildColumns.Add(lColumn);
          lColumn.ParentColumn := lExtensionColumn;
        end;
        lObjectMapper.Columns.Extract(lColumn);
        ObjectMapper.Columns.Add(lColumn);
      end;
    finally
      TObjectToMapper.RemoveIncompleteMapper(lObjectMapper.GetHashCode);
    end;
  end;
end;

end.
