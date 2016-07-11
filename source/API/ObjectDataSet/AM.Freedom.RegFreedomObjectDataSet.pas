unit AM.Freedom.RegFreedomObjectDataSet;

interface

uses
  System.Rtti,
  System.Classes,
  ToolsAPI,
  DesignEditors,
  DesignIntf,
  Data.DB,
  AM.UnitReader.Members.ClassMember,
  AM.Freedom.ClassesFinder,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.FreedomObject,
  AM.Freedom.FreedomObjectDataSet,
  AM.Freedom.frmFieldTypeFromPropertyTypeName,
  AM.Freedom.ObjectMapper,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.ObjectMapper.CustomColumnMapper;

type
  TFreedomObjectDataSetPropertyEditor = class(TStringProperty)
  strict private
    procedure GenerateFieldsWithClassMember(pClassMember: TClassMember);
    function GetFieldTypeFromPropertyTypeName(pPropertyName, pPropertyTypeName: String): TFieldType;
    procedure DeleteAllFields;
    function FindFieldDef(pFieldName: String): TFieldDef;
    procedure DeleteFieldDef(pFieldDef: TFieldDef);
    function GetMetaClassFromClassName(pClassName: String): TClass;
    function CurrentDataSet: TCustomFreedomObjectDataSet;
    procedure GenerateFieldsWithMetaClass(pMetaClass: TClass);
    function ExtractMapperFromClass(pMetaClass: TClass): TObjectMapper;
    procedure AddFieldFromColumn(const  pColumn: TCustomColumnMapper);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

procedure Register;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.GroupFilterCriteria,
  AM.UnitReader.Members.UnitMember,
  AM.UnitReader.Enumerations,
  AM.UnitReader.Members.PropertyMember,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.IFreedomObjectList;

procedure Register;
begin
  RegisterComponents('Data Acess', [TFreedomObjectDataSet]);
  RegisterPropertyEditor(TypeInfo(String), TFreedomObjectDataSet, 'ObjectClassName', TFreedomObjectDataSetPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TGroupFilterCriteria), TFreedomObjectDataSet, 'FilterCriteria', nil);
end;


{ TFreedomObjectDataSetPropertyEditor }

function TFreedomObjectDataSetPropertyEditor.CurrentDataSet: TCustomFreedomObjectDataSet;
begin
  Result := (GetComponent(0) as TCustomFreedomObjectDataSet);
end;

procedure TFreedomObjectDataSetPropertyEditor.DeleteAllFields;
var
  lDataSet: TCustomFreedomObjectDataSet;
begin
  lDataSet := CurrentDataSet;
  lDataSet.FieldDefs.Clear;
  lDataSet.Fields.Clear;
end;

procedure TFreedomObjectDataSetPropertyEditor.GenerateFieldsWithClassMember(pClassMember: TClassMember);
var
  lProperty: TPropertyMember;
  lDataSet: TCustomFreedomObjectDataSet;
  lFieldDef: TFieldDef;
  lFieldType: TFieldType;
  lIndex: Integer;

  function FieldTypeFromPropertyType: TFieldType;
  begin
    case lProperty.PropertyType of
      mtByte, mtInt8, mtUInt8: Result := ftByte;
      mtSmallInt, mtShortInt, mtInt16, mtUInt16: Result := ftSmallint;
      mtCardinal, mtInteger, mtInt32, mtUInt32, mtWord: Result := ftInteger;
      mtInt64, mtUInt64: Result := ftLargeint;
      mtSingle, mtReal, mtDouble, mtExtended, mtCurrency: Result := ftFloat;
      mtBoolean: Result := ftBoolean;
      mtDate: Result := ftDate;
      mtTime: Result := ftTime;
      mtDateTime: Result := ftDateTime;
      mtTStrings: Result := ftWideMemo;
      mtUnknow: Result := GetFieldTypeFromPropertyTypeName(lProperty.Name, lProperty.PropertyTypeName);
      else
        Result := ftString;
    end;
  end;
begin
  if (Assigned(Designer)) then
  begin
    lDataSet := CurrentDataSet;
    if (csDesigning in lDataSet.ComponentState) then
    begin
      if Assigned(pClassMember) then
      begin
        for lProperty in pClassMember.Properties do
        begin
          lFieldType := FieldTypeFromPropertyType;
          if lFieldType <> ftUnknown then
          begin
            lFieldDef := FindFieldDef(lProperty.Name);
            if (lFieldDef <> nil) then
            begin
              if (lFieldDef.DataType <> lFieldType) then
              begin
                lFieldDef.Free;
              end;
            end;
            try
              lFieldDef := lDataSet.FieldDefs.AddFieldDef;
              lFieldDef.Name := lProperty.Name;
              lFieldDef.DataType := lFieldType;
              if (lFieldDef.DataType = ftString) then
              begin
                lFieldDef.Size := 250;
              end;
            except
            end;
          end
          else
          begin
            lFieldDef := FindFieldDef(lProperty.Name);
            if (lFieldDef <> nil) then
            begin
              DeleteFieldDef(lFieldDef);
            end;
          end;
        end;
        for lIndex := lDataSet.FieldDefs.Count - 1 downto 0 do
        begin
          if not pClassMember.Properties.FindProperty(lDataSet.FieldDefs.Items[lIndex].Name) then
          begin
            DeleteFieldDef(lDataSet.FieldDefs.Items[lIndex])
          end;
        end;
      end
      else
      begin
        DeleteAllFields;
      end;
      Designer.Modified;
    end;
  end;
end;

procedure TFreedomObjectDataSetPropertyEditor.DeleteFieldDef(pFieldDef: TFieldDef);
var
  lField: TField;
begin
  lField := CurrentDataSet.Fields.FindField(pFieldDef.Name);
  if (Assigned(lField)) then
  begin
    lField.Free;
  end;
  pFieldDef.Free;
end;

function TFreedomObjectDataSetPropertyEditor.FindFieldDef(pFieldName: String): TFieldDef;
var
  lIndex: Integer;
  lDataSet: TCustomFreedomObjectDataSet;
  lField: TFieldDef;
begin
  lDataSet := CurrentDataSet;
  Result := nil;
  for lIndex := 0 to lDataSet.FieldDefs.Count - 1 do
  begin
    lField := lDataSet.FieldDefs.Items[lIndex];
    if SameText(lField.Name, pFieldName) then
    begin
      Result := lField;
      Break;
    end;
  end;
end;

function TFreedomObjectDataSetPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

function TFreedomObjectDataSetPropertyEditor.GetFieldTypeFromPropertyTypeName(pPropertyName, pPropertyTypeName: String): TFieldType;
begin
  Result := TfrmFieldTypeFromPropertyTypeName.ExtractFieldType(pPropertyName, pPropertyTypeName);
end;

procedure TFreedomObjectDataSetPropertyEditor.GetValues(Proc: TGetStrProc);
var
  lClasses: TClassList;
  lClass: TClassMember;
begin
  lClasses := TClassesFinder.GetClasses;
  if (Assigned(lClasses)) then
  begin
    for lClass in lClasses do
    begin
      if (lClass.Properties.Count > 0) and (lClass.ParentName <> 'TForm') and
          (lClass.ParentName <> 'TDataModule') then
      begin
        Proc(lClass.UnitClassName + '.' + lClass.Name);
      end;
    end;
  end;
end;

procedure TFreedomObjectDataSetPropertyEditor.SetValue(const Value: string);
var
  lClasses: TClassList;
  lClass: TClassMember;
  lMetaClass: TClass;
begin
  inherited;
  lMetaClass := GetMetaClassFromClassName(Value);
  if (Assigned(lMetaClass)) then
  begin
    GenerateFieldsWithMetaClass(lMetaClass);
  end
  else
  begin
    try
      lClasses := TClassesFinder.GetClasses;
      if (Assigned(lClasses)) then
      begin
        lClass := lClasses.FindClass(Value);
        GenerateFieldsWithClassMember(lClass);
      end;
    finally
      FreeAndNil(lClasses);
    end;
  end;
end;

procedure TFreedomObjectDataSetPropertyEditor.GenerateFieldsWithMetaClass(pMetaClass: TClass);
var
  lMapper: TObjectMapper;
  lColumn: TCustomColumnMapper;
  lIndex: Integer;
  lFieldDef: TFieldDef;
begin
  lMapper := ExtractMapperFromClass(pMetaClass);
  try
    for lColumn in lMapper.Columns do
    begin
      AddFieldFromColumn(lColumn);
    end;
    for lIndex := CurrentDataSet.FieldDefs.Count - 1 downto 0 do
    begin
      lFieldDef := CurrentDataSet.FieldDefs.Items[lIndex];
      if (lMapper.Columns.FindColumn(lFieldDef.Name) = nil) then
      begin
        DeleteFieldDef(lFieldDef);
      end;
    end;
  finally
    TObjectToMapper.UnlockMapper(lMapper.GetHashCode);
  end;
end;

procedure TFreedomObjectDataSetPropertyEditor.AddFieldFromColumn(const  pColumn: TCustomColumnMapper);
var
  lFieldDef: TFieldDef;
begin
  lFieldDef := FindFieldDef(pColumn.Name);
  if (Assigned(lFieldDef)) then
  begin
    if (lFieldDef.DataType <> pColumn.ColumnType.ToFieldType) then
    begin
      DeleteFieldDef(lFieldDef);
      lFieldDef := nil;
    end;
  end;
  if (not Assigned(lFieldDef)) then
  begin
    try
      if (not pColumn.RttiOptions.RttiProperty.PropertyType.IsInstance) then
      begin
        lFieldDef := CurrentDataSet.FieldDefs.AddFieldDef;
        lFieldDef.Name := pColumn.Name;
        lFieldDef.DataType := pColumn.ColumnType.ToFieldType;
        if (lFieldDef.DataType = ftString) then
        begin
          lFieldDef.Size := 250;
        end;
      end
      else if (Supports(pColumn.RttiOptions.RttiProperty.PropertyType.AsInstance.MetaClassType, IFreedomObjectList)) then
      begin
        lFieldDef := CurrentDataSet.FieldDefs.AddFieldDef;
        lFieldDef.Name := pColumn.Name;
        lFieldDef.DataType := ftDataSet;
      end;
    except
    end;
  end;
end;

function TFreedomObjectDataSetPropertyEditor.ExtractMapperFromClass(pMetaClass: TClass): TObjectMapper;
var
  lParams: TObjectToMapperParams;
begin
  lParams := TObjectToMapperParams.Create;
  lParams.MetaClassType := pMetaClass;
  lParams.Options := [Properties];
  Result := TObjectToMapper.ObjectToMapper(lParams);
end;

function TFreedomObjectDataSetPropertyEditor.GetMetaClassFromClassName(pClassName: String): TClass;
var
  lContext: TRttiContext;
  lType: TRttiType;
begin
  lType := lContext.FindType(pClassName);
  Result := nil;
  if (Assigned(lType)) then
  begin
    Result := lType.AsInstance.MetaclassType
  end;
end;

end.
