unit AM.Freedom.ObjectMapper.CustomColumnReader;

interface

uses
  System.Rtti,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.Attributes,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.FreedomAttributes,
  AM.Freedom.Helper.RttiField,
  AM.Freedom.Exceptions,
  AM.Freedom.ObjectMapper;

type
  TFieldToMapperParams = class sealed
  private
    FRttiField: TRttiField;
    FObjectMapper: TObjectMapper;
    FSubLevels: Boolean;
    FObjectInstance: TObject;
    FIsChild: Boolean;
  public
    property RttiField: TRttiField read FRttiField write FRttiField;
    property ObjectInstance: TObject read FObjectInstance write FObjectInstance;
    property SubLevels: Boolean read FSubLevels write FSubLevels;
    property ObjectMapper: TObjectMapper read FObjectMapper write FObjectMapper;
    property IsChild: Boolean read FIsChild write FIsChild;
  end;

  TColumnReaderClass = class of TCustomColumnReader;

  TCustomColumnReader = class
  strict private
    FColumnType: TColumnType;
    FInstance: TObject;
    FColumn: TCustomColumnMapper;
    FField: TRttiField;
    FFieldHelper: TRttiFieldHelper;
    FReadSubLevels: Boolean;
    FObjectMapper: TObjectMapper;
    procedure DoGetColumnOptionsFromAttribute(pAttribute: TCustomAttribute);
    procedure ReadLazyOptions(pField: TRttiField);
    procedure GetColumnValue;
    procedure ReadPropertyAttributes(pProperty: TRttiProperty);
    function NullIfNullableCompare(pValue: Variant): Variant;
  private
    FIsChild: Boolean;
  strict protected
    property InternalInstance: TObject read FInstance;
    property InternalColumn: TCustomColumnMapper read FColumn;
    property InternalField: TRttiField read FField;
    property InternalFieldHelper: TRttiFieldHelper read FFieldHelper;
    property ObjectMapper: TObjectMapper read FObjectMapper;
    property ReadSubLevels: Boolean read FReadSubLevels;
    property IsChild: Boolean read FIsChild;
  strict protected
    function GetSubLevels: Boolean;
    procedure DoGetFieldAttributeValuesFromAutoMappingAttribute(pField: TRttiField); virtual;
    procedure DoGetFieldAttributeValuesFromColumnAttribute(pAttribute: CustomColumn); virtual;
    function DoGetColumnValue: Variant; virtual;
    procedure GetFieldAttributeValues(pField: TRttiField); virtual;
    function GetColumnMapperClass: TColumnMapperClass; virtual;
  public
    destructor Destroy; override;
    function FieldToMapper(pFieldToMapperParams: TFieldToMapperParams): TCustomColumnMapper; virtual;
    function PropertyToMapper(pProperty: TRttiProperty; pInstance: TObject; pSubLevels: Boolean): TCustomColumnMapper; virtual;
    property ColumnType: TColumnType read FColumnType;
  end;

  TCustomNamedColumnReader = class(TCustomColumnReader)
  strict protected
    procedure DoGetFieldAttributeValuesFromColumnAttribute(pAttribute: CustomColumn); override;
  end;

implementation

uses
  System.Classes,
  AM.Freedom.Helper.RttiType,
  AM.Freedom.Helper.ColumnType,
  System.StrUtils,
  AM.Freedom.ILazy,
  System.SysUtils,
  System.Variants,
  AM.Freedom.INullable,
  AM.Freedom.Helper.RttiProperty,
  AM.Freedom.NullableCompare;

{ TColumnReader }

procedure TCustomColumnReader.GetColumnValue;
begin
  if Assigned(FInstance) then
  begin
    FColumn.ColumnValue := DoGetColumnValue;
  end;
end;

function TCustomColumnReader.DoGetColumnValue: Variant;
begin
  if (not FColumn.IsNullable) then
  begin
    Result := NullIfNullableCompare(FFieldHelper.GetVariantValue(FInstance, FColumnType));
  end
  else
  begin
    Result := FFieldHelper.GetNullableVariantValue(FInstance);
  end;
end;

destructor TCustomColumnReader.Destroy;
begin
  FreeAndNil(FFieldHelper);
  inherited;
end;

procedure TCustomColumnReader.DoGetColumnOptionsFromAttribute(pAttribute: TCustomAttribute);
var
  lSchema: string;
begin
  if pAttribute.ClassType = Id then
  begin
    FColumn.IdOptions.IsId := True;
    FColumn.IdOptions.SequenceName := Id(pAttribute).SequenceName;
    FColumn.IdOptions.IdOption := Id(pAttribute).IdOption;
    for lSchema in Id(pAttribute).Schemas do
    begin
      FColumn.IdOptions.Schemas.AddSchema(lSchema, False);
    end;
  end
  else if pAttribute.ClassType = Order then
  begin
    FColumn.OrderOptions.Index := Order(pAttribute).Index;
    FColumn.OrderOptions.OrderType := Order(pAttribute).OrderType;
    FColumn.OrderOptions.Directive := Order(pAttribute).Directive;
  end
  else if pAttribute.ClassType = DefaultValue then
  begin
    FColumn.DefaultValueOptions.IsNow := False;
    FColumn.DefaultValueOptions.Value := DefaultValue(pAttribute).Value;
  end
  else if pAttribute.ClassType = DefaultNowValue then
  begin
    FColumn.DefaultValueOptions.IsNow := True;
  end
  else if pAttribute.InheritsFrom(Schema) then
  begin
    FColumn.Schemas.AddSchema(Schema(pAttribute).Name, Schema(pAttribute).Default);
  end
  else if pAttribute.InheritsFrom(Domain) then
  begin
    FColumn.Domain := Domain(pAttribute).Name;
  end
  else if pAttribute.InheritsFrom(Rounded) then
  begin
    FColumn.RoundOptions.IsRounded := True;
    FColumn.RoundOptions.RoundDecimals := Rounded(pAttribute).RoundDecimals;
    FColumn.RoundOptions.RoundDecimalsMode := Rounded(pAttribute).RoundDecimalsMode;
    FColumn.RoundOptions.CanBeModified := Rounded(pAttribute).CanBeModified;
  end
end;

procedure TCustomColumnReader.DoGetFieldAttributeValuesFromAutoMappingAttribute(pField: TRttiField);
begin
  raise EInvalidMethodCallOnClass.Create('DoGetFieldAttributeValuesFromAutoMappingAttribute', ClassName);
end;

procedure TCustomColumnReader.DoGetFieldAttributeValuesFromColumnAttribute(pAttribute: CustomColumn);
begin
  // do when necessary
end;

function TCustomColumnReader.FieldToMapper(pFieldToMapperParams: TFieldToMapperParams): TCustomColumnMapper;
begin
  try
    FReadSubLevels := pFieldToMapperParams.SubLevels;
    FColumnType := pFieldToMapperParams.RttiField.FieldType.ToColumnType;
    FInstance := pFieldToMapperParams.ObjectInstance;
    FField := pFieldToMapperParams.RttiField;
    FFieldHelper := TRttiFieldHelper.Create(FField);
    FColumn := GetColumnMapperClass.Create;
    FObjectMapper := pFieldToMapperParams.ObjectMapper;
    FIsChild := pFieldToMapperParams.IsChild;
    Result := FColumn;
    Result.ColumnType := FColumnType;
    Result.IsNullable := (FField.FieldType.IsInstance) and Supports(FField.FieldType.AsInstance.MetaClassType, INullable);
    GetFieldAttributeValues(FField);
  finally
    FInstance := nil;
    FField := nil;
    FColumn := nil;
    pFieldToMapperParams.Free;
  end;
end;

function TCustomColumnReader.GetColumnMapperClass: TColumnMapperClass;
begin
  Result := TCustomColumnMapper;
end;

procedure TCustomColumnReader.GetFieldAttributeValues(pField: TRttiField);
var
  lAttribute: TCustomAttribute;
begin
  ReadLazyOptions(pField);
  for lAttribute in pField.GetAttributes do
  begin
    if lAttribute.ClassType.InheritsFrom(CustomColumn) then
    begin
      if lAttribute.ClassType.InheritsFrom(Column) then
      begin
        FColumn.Size := Column(lAttribute).Size;
        FColumn.Scale := Column(lAttribute).Scale;
      end;
      DoGetFieldAttributeValuesFromColumnAttribute(CustomColumn(lAttribute));
      FColumn.ColumnOptions := CustomColumn(lAttribute).Options;
      GetColumnValue;
    end
    else if lAttribute.ClassType = AutoMapping then
    begin
      DoGetFieldAttributeValuesFromAutoMappingAttribute(pField);
    end

    else if lAttribute.ClassType.InheritsFrom(FreedomAttribute) or lAttribute.ClassType.InheritsFrom(DefaultValue) then
    begin
      DoGetColumnOptionsFromAttribute(lAttribute);
    end;
  end;
end;

function TCustomColumnReader.GetSubLevels: Boolean;
begin
  Result := FReadSubLevels;
end;

function TCustomColumnReader.NullIfNullableCompare(pValue: Variant): Variant;
begin
  Result := TNullableCompare.NullIfNullable(pValue);
end;

function TCustomColumnReader.PropertyToMapper(pProperty: TRttiProperty; pInstance: TObject;
  pSubLevels: Boolean): TCustomColumnMapper;
var
  lValue: TValue;
  lObject: TObject;
  lAttribute: TCustomAttribute;
begin
  try
    for lAttribute in pProperty.GetAttributes do
    begin
      if (lAttribute.InheritsFrom(NoMapping)) then
      begin
        Exit(nil);
      end;
    end;
    FColumn := GetColumnMapperClass.Create;
    FColumn.Name := pProperty.Name;
    FColumn.ColumnType := pProperty.PropertyType.ToColumnType;
    FColumn.IsNullable := (pProperty.PropertyType.IsInstance) and Supports(pProperty.PropertyType.AsInstance.MetaclassType, INullable);
    if (Assigned(pInstance)) then
    begin
      if FColumn.ColumnType.IsSimpleType then
      begin
        FColumn.ColumnValue := pProperty.GetVariantValue(pInstance, FColumn.ColumnType);
      end
      else if FColumn.ColumnType.IsOrdinal then
      begin
        if FColumn.IsNullable then
        begin
          FColumn.ColumnValue := pProperty.GetNullableVariantValue(pInstance);
        end
        else
        begin
          lValue := pProperty.GetValue(pInstance);
          FColumn.ColumnValue := lValue.AsOrdinal;
        end;

      end
      else if FColumn.ColumnType.IsObject then
      begin
        if FColumn.ColumnType.IsEquals(ctyMemo) then
        begin
          lObject := pProperty.GetValue(pInstance).AsObject;
          if Assigned(lObject) then
          begin
            if lObject.InheritsFrom(TStrings) then
            begin
              FColumn.ColumnValue := TStrings(lObject).Text;
            end;
          end;
        end;
      end;
    end;
    ReadPropertyAttributes(pProperty);
    Result := FColumn;
  finally
    FInstance := nil;
    FField := nil;
    FColumn := nil;
  end;
end;

procedure TCustomColumnReader.ReadLazyOptions(pField: TRttiField);
var
  lIsLazy: Boolean;
  lObject: TObject;
  lLazy: ILazy;
begin
  lIsLazy := pField.FieldType.IsInstance;
  if lIsLazy then
  begin
    lIsLazy := Supports(pField.FieldType.AsInstance.MetaclassType, ILazy);
    if lIsLazy then
    begin
      FColumn.LazyOptions.LazyClassType := pField.FieldType.AsInstance.GetField('FClassType').FieldType.AsInstance.MetaclassType;
      if Assigned(FInstance) then
      begin
        lObject := pField.GetValue(FInstance).AsObject;
        if Assigned(lObject) then
        begin
          if Supports(lObject, ILazy, lLazy) then
          begin
            FColumn.LazyOptions.IsLazyLoaded := lLazy.GetLoaded;
          end;
        end;
      end;
    end;
  end;
  FColumn.LazyOptions.IsLazy := lIsLazy
end;
procedure TCustomColumnReader.ReadPropertyAttributes(pProperty: TRttiProperty);
var
  lAttribute: TCustomAttribute;
begin
  for lAttribute in pProperty.GetAttributes do
  begin
    if lAttribute.InheritsFrom(Bind) then
    begin
      FColumn.BindOptions.DisplayFormat := Bind(lAttribute).DisplayFormat;
      FColumn.BindOptions.DisplayLabel := Bind(lAttribute).DisplayLabel;
    end;
  end;
end;

{ TCustomNameColumnReader }

procedure TCustomNamedColumnReader.DoGetFieldAttributeValuesFromColumnAttribute(pAttribute: CustomColumn);
begin
  inherited;
  InternalColumn.Name := CustomNamedColumn(pAttribute).Name;
  InternalColumn.Alias := CustomNamedColumn(pAttribute).Alias;
end;

end.
