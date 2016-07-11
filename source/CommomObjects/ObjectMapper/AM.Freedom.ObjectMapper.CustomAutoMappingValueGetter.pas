unit AM.Freedom.ObjectMapper.CustomAutoMappingValueGetter;

interface

uses
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.ObjectMapper.CustomColumnMapper;

type
  TAutoMappingValueGetterClass = class of TCustomAutoMappingValueGetter;

  TCustomAutoMappingValueGetter = class
  strict protected
    function GetDefaultSizeForSizeableFields: Integer; virtual;
    function GetDefaultSizeForScalableFields: Integer; virtual;
    function GetDefaultScaleForScalableFields: Integer; virtual;
    function GetDefaultTrueValueForBooleanColumns: Variant; virtual;
    function GetDefaultFalseValueForBooleanColumns: Variant; virtual;
  public
    function GetObjectNameFromClassName(pClassName: String): String; virtual;
    function GetObjectAliasFromClassName(pClassName: String): String; virtual;
    function GetFieldNameFromColumnName(pColumnName: String): String; virtual;
    function GetFieldOptionsFromColumnName(pColumnName: String): TColumnOptions; virtual;
    procedure SetValuesToBooleanColumn(pColumn: TBooleanColumnMapper); virtual;
    procedure SetValuesToEnumerationColumn(pColumn: TEnumerationColumnMapper); virtual;
    procedure SetValuesToColumn(pColumn: TCustomColumnMapper); virtual;
  end;

implementation

{ TCustomAutoMappingValueGetter }

function TCustomAutoMappingValueGetter.GetDefaultFalseValueForBooleanColumns: Variant;
begin
  Result := 0;
end;

function TCustomAutoMappingValueGetter.GetDefaultScaleForScalableFields: Integer;
begin
  Result := 6;
end;

function TCustomAutoMappingValueGetter.GetDefaultSizeForScalableFields: Integer;
begin
  Result := 15;
end;

function TCustomAutoMappingValueGetter.GetDefaultSizeForSizeableFields: Integer;
begin
  Result := 150;
end;

function TCustomAutoMappingValueGetter.GetDefaultTrueValueForBooleanColumns: Variant;
begin
  Result := 1;
end;

function TCustomAutoMappingValueGetter.GetFieldNameFromColumnName(pColumnName: String): String;
begin
  Result := Copy(pColumnName, 2, Length(pColumnName));
end;

function TCustomAutoMappingValueGetter.GetFieldOptionsFromColumnName(pColumnName: String): TColumnOptions;
begin
  Result := [];
end;

function TCustomAutoMappingValueGetter.GetObjectAliasFromClassName(pClassName: String): String;
begin
  Result := GetObjectNameFromClassName(pClassName);
  Result := Copy(Result, 1, 4);
end;

function TCustomAutoMappingValueGetter.GetObjectNameFromClassName(pClassName: String): String;
begin
  Result := Copy(pClassName, 2, Length(pClassName));
end;

procedure TCustomAutoMappingValueGetter.SetValuesToBooleanColumn(pColumn: TBooleanColumnMapper);
begin
  pColumn.ValueTrue := GetDefaultTrueValueForBooleanColumns;
  pColumn.ValueFalse := GetDefaultFalseValueForBooleanColumns;
end;

procedure TCustomAutoMappingValueGetter.SetValuesToColumn(pColumn: TCustomColumnMapper);
begin
  if pColumn.ColumnType.Sizeable then
  begin
    pColumn.Size := GetDefaultSizeForSizeableFields;
  end;
  if pColumn.ColumnType.Scalable then
  begin
    pColumn.Size := GetDefaultSizeForScalableFields;
    pColumn.Scale := GetDefaultScaleForScalableFields;
  end;
end;

procedure TCustomAutoMappingValueGetter.SetValuesToEnumerationColumn(pColumn: TEnumerationColumnMapper);
begin
  pColumn.EnumType := emByte;
end;

end.
