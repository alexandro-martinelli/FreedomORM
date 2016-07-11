unit AM.Freedom.ObjectMapper.CustomColumnMapper;

interface

uses
  System.Generics.Collections,
  System.Rtti,
  AM.Freedom.ObjectMapper.CustomMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.ColumnOptions,
  AM.Freedom.ObjectMapper.CustomColumn,
  AM.Freedom.ObjectMapper.Schemas, System.Classes;

type
  TColumnMapperClass = class of TCustomColumnMapper;

  TCustomColumnMapper = class(TCustomColumn)
  strict private
    FOrderOptions: TOrderOptions;
    FLazyOptions: TLazyOptions;
    FColumnValue: Variant;
    FRttiOptions: TRttiColumnOptions;
    FBindOptions: TBindOptions;
    FSize: UInt32;
    FScale: Byte;
    FSchemas: TSchemas;
    FDuplicatedAlias: Boolean;
    FIsNullable: Boolean;
    FRoundOptions: TRoundOptions;
    FIsChild: Boolean;
    FParentColumn: TCustomColumnMapper;

    function GetIsNull: Boolean;
    procedure SetColumnValue(const pValue: Variant);
    function GetAsTValue: TValue;
    function GetIsExtension: Boolean;
    function GetCurrentValue: Variant;
    function GetValueFromField: Variant;
    function GetValueFromProperty: Variant;
    function GetCurrentStream: TStream;
    function GetIsNotNull: Boolean;
  protected
    function GetBooleanTValue: TValue; virtual;
    function GetEnumeratiorTValue: TValue; virtual;
    function GetJoinedTValue: TValue; virtual;
    function DoGetNullableValue(pInitialValue: Variant): Variant; virtual;
    function DoGetCurrentValue(pFieldValue: Variant): Variant; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Alias;
    property Domain;
    property Size: UInt32 read FSize write FSize;
    property Scale: Byte read FScale write FScale;
    property ColumnValue: Variant read FColumnValue write SetColumnValue;
    property OrderOptions: TOrderOptions read FOrderOptions;
    property LazyOptions: TLazyOptions read FLazyOptions;
    property RttiOptions: TRttiColumnOptions read FRttiOptions;
    property BindOptions: TBindOptions read FBindOptions;
    property RoundOptions: TRoundOptions read FRoundOptions;
    property DuplicatedAlias: Boolean read FDuplicatedAlias write FDuplicatedAlias;
    property IsNullable: Boolean read FIsNullable write FIsNullable;
    property IsChild: Boolean read FIsChild write FIsChild;
    property IsNull: Boolean read GetIsNull;
    property IsNotNull: Boolean read GetIsNotNull;
    property AsTValue: TValue read GetAsTValue;
    property Schemas: TSchemas read FSchemas;
    property IsExtension: Boolean read GetIsExtension;
    property ParentColumn: TCustomColumnMapper read FParentColumn write FParentColumn;
    property CurrentValue: Variant read GetCurrentValue;
    property CurrentStream: TStream read GetCurrentStream;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.Helper.Variant,
  AM.Freedom.DefaultsClassRegister,
  AM.Freedom.CustomNullable,
  AM.Freedom.NullableCompare,
  AM.Freedom.Helper.RttiField,
  AM.Freedom.Helper.RttiProperty,
  AM.Freedom.ILazy,
  AM.Freedom.XML;

{ TCustomColumnMapper }

constructor TCustomColumnMapper.Create;
begin
  inherited Create;
  FOrderOptions := TOrderOptions.Create;
  FLazyOptions := TLazyOptions.Create;
  FRttiOptions := TRttiColumnOptions.Create;
  FColumnValue := null;
  FSchemas := TSchemas.Create;
  FBindOptions := TBindOptions.Create;
  FRoundOptions := TRoundOptions.Create;
end;

destructor TCustomColumnMapper.Destroy;
begin
  FOrderOptions.Free;
  FLazyOptions.Free;
  FRttiOptions.Free;
  FSchemas.Free;
  FBindOptions.Free;
  FRoundOptions.Free;
  inherited;
end;

function TCustomColumnMapper.DoGetCurrentValue(pFieldValue: Variant): Variant;
begin
  Result := pFieldValue;
end;

function TCustomColumnMapper.DoGetNullableValue(pInitialValue: Variant): Variant;
begin
  Result := pInitialValue;
end;

function TCustomColumnMapper.GetAsTValue: TValue;
begin
  case ColumnType of
    ctyByte:
      Result := TValue.From<Byte>(ColumnValue.TryToByte);
    ctySmallint:
      Result := TValue.From<SmallInt>(ColumnValue.TryToInt);
    ctyInteger:
      Result := TValue.From<Integer>(ColumnValue.TryToInt);
    ctyInt64:
      Result := TValue.From<Int64>(ColumnValue.TryToInt64);
    ctyChar, ctyString:
      Result := TValue.From<String>(ColumnValue.ToString);
    ctySingle:
      Result := TValue.From<Single>(ColumnValue.TryToSingle);
    ctyDouble:
      Result := TValue.From<Double>(ColumnValue.TryToDouble);
    ctyCurrency:
      Result := TValue.From<Currency>(ColumnValue.TryToCurrency);
    ctyExtended:
      Result := TValue.From<Extended>(ColumnValue.TryToExtended);
    ctyDate:
      Result := TValue.From<TDate>(ColumnValue.TryToDate);
    ctyTime:
      Result := TValue.From<TTime>(ColumnValue.TryToTime);
    ctyDateTime:
      Result := TValue.From<TDateTime>(ColumnValue.TryToDateTime);
    ctyBoolean:
      Result := GetBooleanTValue;
    ctyEnumerator:
      Result := GetEnumeratiorTValue;
    ctyJoin:
      Result := GetJoinedTValue;
  end;
end;

function TCustomColumnMapper.GetBooleanTValue: TValue;
begin
  Result := nil;
end;

function TCustomColumnMapper.GetCurrentStream: TStream;
var
  lObject: TObject;
begin
  Result := nil;
  lObject := FRttiOptions.RttiFieldHelper.GetObjectValue(FRttiOptions.RttiObject);
  if (Assigned(lObject)) then
  begin
    if (ColumnType = ctyMemo) then
    begin
      Result := TStringStream.Create;
      Result.Position := 0;
      TStrings(lObject).SaveToStream(Result);
    end
    else if (ColumnType = ctyXML) then
    begin
      Result := TStringStream.Create;
      Result.Position := 0;
      TXML(lObject).SaveToStream(Result);
    end
    else
    begin
      Result := TMemoryStream.Create;
      TStream(lObject).Position := 0;
      Result.CopyFrom(TStream(lObject), 0);
    end;
  end;
end;

function TCustomColumnMapper.GetCurrentValue: Variant;
begin
  if (FRttiOptions.IsField) then
  begin
    Result := GetValueFromField;
  end
  else
  begin
    Result := GetValueFromProperty;
  end;
end;

function TCustomColumnMapper.GetEnumeratiorTValue: TValue;
begin
  Result := nil;
end;

function TCustomColumnMapper.GetIsExtension: Boolean;
begin
  Result := ColumnType = ctyExtension;
end;

function TCustomColumnMapper.GetIsNotNull: Boolean;
begin
  Result := Not IsNull;
end;

function TCustomColumnMapper.GetIsNull: Boolean;
var
  lColumnValue: Variant;
begin
  if (IsNullable) then
  begin
    lColumnValue := FRttiOptions.RttiFieldHelper.GetNullableVariantValue(FRttiOptions.RttiObject);
    Result := VarIsNull(lColumnValue)
  end
  else
  begin
    lColumnValue := FRttiOptions.RttiFieldHelper.GetVariantValue(FRttiOptions.RttiObject, ColumnType);
    Result := VarIsNull(TNullableCompare.NullIfNullable(lColumnValue));
  end;
end;

function TCustomColumnMapper.GetJoinedTValue: TValue;
begin
  Result := nil;
end;

function TCustomColumnMapper.GetValueFromField: Variant;
begin
  if (IsNullable) then
  begin
    Result := FRttiOptions.RttiFieldHelper.GetNullableVariantValue(FRttiOptions.RttiObject);
  end
  else
  begin
    Result := FRttiOptions.RttiFieldHelper.GetVariantValue(FRttiOptions.RttiObject, ColumnType);
    Result := DoGetCurrentValue(Result);
    Result := TNullableCompare.NullIfNullable(Result, Self);
  end;
end;

function TCustomColumnMapper.GetValueFromProperty: Variant;
begin
  if (IsNullable) then
  begin
    Result := FRttiOptions.RttiProperty.GetNullableVariantValue(FRttiOptions.RttiObject);
  end
  else
  begin
    Result := FRttiOptions.RttiProperty.GetVariantValue(FRttiOptions.RttiObject, ColumnType);
    Result := DoGetCurrentValue(Result);
   { if (ColumnType.IsSimpleType) then
    begin
      Result := TNullableCompare.NullIfNullable(Result, Self);
    end;}
  end;
end;

procedure TCustomColumnMapper.SetColumnValue(const pValue: Variant);
begin
  FColumnValue := pValue;
end;

end.
