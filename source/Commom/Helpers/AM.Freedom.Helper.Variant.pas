unit AM.Freedom.Helper.Variant;

interface

uses
  System.Classes,
  AM.Freedom.EnumerationTypes,
  System.Rtti;

type
  TVariantHelper = record Helper for Variant
  public
    function ToSingle: Single;
    function ToDouble: Double;
    function ToExtended: Extended;
    function ToCurrency: Currency;
    function ToInt64: Int64;
    function ToInt: Integer;
    function ToString: String;
    function ToDate: TDate;
    function ToTime: TTime;
    function ToDateTime: TDateTime;
    function ToStream: TStream;
    function ToBoolean: Boolean;
    function ToByte: Byte;

    function TryToSingle(pDefault: Single = 0.0): Single;
    function TryToDouble(pDefault: Double = 0.0): Double;
    function TryToCurrency(pDefault: Currency = 0.0): Currency;
    function TryToExtended(pDefault: Extended = 0.0): Extended;
    function TryToInt64(pDefault: Int64 = 0): Int64;
    function TryToInt(pDefault: Integer = 0): Integer;
    function TryToDate(pDefault: TDate = 0): TDate;
    function TryToTime(pDefault: TTime = 0): TTime;
    function TryToDateTime(pDefault: TDateTime = 0): TDateTime;
    function TryToBoolean(pDefault: Boolean = False): Boolean;
    function TryToStream(pDefault: TStream = nil): TStream;
    function TryToByte(pDefault: Byte = 0): Byte;
    function TryToColumnType(pColumnType: TColumnType): Variant;

    function VariantType: TColumnType;
    function IsNotNull: Boolean;
    function IsNull: Boolean;
    function NullIf(pValue: Variant): Variant;
    function AsTValue: TValue;
  end;

implementation

uses
  System.Variants,
  System.SysUtils,
  AM.Freedom.DefaultsClassRegister,
  AM.Freedom.CustomNullable;

{ TVariantHelper }

function TVariantHelper.AsTValue: TValue;
begin
  case VariantType of
    ctyByte, ctySmallint, ctyInteger: Result := TValue.From<Integer>(Self);
    ctyInt64, ctyEnumerator: Result := TValue.From<Int64>(Self);
    ctyChar, ctyString: Result := TValue.From<String>(Self);
    ctySingle, ctyDouble, ctyCurrency, ctyExtended: Result := TValue.From<Extended>(Self);
    ctyDate: Result := TValue.From<TDate>(Self);
    ctyTime: Result := TValue.From<TTime>(Self);
    ctyDateTime: Result := TValue.From<TDateTime>(Self);
    ctyBoolean: Result := TValue.From<Boolean>(Self);
  end;
end;

function TVariantHelper.IsNull: Boolean;
begin
  Result := VarIsNull(Self);
end;

function TVariantHelper.IsNotNull: Boolean;
begin
  Result := not Self.IsNull;
end;

function TVariantHelper.NullIf(pValue: Variant): Variant;
begin
  if (Self = pValue) or (Self = Unassigned) then
  begin
    Self := Null;
  end;
  Result := Self;
end;

function TVariantHelper.ToBoolean: Boolean;
begin
  Result := Self;
end;

function TVariantHelper.ToByte: Byte;
begin
  Result := Self;
end;

function TVariantHelper.ToCurrency: Currency;
begin
  Result := Self;
end;

function TVariantHelper.ToDate: TDate;
begin
  Result := VarToDateTime(Self);
end;

function TVariantHelper.ToDateTime: TDateTime;
begin
  Result := VarToDateTime(Self);
end;

function TVariantHelper.ToDouble: Double;
begin
  Result := Self;
end;

function TVariantHelper.ToExtended: Extended;
begin
  Result := Self;
end;

function TVariantHelper.ToInt: Integer;
begin
  Result := Self;
end;

function TVariantHelper.ToInt64: Int64;
begin
  Result := Self;
end;

function TVariantHelper.ToSingle: Single;
begin
  Result := Self;
end;

function TVariantHelper.ToStream: TStream;
var
  xData: PByteArray;
  xSize: Integer;
begin
  Result := TMemoryStream.Create;
  try
    xSize := VarArrayHighBound(Self, 1) - VarArrayLowBound(Self, 1) + 1;
    xData := VarArrayLock(Self);
    try
      Result.Position := 0;
      Result.WriteBuffer(xData^, xSize);
    finally
      VarArrayUnlock(Self);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TVariantHelper.ToString: String;
begin
  Result := VarToStr(Self);
end;

function TVariantHelper.ToTime: TTime;
begin
  Result := TTime(VarToDateTime(Self));
end;

function TVariantHelper.TryToBoolean(pDefault: Boolean): Boolean;
begin
  try
    Result := Self.ToBoolean;
  except
    Result := pDefault;
  end;
end;

function TVariantHelper.TryToByte(pDefault: Byte): Byte;
begin
  try
    Result := Self.ToByte;
  except
    Result := pDefault;
  end;
end;

function TVariantHelper.TryToColumnType(pColumnType: TColumnType): Variant;
begin
  case pColumnType of
    ctyByte: Result := TryToByte;
    ctySmallint, ctyInteger: Result := TryToInt;
    ctyInt64, ctyEnumerator: Result := TryToInt64;
    ctyChar, ctyString: Result := ToString;
    ctySingle: Result := TryToSingle;
    ctyDouble: Result := TryToDouble;
    ctyCurrency: Result := TryToCurrency;
    ctyExtended: Result := TryToExtended;
    ctyDate: Result := TryToDate;
    ctyTime: Result := TryToTime;
    ctyDateTime: Result := TryToDateTime;
    ctyBoolean: Result := TryToBoolean;
  end;
end;

function TVariantHelper.TryToCurrency(pDefault: Currency): Currency;
begin
  try
    Result := Self.ToCurrency;
  except
    Result := pDefault;
  end;
end;

function TVariantHelper.TryToDate(pDefault: TDate): TDate;
begin
  try
    Result := Self.ToDate;
  except
    Result := pDefault;
  end;
end;

function TVariantHelper.TryToDateTime(pDefault: TDateTime): TDateTime;
begin
  try
    Result := Self.ToDateTime;
  except
    Result := pDefault;
  end;
end;

function TVariantHelper.TryToDouble(pDefault: Double): Double;
begin
  try
    Result := Self.ToDouble;
  except
    Result := pDefault;
  end;
end;

function TVariantHelper.TryToExtended(pDefault: Extended): Extended;
begin
  try
    Result := Self.ToExtended;
  except
    Result := pDefault;
  end;
end;

function TVariantHelper.TryToInt(pDefault: Integer): Integer;
begin
  try
    Result := Self.ToInt;
  except
    Result := pDefault;
  end;
end;

function TVariantHelper.TryToInt64(pDefault: Int64): Int64;
begin
  try
    Result := Self.ToInt64;
  except
    Result := pDefault;
  end;
end;

function TVariantHelper.TryToSingle(pDefault: Single): Single;
begin
  try
    Result := Self.ToSingle;
  except
    Result := pDefault;
  end;

end;

function TVariantHelper.TryToStream(pDefault: TStream): TStream;
begin
  try
    Result := Self.ToStream;
  except
    Result := pDefault;
  end;
end;

function TVariantHelper.TryToTime(pDefault: TTime): TTime;
begin
  try
    Result := Self.ToTime;
  except
    Result := pDefault;
  end;
end;

function TVariantHelper.VariantType: TColumnType;
var
  lVarType: TVarType;
const
  varArrayVariant = 8204;
  varSQLDateTime = 271;
begin
  Result   := ctyUnknow;
  lVarType := VarType(Self);
  case lVarType of
    varWord, varSmallInt, varShortInt, varByte:
      Result := ctySmallInt;
    varLongWord, varInteger:
      Result := ctyInteger;
    varSingle:
      Result := ctySingle;
    varDouble:
      Result := ctyDouble;
    varCurrency:
      Result := ctyCurrency;
    varDate:
      begin
        if (Frac(TVarData(Self).VDate) > 0) and (Trunc(TVarData(Self).VDate) > 0) then
        begin
          Result := ctyDateTime;
        end
        else if (Frac(TVarData(Self).VDate) > 0) and (Trunc(TVarData(Self).VDate) = 0) then
        begin
          Result := ctyTime;
        end
        else
        begin
          Result := ctyDate;
        end;
      end;
    varSQLDateTime:
      Result := ctyDateTime;
    varInt64, varUInt64:
      Result := ctyInt64;
    varOleStr, varString, varUString:
      Result := ctyString;
    varArray, varArrayVariant:
      Result := Self[VarArrayLowBound(Self, 1)].VariantType;
  end;
end;

end.
