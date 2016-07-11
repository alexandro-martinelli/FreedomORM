unit AM.Freedom.NullableCompare;

interface

uses
  System.SysUtils,
  AM.Freedom.CustomNullable,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.ObjectMapper.ColumnMappers;

type
  TNullableCompare = class sealed
  strict private
    class var FNullableCompare: TCustomNullableCompare;
    class procedure CreateNullable;
  private
    class procedure DestroyNullable;
    class function NullIfBoolean(pValue: Variant; pColumn: TBooleanColumnMapper): Variant;
    class function NullIfEnumerator(pValue: Variant; pColumn: TEnumerationColumnMapper): Variant;
  public
    class function NullIfNullable(pValue: Variant): Variant; overload;
    class function NullIfNullable(pValue: Variant; pColumn: TCustomColumnMapper): Variant; overload;
  end;

implementation

uses
  AM.Freedom.DefaultsClassRegister,
  System.Variants,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Helper.Variant;

{ TNullableCompare }

class procedure TNullableCompare.CreateNullable;
begin
  if not Assigned(FNullableCompare) then
  begin
    FNullableCompare := TDefaultsClassRegister.DefaultNullableCompareClass.Create;
  end;
end;

class procedure TNullableCompare.DestroyNullable;
begin
  FreeAndNil(FNullableCompare);
end;

class function TNullableCompare.NullIfNullable(pValue: Variant; pColumn: TCustomColumnMapper): Variant;
begin
  if (not pColumn.IsNullable) then
  begin
    CreateNullable;
    Result := pValue;
    case pColumn.ColumnType of
      ctyByte, ctySmallint, ctyInteger, ctyInt64:
        if VarIsNull(pValue) or (pValue.TryToInt64 = FNullableCompare.IntegerNullable) then
        begin
          Result := Null;
        end;
      ctyMemo, ctyChar, ctyString:
        if VarIsNull(pValue) or (pValue.ToString = FNullableCompare.StringNullable) then
        begin
          Result := Null;
        end;
      ctySingle, ctyDouble, ctyCurrency, ctyExtended:
        if VarIsNull(pValue) or (pValue.TryToCurrency = FNullableCompare.FloatNullable) then
        begin
          Result := Null;
        end;
      ctyDate, ctyTime, ctyDateTime:
        if VarIsNull(pValue) or (pValue.TryToDateTime = FNullableCompare.DateTimeNullable) then
        begin
          Result := Null;
        end;
      ctyEnumerator:
        Result := NullIfEnumerator(pValue, TEnumerationColumnMapper(pColumn));
      ctyBoolean:
        Result := NullIfBoolean(pValue, TBooleanColumnMapper(pColumn));
    end;
  end
  else
  begin
    Result := pColumn.CurrentValue;
  end;
end;

class function TNullableCompare.NullIfEnumerator(pValue: Variant; pColumn: TEnumerationColumnMapper): Variant;
  function ArrayContainsCharText(pArray: TArray<Char>; pText: String): Boolean;
  var
    lChar: Char;
  begin
    Result := False;
    for lChar in pArray do
    begin
      Result := SameText(lChar, pText);
      if (Result) then
      begin
        Break;
      end;
    end;
  end;
begin
  case TEnumerationColumnMapper(pColumn).EnumType of
    emChar:
      if VarIsNull(pValue) or
         (not ArrayContainsCharText(TEnumerationColumnMapper(pColumn).EnumCharOf, pValue.ToString)) then
      begin
       Result := Null;
      end;
    emByte:
      if VarIsNull(pValue) or
         (pValue < pColumn.RttiOptions.RttiField.FieldType.AsOrdinal.MinValue) or
         (pValue > pColumn.RttiOptions.RttiField.FieldType.AsOrdinal.MaxValue) then
      begin
       Result := Null;
      end;
  end;
end;

class function TNullableCompare.NullIfBoolean(pValue: Variant; pColumn: TBooleanColumnMapper): Variant;
begin
  Result := pValue;
  if (Result <> pColumn.ValueTrue) and (Result <> pColumn.ValueFalse) then
  begin
    Result := Null;
  end;
end;

class function TNullableCompare.NullIfNullable(pValue: Variant): Variant;
begin
  CreateNullable;
  if (pValue = Unassigned) or (VarIsNull(pValue)) then
  begin
    Result := Null;
  end
  else
  begin
    Result := pValue;
    case VarType(pValue) of
      varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64, varUInt64:
        if (pValue = FNullableCompare.IntegerNullable) then
        begin
          Result := Null;
        end;
      varSingle, varDouble, varCurrency:
        if (pValue = FNullableCompare.FloatNullable) then
        begin
          Result := Null;
        end;
      varDate:
        if (pValue = FNullableCompare.DateTimeNullable) then
        begin
          Result := Null;
        end;
      varOleStr, varUString, varString:
        if (pValue = FNullableCompare.StringNullable) then
        begin
          Result := Null;
        end;
    end;
  end;
end;

initialization

finalization
  TNullableCompare.DestroyNullable;

end.
