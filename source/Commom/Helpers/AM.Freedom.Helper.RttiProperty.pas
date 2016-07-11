unit AM.Freedom.Helper.RttiProperty;

interface

uses
  System.Rtti,
  System.Variants,
  AM.Freedom.EnumerationTypes;

type
  TRttiPropertyHelper = class helper for TRttiProperty
  public
    function GetObjectValue(pInstance: TObject): TObject;
    function GetVariantValue(pInstance: TObject; pColumnType: TColumnType): Variant;
    function GetNullableVariantValue(pInstance: TObject): Variant;
    procedure SetNullableVariantValue(pInstance: TObject; pValue: Variant);
  end;

implementation

{ TRttiPropertyHelper }

uses
  System.SysUtils,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.INullable;

function TRttiPropertyHelper.GetNullableVariantValue(pInstance: TObject): Variant;
var
  lObject: TObject;
  lINullable: INullable;
begin
  Result := Null;
  lObject := GetObjectValue(pInstance);
  if (Assigned(lObject)) then
  begin
    Supports(lObject, INullable, lINullable);
    Result := lINullable.GetPropertyValue;
  end;
end;

function TRttiPropertyHelper.GetObjectValue(pInstance: TObject): TObject;
begin
  Result := Self.GetValue(pInstance).AsObject;
end;

function TRttiPropertyHelper.GetVariantValue(pInstance: TObject; pColumnType: TColumnType): Variant;
begin
  Result := Null;
  if pColumnType.IsSimpleType or pColumnType.IsOrdinal then
  begin
    if (not Self.PropertyType.IsInstance) then
    begin
      case pColumnType of
        ctyDate:
          Result := Trunc(Self.GetValue(pInstance).AsType<TDate>);
        ctyTime:
          Result := Self.GetValue(pInstance).AsType<TTime>;
        ctyDateTime:
          Result := Self.GetValue(pInstance).AsType<TDateTime>;
        ctyEnumerator:
          Result := Self.GetValue(pInstance).AsOrdinal;
        ctyBoolean:
          Result := Self.GetValue(pInstance).AsBoolean;
        else
          Result := Self.GetValue(pInstance).AsVariant;
      end;
    end
    else
    begin
      Result := GetNullableVariantValue(pInstance);
    end;
  end;
end;

procedure TRttiPropertyHelper.SetNullableVariantValue(pInstance: TObject; pValue: Variant);
var
  lObject: TObject;
  lINullable: INullable;
begin
  lObject := Self.GetObjectValue(pInstance);
  if (Assigned(lObject)) then
  begin
    Supports(lObject, INullable, lINullable);
    lINullable.SetInternalValue(pValue);
  end;
end;

end.
