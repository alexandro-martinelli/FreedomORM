unit AM.Freedom.Helper.RttiField;

interface

uses
  System.Rtti,
  System.Variants,
  System.Generics.Collections,
  AM.Freedom.EnumerationTypes;

type
  TRttiFieldHelper = class
  private
    FField: TRttiField;
  public
    constructor Create(pField: TRttiField);
    function GetObjectValue(pInstance: TObject): TObject;
    procedure SetObjectValue(pInstanceOwner, pInstanceToSet: TObject);
    function GetVariantValue(pInstance: TObject; pColumnType: TColumnType): Variant;
    function GetNullableVariantValue(pInstance: TObject): Variant;
    procedure SetNullableVariantValue(pInstance: TObject; pValue: Variant);
    procedure SetVariantValue(pInstance: TObject; pValue: Variant);
  end;

implementation

{ TRttiFieldHelper }

uses
  System.SysUtils,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.INullable;

constructor TRttiFieldHelper.Create(pField: TRttiField);
begin
  FField := pField;
end;

function TRttiFieldHelper.GetNullableVariantValue(pInstance: TObject): Variant;
var
  lObject: TObject;
  lINullable: INullable;
begin
  Result := Null;
  lObject := GetObjectValue(pInstance);
  if (Assigned(lObject)) then
  begin
    if lObject.GetInterface(INullable, lINullable) then
    begin
      Result := lINullable.GetInternalValue;
    end;
  end;
end;

function TRttiFieldHelper.GetObjectValue(pInstance: TObject): TObject;
begin
  Result := FField.GetValue(pInstance).AsObject;
end;

function TRttiFieldHelper.GetVariantValue(pInstance: TObject; pColumnType: TColumnType): Variant;
var
  lObject: TObject;
  lINullable: INullable;
begin
  Result := Null;
  if pColumnType.IsSimpleType or pColumnType.IsOrdinal then
  begin
    if (not FField.FieldType.IsInstance) then
    begin
      case pColumnType of
        ctyDate:
          Result := Trunc(FField.GetValue(pInstance).AsType<TDate>);
        ctyTime:
          Result := FField.GetValue(pInstance).AsType<TTime>;
        ctyDateTime:
          Result := FField.GetValue(pInstance).AsType<TDateTime>;
        ctyBoolean:
          Result := FField.GetValue(pInstance).AsType<Boolean>;
        ctyEnumerator:
          Result := FField.GetValue(pInstance).AsOrdinal;
        else
          Result := FField.GetValue(pInstance).AsVariant;
      end;
    end
    else
    begin
      lObject := FField.GetValue(pInstance).AsObject;
      if lObject.GetInterface(INullable, lINullable) then
      begin
        Result := lINullable.GetInternalValue;
      end;
    end;
  end;
end;

procedure TRttiFieldHelper.SetNullableVariantValue(pInstance: TObject; pValue: Variant);
var
  lObject: TObject;
  lINullable: INullable;
begin
  lObject := GetObjectValue(pInstance);
  if (Assigned(lObject)) then
  begin
    lObject.GetInterface(INullable, lINullable);
    lINullable.SetInternalValue(pValue);
  end;
end;

procedure TRttiFieldHelper.SetObjectValue(pInstanceOwner, pInstanceToSet: TObject);
begin
  FField.SetValue(pInstanceOwner, TValue.From<TObject>(pInstanceToSet));
end;

procedure TRttiFieldHelper.SetVariantValue(pInstance: TObject; pValue: Variant);
var
  lValue: TValue;
begin
  if (FField.FieldType.IsOrdinal) then
  begin
    lValue := TValue.FromOrdinal(FField.FieldType.Handle, pValue);
  end
  else
  begin
    lValue := TValue.FromVariant(pValue);
  end;
  FField.SetValue(pInstance, lValue);
end;

end.
