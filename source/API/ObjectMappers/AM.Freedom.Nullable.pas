unit AM.Freedom.Nullable;

interface

uses
  System.Rtti,
  AM.Freedom.INullable,
  AM.Freedom.InterfacedObjects,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.BooleanValueOptions,
  AM.Freedom.JSONFreedomObject,
  Data.DBXJSONReflect;

type
  TNullable<T> = class(TJSONFreedomObject, INullable)
  strict private
    FValue: Variant;
    FBooleanValueOptions: TBooleanValueOptions;
    procedure SetValue(const pValue: T);
    function GetIsNull: Boolean;
    function ExtractTValueFromFieldValue: TValue;
    function GetValue: T;
    function GetHasValue: Boolean;
    function GetInternalValue: Variant;
    procedure SetInternalValue(pValue: Variant);
    function ConvertBooleanToValue(pBoolean: Boolean): Variant;
    function ConvertValueToBoolean(pValue: Variant): Boolean;
    function GetBooleanValueOptions: TBooleanValueOptions;
    function GetPropertyValue: Variant;
  strict protected
    property BooleanValueOptions: TBooleanValueOptions read GetBooleanValueOptions;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    property HasValue: Boolean read GetHasValue;
    property Value: T read GetValue write SetValue;
    property IsNull: Boolean read GetIsNull;
    property InternalValue: Variant read GetInternalValue write SetInternalValue;
  end;

implementation

uses
  System.Variants,
  AM.Freedom.Helper.Variant;

{ TNullable<T> }

procedure TNullable<T>.Clear;
begin
  FValue := Null;
end;

function TNullable<T>.ConvertBooleanToValue(pBoolean: Boolean): Variant;
begin
  case pBoolean of
    True: Result := FBooleanValueOptions.ValueTrue;
    else
      Result := FBooleanValueOptions.ValueFalse;
  end;
end;

function TNullable<T>.ConvertValueToBoolean(pValue: Variant): Boolean;
begin
  Result := (pValue = FBooleanValueOptions.ValueTrue);
end;

constructor TNullable<T>.Create;
begin
  Clear;
  FBooleanValueOptions := TBooleanValueOptions.Create;
end;

destructor TNullable<T>.Destroy;
begin
  FBooleanValueOptions.Free;
  inherited;
end;

function TNullable<T>.ExtractTValueFromFieldValue: TValue;
var
  lBoolean: Boolean;
begin
  if (TypeInfo(Integer) = TypeInfo(T)) then
  begin
    Result := TValue.From<Integer>(FValue.TryToInt);
  end
  else if TypeInfo(Byte) = TypeInfo(T) then
  begin
    Result := TValue.From<Byte>(FValue.TryToInt)
  end
  else if TypeInfo(Smallint) = TypeInfo(T) then
  begin
    Result := TValue.From<Smallint>(FValue.TryToInt);
  end
  else if TypeInfo(Integer) = TypeInfo(T) then
  begin
    Result := TValue.From<Integer>(FValue.TryToInt);
  end
  else if TypeInfo(Int64) = TypeInfo(T) then
  begin
    Result := TValue.From<Int64>(FValue.TryToInt64);
  end
  else if TypeInfo(Char) = TypeInfo(T) then
  begin
    Result := TValue.From<String>(FValue.ToString);
  end
  else if TypeInfo(String) = TypeInfo(T) then
  begin
    Result := TValue.From<String>(FValue.ToString);
  end
  else if TypeInfo(Single) = TypeInfo(T) then
  begin
    Result := TValue.From<Single>(FValue.TryToSingle);
  end
  else if TypeInfo(Double) = TypeInfo(T) then
  begin
    Result := TValue.From<Double>(FValue.TryToDouble);
  end
  else if TypeInfo(Currency) = TypeInfo(T) then
  begin
    Result := TValue.From<Currency>(FValue.TryToCurrency);
  end
  else if TypeInfo(Extended) = TypeInfo(T) then
  begin
    Result := TValue.From<Extended>(FValue.TryToExtended);
  end
  else if TypeInfo(TDate) = TypeInfo(T) then
  begin
    Result := TValue.From<TDate>(FValue.TryToDate);
  end
  else if TypeInfo(TTime) = TypeInfo(T) then
  begin
    Result := TValue.From<TTime>(FValue.TryToTime);
  end
  else if TypeInfo(TDateTime) = TypeInfo(T) then
  begin
    Result := TValue.From<TDateTime>(FValue.TryToDateTime);
  end
  else if TypeInfo(Boolean) = TypeInfo(T) then
  begin
    lBoolean := ConvertValueToBoolean(FValue);
    Result := TValue.From<Boolean>(lBoolean);
  end;
end;

function TNullable<T>.GetBooleanValueOptions: TBooleanValueOptions;
begin
  Result := FBooleanValueOptions;
end;

function TNullable<T>.GetHasValue: Boolean;
begin
  Result := not VarIsNull(FValue);
end;

function TNullable<T>.GetIsNull: Boolean;
begin
  Result := VarIsNull(FValue);
end;

function TNullable<T>.GetPropertyValue: Variant;
var
  lValue: TValue;
begin
  lValue := ExtractTValueFromFieldValue;
  Result := lValue.AsVariant;
end;

function TNullable<T>.GetValue: T;
var
  lValue: TValue;
begin
  lValue := ExtractTValueFromFieldValue;
  Result := lValue.AsType<T>;
end;

function TNullable<T>.GetInternalValue: Variant;
begin
  Result := FValue;
end;

procedure TNullable<T>.SetInternalValue(pValue: Variant);
begin
  FValue := pValue;
end;

procedure TNullable<T>.SetValue(const pValue: T);
var
  lValue: TValue;
begin
  lValue := TValue.From<T>(pValue);
  FValue := lValue.AsVariant;
  if TypeInfo(Boolean) = TypeInfo(T) then
  begin
    FValue := ConvertBooleanToValue(FValue);
  end
end;

end.
