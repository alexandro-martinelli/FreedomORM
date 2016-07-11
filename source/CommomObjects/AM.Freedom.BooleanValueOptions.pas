unit AM.Freedom.BooleanValueOptions;

interface

uses
  System.SysUtils,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.JSONFreedomObject;

type
  TBooleanValueOptions = class sealed(TJSONFreedomObject)
  private
    FValueTrue: Variant;
    FValueFalse: Variant;
    FValueType: TColumnType;
  public
    property ValueTrue: Variant read FValueTrue write FValueTrue;
    property ValueFalse: Variant read FValueFalse write FValueFalse;
    property ValueType: TColumnType read FValueType write FValueType;
  end;

implementation

end.
