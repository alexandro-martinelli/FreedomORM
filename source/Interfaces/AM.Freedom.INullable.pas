unit AM.Freedom.INullable;

interface

uses
  AM.Freedom.BooleanValueOptions;

type
  INullable = interface
  ['{1D8C03FF-86F8-457A-A69F-EBB96B4C8BA9}']
    function GetIsNull: Boolean;
    function GetHasValue: Boolean;
    function GetInternalValue: Variant;
    procedure SetInternalValue(pValue: Variant);
    function ConvertBooleanToValue(pBoolean: Boolean): Variant;
    function ConvertValueToBoolean(pValue: Variant): Boolean;
    function GetBooleanValueOptions: TBooleanValueOptions;
    function GetPropertyValue: Variant;


    procedure Clear;
    property HasValue: Boolean read GetHasValue;
    property IsNull: Boolean read GetIsNull;
    property BooleanValueOptions: TBooleanValueOptions read GetBooleanValueOptions;
  end;

implementation

end.
