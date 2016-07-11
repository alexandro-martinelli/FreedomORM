unit AM.Freedom.JSONAttributes;

interface

uses
  System.SysUtils,
  System.Rtti,
  AM.Freedom.JSONObjects;

type
  JSONAttribute = class(TCustomAttribute);

  JSONUnparsed = class(JSONAttribute);

  JSONMinBaseClassName = class(JSONAttribute)
  private
    FMinClassName: String;
    FMinClassType: TClass;
  public
    constructor Create(pClassName: String); overload;
    constructor Create(pClassType: TClass); overload;
    property MinClassName: String read FMinClassName;
    property MinClassType: TClass read FMinClassType;
  end;

  JSONInternalValue = class(JSONAttribute);

  JSONNull = class(JSONInternalValue);

  JSONCustomValue = class(JSONInternalValue)
  private
    FValue: Variant;
  public
    constructor Create(pJSONValue: Integer); overload;
    constructor Create(pJSONValue: String); overload;
    constructor Create(pJSONValue: TDate); overload;
    constructor Create(pJSONValue: TTime); overload;
    constructor Create(pJSONValue: TDateTime); overload;
    constructor Create(pJSONValue: Extended); overload;
    constructor Create(pJSONValue: Boolean); overload;
    function Value: TJSONValue;
  end;

  TJSONReflectorClass = class of TCustomJSONReflector;

  TCustomJSONReflector = class
  public
    function Convert(pObject: TObject; pField: TRttiField): TJSONValue; virtual; abstract;
    procedure Revert(pObject: TObject; pField: TRttiField; pConvertedValue: String); virtual; abstract;
  end;

  TReflectorParseType = (rptConvert, rptRevert);
  TReflectorParseTypes = set of TReflectorParseType;

const
  ReflectorParseAll = [rptConvert, rptRevert];

type
  JSONReflector = class(JSONAttribute)
  private
    FReflectorClass: TJSONReflectorClass;
    FReflectorParseTypes: TReflectorParseTypes;
  public
    constructor Create(pReflectorClass: TJSONReflectorClass;
        pReflectorParseTypes: TReflectorParseTypes = ReflectorParseAll);
    property ReflectorClass: TJSONReflectorClass read FReflectorClass;
    property ReflectorParseTypes: TReflectorParseTypes read FReflectorParseTypes;
  end;

  JSONListOnlyItens = class(JSONAttribute);

implementation

uses
  AM.Freedom.JSONValueFactory;

{ ParseJSONValue }

constructor JSONCustomValue.Create(pJSONValue: Extended);
begin
  FValue := pJSONValue;
end;

constructor JSONCustomValue.Create(pJSONValue: TDateTime);
begin
  FValue := pJSONValue;
end;

constructor JSONCustomValue.Create(pJSONValue: TTime);
begin
  FValue := pJSONValue;
end;

constructor JSONCustomValue.Create(pJSONValue: TDate);
begin
  FValue := pJSONValue;
end;

constructor JSONCustomValue.Create(pJSONValue: String);
begin
  FValue := pJSONValue;
end;

constructor JSONCustomValue.Create(pJSONValue: Integer);
begin
  FValue := pJSONValue;
end;

constructor JSONCustomValue.Create(pJSONValue: Boolean);
begin
  FValue := pJSONValue;
end;

function JSONCustomValue.Value: TJSONValue;
begin
  Result := TJSONValueFactory.CreateFromVariant(FValue);
end;

{ ParseJSONReflector }

constructor JSONReflector.Create(pReflectorClass: TJSONReflectorClass; pReflectorParseTypes: TReflectorParseTypes);
begin
  FReflectorClass := pReflectorClass;
  FReflectorParseTypes := pReflectorParseTypes;
end;

{ ParseJSONMinBaseClassName }

constructor JSONMinBaseClassName.Create(pClassType: TClass);
begin
  Create(pClassType.ClassName);
  FMinClassType := pClassType
end;

constructor JSONMinBaseClassName.Create(pClassName: String);
begin
  FMinClassName := pClassName;
end;

end.
