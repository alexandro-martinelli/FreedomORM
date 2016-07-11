unit uObjectsTestsJSON;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  AM.Freedom.JSONAttributes;

type
  TEnumType = (etChar, etSingle, etMultiply, etDividir);
  TEnumTypes = Set of TEnumType;


  TSubObjectTestJSON = class
  private
    FPropSmallInt: SmallInt;
    FPropString: String;
    FPropInt: Integer;
    FPropByte: Byte;
  public
    property PropInt: Integer read FPropInt write FPropInt;
    property PropSmallInt: SmallInt read FPropSmallInt write FPropSmallInt;
    property PropByte: Byte read FPropByte write FPropByte;
    property PropString: String read FPropString write FPropString;
  end;

  TObjectTestJSON = class
  private
    FPropInt: Integer;
    FPropSmallInt: SmallInt;
    FPropByte: Byte;
    FPropString: String;
    FPropChar: Char;
    FPropExtended: Extended;
    FPropCurrency: Currency;
    FPropDouble: Double;
    FPropInt64: Int64;
    FPropVariant: Variant;
    FPropDate: TDate;
    FPropTime: TTime;
    FPropDateTime: TDateTime;
    FPropSingle: Single;
    FPropBoolean: Boolean;
    FPropEnum: TEnumType;
    FPropSet: TEnumTypes;
    FPropArray: TArray<String>;
    FPropObject: TSubObjectTestJSON;
    [JSONListOnlyItens]
    FPropList: TList<String>;
  public
    constructor Create;
    destructor Destroy; override;
    property PropInt: Integer read FPropInt write FPropInt;
    property PropSmallInt: SmallInt read FPropSmallInt write FPropSmallInt;
    property PropByte: Byte read FPropByte write FPropByte;
    property PropString: String read FPropString write FPropString;
    property PropChar: Char read FPropChar write FPropChar;
    property PropSingle: Single read FPropSingle write FPropSingle;
    property PropExtended: Extended read FPropExtended write FPropExtended;
    property PropCurrency: Currency read FPropCurrency write FPropCurrency;
    property PropDouble: Double read FPropDouble write FPropDouble;
    property PropInt64: Int64 read FPropInt64 write FPropInt64;
    property PropVariant: Variant read FPropVariant write FPropVariant;
    property PropDate: TDate read FPropDate write FPropDate;
    property PropTime: TTime read FPropTime write FPropTime;
    property PropDateTime: TDateTime read FPropDateTime write FPropDateTime;
    property PropBoolean: Boolean read FPropBoolean write FPropBoolean;
    property PropEnum: TEnumType read FPropEnum write FPropEnum;
    property PropSet: TEnumTypes read FPropSet write FPropSet;
    property PropArray: TArray<String> read FPropArray write FPropArray;
    property PropObject: TSubObjectTestJSON read FPropObject write FPropObject;
    property PropList: TList<String> read FPropList write FPropList;
  end;


implementation

{ TObjectTestJSON }

constructor TObjectTestJSON.Create;
begin
  FPropObject := TSubObjectTestJSON.Create;
  FPropList := TList<String>.Create;
end;

destructor TObjectTestJSON.Destroy;
begin
  FPropObject.Free;
  FPropList.Free;
  inherited;
end;

end.