unit AM.UnitReader.Helper.MemberType;

interface

uses
  AM.UnitReader.Enumerations;

type
  TMemberTypeHelper = record helper for TMemberType
  public
    function ToString: String;
  end;

  TMemberTypeFuncs = class
  public
    class function MemberTypeFromString(pString: String): TMemberType;
  end;

implementation


uses
  System.SysUtils;

{ TMemberTypeHelper }

function TMemberTypeHelper.ToString: String;
begin
  case Self of
    mtUnknow: Result := 'UNKNOW';
    mtByte: Result := 'BYTE';
    mtSmallInt: Result := 'SMALLINT';
    mtInteger: Result := 'INTEGER';
    mtInt64: Result := 'INT64';
    mtShortInt: Result := 'SHORTINT';
    mtWord: Result := 'WORD';
    mtCardinal: Result := 'CARDINAL';
    mtUInt8: Result := 'UINT8';
    mtInt8: Result := 'INT8';
    mtUInt16: Result := 'UINT16';
    mtInt16: Result := 'INT16';
    mtUInt32: Result := 'UINT32';
    mtInt32: Result := 'INT32';
    mtUInt64: Result := 'UINT64';
    mtSingle: Result := 'SINGLE';
    mtReal: Result := 'REAL';
    mtDouble: Result := 'DOUBLE';
    mtExtended: Result := 'EXTENDED';
    mtCurrency: Result := 'CURRENCY';
    mtBoolean: Result := 'BOOLEAN';
    mtDate: Result := 'TDATE';
    mtTime: Result := 'TTIME';
    mtDateTime: Result := 'TDATETIME';
    mtTStrings: Result := 'TSTRINGS';
    mtString: Result := 'STRING';
    mtChar: Result := 'CHAR';
  end;
end;

{ TMemberTypeFuncs }

class function TMemberTypeFuncs.MemberTypeFromString(pString: String): TMemberType;
var
  lCounter: Integer;
begin
  Result := mtUnknow;
  for lCounter := Ord(Low(TMemberType)) to Ord(High(TMemberType)) do
  begin
    if SameText(TMemberType(lCounter).ToString, pString) then
    begin
      Result := TMemberType(lCounter);
      Break;
    end;
  end;
end;

end.
