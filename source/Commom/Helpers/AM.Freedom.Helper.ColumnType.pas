unit AM.Freedom.Helper.ColumnType;

interface

uses
  Data.DB,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.CustomFieldCommand;

type
  TGetFieldCommandClass = function(pColumnType: TColumnType): TFieldCommandClass of object;

  TColumnTypeHelper = record Helper for TColumnType
  public
    function Sizeable: Boolean;
    function Scalable: Boolean;
    function IsSimpleType: Boolean;
    function CanBeAutoMapped: Boolean;
    function IsDateTime: Boolean;
    function IsObject: Boolean;
    function IsOrdinal: Boolean;
    function IsBoolean: Boolean;
    function ToString: String;
    function IsEquals(pColumnType: TColumnType): Boolean;
    function IsBlob: Boolean;
    function ToFieldType: TFieldType;
    function IsFloat: Boolean;
  end;

  TColumnTypeMethods = class sealed
  public
    class function ColumnTypeFromString(pString: String): TColumnType;
  end;

implementation

{ TColumnTypeHelper }

uses
  System.SysUtils,
  AM.Freedom.SQLCommands.Fields;

function TColumnTypeHelper.Scalable: Boolean;
begin
  Result := (Self in [ctySingle, ctyDouble, ctyExtended, ctyCurrency]);
end;

function TColumnTypeHelper.CanBeAutoMapped: Boolean;
begin
  Result := Self.IsSimpleType or (Self in [ctyBoolean, ctyEnumerator]);
end;

function TColumnTypeHelper.IsBlob: Boolean;
begin
  Result := Self in [ctyBlob, ctyMemo, ctyXML];
end;

function TColumnTypeHelper.IsBoolean: Boolean;
begin
  Result := Self = ctyBoolean;
end;

function TColumnTypeHelper.IsDateTime: Boolean;
begin
  Result := Self in [ctyDate, ctyDateTime, ctyTime];
end;

function TColumnTypeHelper.IsEquals(pColumnType: TColumnType): Boolean;
begin
  Result := Self = pColumnType;
end;

function TColumnTypeHelper.IsFloat: Boolean;
begin
  Result := Self.Scalable;
end;

function TColumnTypeHelper.IsObject: Boolean;
begin
  Result := Self in [ctyBlob, ctyMemo, ctyDetail];
end;

function TColumnTypeHelper.IsOrdinal: Boolean;
begin
  Result := Self in [ctyBoolean, ctyEnumerator];
end;

function TColumnTypeHelper.IsSimpleType: Boolean;
begin
  Result := Self in [ctyByte, ctySmallint, ctyInteger, ctyInt64, ctyChar, ctyString, ctySingle, ctyDouble, ctyExtended,
      ctyCurrency, ctyDate, ctyTime, ctyDateTime];
end;

function TColumnTypeHelper.Sizeable: Boolean;
begin
  Result := Self.Scalable or (Self in [ctyChar, ctyString, ctyBlob, ctyMemo]);
end;

function TColumnTypeHelper.ToFieldType: TFieldType;
begin
  Result := ftUnknown;
  case Self of
    ctyByte:
      Result := ftByte;
    ctySmallint:
      Result := ftSmallint;
    ctyEnumerator, ctyInteger:
      Result := Data.DB.ftInteger;
    ctyInt64:
      Result := ftLargeint;
    ctyString:
      Result := Data.DB.ftString;
    ctyBlob:
      Result := ftBlob;
    ctyMemo, ctyXML:
      Result := ftWideMemo;
    ctyDate:
      Result := Data.DB.ftDate;
    ctyTime:
      Result := Data.DB.ftTime;
    ctyDateTime:
      Result := Data.DB.ftDateTime;
    ctySingle, ctyDouble, ctyCurrency:
      Result := ftFloat;
    ctyBoolean:
      Result := Data.DB.ftBoolean;
  end;
end;

function TColumnTypeHelper.ToString: String;
begin
  Result := 'Unknow';
  case Self of
    ctySmallint:
      Result := 'Smallint';
    ctyInteger:
      Result := 'Integer';
    ctyInt64:
      Result := 'Int64';
    ctyChar:
      Result := 'Char';
    ctyString:
      Result := 'String';
    ctySingle:
      Result := 'Single';
    ctyDouble:
      Result := 'Double';
    ctyCurrency:
      Result := 'Currency';
    ctyExtended:
      Result := 'Extended';
    ctyDate:
      Result := 'TDate';
    ctyTime:
      Result := 'TTime';
    ctyDateTime:
      Result := 'TDateTime';
    ctyBoolean:
      Result := 'Boolean';
    ctyEnumerator:
      Result := 'Enumerator';
    ctyDetail:
      Result := 'Detail';
    ctyJoin:
      Result := 'Join';
    ctyBlob:
      Result := 'Blob';
    ctyMemo:
      Result := 'Memo';
    ctyXML:
      Result := 'XML';
    ctyByte:
      Result := 'Byte';
    ctyExtension:
      Result := 'Extension';
  end;
end;

{ TColumnTypeMethods }

class function TColumnTypeMethods.ColumnTypeFromString(pString: String): TColumnType;
var
  lIndex: Integer;
begin
  Result := ctyUnknow;
  for lIndex := Ord(Low(TColumnType)) to Ord(High(TColumnType)) do
  begin
    if SameText(TColumnType(lIndex).ToString, pString) then
    begin
      Result := TColumnType(lIndex);
      Break;
    end;
  end;
end;

end.
