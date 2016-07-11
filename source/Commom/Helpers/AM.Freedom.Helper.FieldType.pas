unit AM.Freedom.Helper.FieldType;

interface

uses
  Data.DB,
  AM.Freedom.EnumerationTypes;

type
  TFieldTypeHelper = record helper for TFieldType
  public
    function ToColumnType: TColumnType;
  end;

implementation

{ TFieldTypeHelper }

function TFieldTypeHelper.ToColumnType: TColumnType;
begin
  Result := ctyUnknow;
  case Self of
    ftExtended: Result := ctyExtended;
    ftString, ftGuid, ftWideString: Result := ctyString;
    ftWord, ftSmallint, ftLongWord: Result := ctySmallint;
    ftInteger, ftAutoInc: Result := ctyInteger;
    ftBoolean: Result := ctyBoolean;
    ftFloat: Result := ctyDouble;
    ftBCD, ftCurrency, ftFMTBcd: Result := ctyCurrency;
    ftDate: Result := ctyDate;
    ftTime: Result := ctyTime;
    ftFixedChar, ftFixedWideChar: Result := ctyChar;
    ftLargeint: Result := ctyInt64;
    ftDateTime, ftTimeStamp, ftOraTimeStamp: Result := ctyDateTime;
    ftByte, ftShortint, ftBytes, ftVarBytes: Result := ctyByte;
    ftSingle: Result := ctySingle;
    ftStream, ftOraBlob, ftOraClob, ftBlob, ftGraphic: Result := ctyBlob;
    ftWideMemo, ftMemo, ftFmtMemo: Result := ctyMemo;
  end;
end;

end.
