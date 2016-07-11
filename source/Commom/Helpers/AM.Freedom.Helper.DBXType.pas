unit AM.Freedom.Helper.DBXType;

interface

uses
  Data.DBXCommon,
  AM.Freedom.EnumerationTypes;

type
  TDBXTypeHelper = record helper for TDBXType
  public
    function ToColumnType: TColumnType;
  end;

implementation

{ TDBXTypeHelper }

function TDBXTypeHelper.ToColumnType: TColumnType;
begin
  case Self of
    TDBXDataTypes.AnsiStringType, TDBXDataTypes.WideStringType:
      Result := ctyString;
    TDBXDataTypes.DateType:
      Result := ctyDate;
    TDBXDataTypes.BlobType:
      Result := ctyBlob;
    TDBXDataTypes.BooleanType:
      Result := ctyBoolean;
    TDBXDataTypes.UInt8Type, TDBXDataTypes.VarBytesType, TDBXDataTypes.BytesType:
      Result :=  ctyByte;
    TDBXDataTypes.Int8Type, TDBXDataTypes.Int16Type, TDBXDataTypes.UINT16Type:
      Result := ctySmallint;
    TDBXDataTypes.Int32Type, TDBXDataTypes.Uint32Type:
      Result := ctyInteger;
    TDBXDataTypes.SingleType:
      Result := ctySingle;
    TDBXDataTypes.DoubleType:
      Result := ctyDouble;
    TDBXDataTypes.BcdType:
      Result := ctyExtended;
    TDBXDataTypes.TimeType:
      Result := ctyTime;
    TDBXDataTypes.DateTimeType, TDBXDataTypes.TimeStampOffsetType:
      Result := ctyDateTime;
    TDBXDataTypes.Int64Type, TDBXDataTypes.Uint64Type:
      Result := ctyInt64;
    TDBXDataTypes.TimeStampType:
      Result := ctyDateTime;
    TDBXDataTypes.CurrencyType:
      Result := ctyCurrency;
  else
    Result := ctyUnknow;
  end;
end;

end.
