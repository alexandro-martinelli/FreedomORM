unit AM.Freedom.Helper.DBXReader;

interface

uses
  System.Classes,
  Data.DBXCommon,
  Data.DB;

type
  TDBXReaderHelper = class helper for TDBXReader
  public
    function FieldValue(pCampo: string): Variant;
    function FieldBlobValue(pCampo: string): TStream;
  end;

implementation

uses
  System.Variants,
  System.SysUtils;

{ TDBXReaderHelper }

function TDBXReaderHelper.FieldBlobValue(pCampo: string): TStream;
var
  lDBXValue: TDBXValue;
  lStrings: TStrings;
  procedure CopyStreamToResult(pStream: TStream);
  begin
    Result := TMemoryStream.Create;
    pStream.Position := 0;
    Result.CopyFrom(pStream, 0);
    Result.Position := 0;
    pStream.Free;
  end;
begin
  Result := nil;
  lDBXValue := Self.Value[pCampo];
  if not lDBXValue.IsNull then
  begin
    case lDBXValue.ValueType.DataType of
      TDBXDataTypes.BlobType:
          case lDBXValue.ValueType.SubType of
            TDBXDataTypes.HMemoSubType, TDBXDataTypes.MemoSubType, TDBXDataTypes.WideMemoSubType:
              begin
                lStrings := TStringList.Create;
                lStrings.Text := lDBXValue.AsString;
                Result := TMemoryStream.Create;
                lStrings.SaveToStream(Result);
                Result.Position := 0;
                lStrings.Free;
              end;
            else
              CopyStreamToResult(TDBXStreamValue(lDBXValue).AsStream);
          end;
      TDBXDataTypes.BinaryBlobType:
        CopyStreamToResult(TDBXStreamValue(lDBXValue).AsStream);

    end;
  end;
  if not Assigned(Result) then
  begin
    Result := TMemoryStream.Create;
  end;
end;

function TDBXReaderHelper.FieldValue(pCampo: string): Variant;
var
  lDBXValue: TDBXValue;
begin
  lDBXValue := Self.Value[pCampo];
  case lDBXValue.ValueType.DataType of
    TDBXDataTypes.UInt8Type, TDBXDataTypes.VarBytesType, TDBXDataTypes.BytesType:
      Result := lDBXValue.AsUInt8;
    TDBXDataTypes.Int8Type:
      Result := lDBXValue.AsInt8;
    TDBXDataTypes.Int16Type:
      Result := lDBXValue.AsInt16;
    TDBXDataTypes.UInt16Type:
      Result := lDBXValue.AsUInt16;
    TDBXDataTypes.Int32Type, TDBXDataTypes.UInt32Type:
      Result := lDBXValue.AsInt32;
    TDBXDataTypes.Int64Type, TDBXDataTypes.UInt64Type:
      Result := lDBXValue.AsInt64;
    TDBXDataTypes.WideStringType, TDBXDataTypes.AnsiStringType:
      Result := lDBXValue.AsString;
    TDBXDataTypes.DateType, TDBXDataTypes.TimeType, TDBXDataTypes.TimeStampType, TDBXDataTypes.TimeStampOffsetType:
      if not lDBXValue.IsNull then
      begin
        Result := lDBXValue.AsDateTime
      end
      else
      begin
        Result := 0;
      end;
    TDBXDataTypes.CurrencyType, TDBXDataTypes.DoubleType, TDBXDataTypes.BcdType:
      Result := lDBXValue.AsDouble;
    TDBXDataTypes.BooleanType:
      Result := lDBXValue.AsBoolean;
    TDBXDataTypes.SingleType:
      Result := lDBXValue.AsSingle;
  else
    Result := null;
  end;
end;

end.
