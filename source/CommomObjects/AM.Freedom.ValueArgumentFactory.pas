unit AM.Freedom.ValueArgumentFactory;

interface

uses
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.EnumerationTypes;

type
  TValueArgumentFactory = class sealed
    class function CreateAsColumnType(pColumnType: TColumnType; pValue: Variant): TValueArgument;
  end;

implementation

{ TValueArgumentFactory }

class function TValueArgumentFactory.CreateAsColumnType(pColumnType: TColumnType; pValue: Variant): TValueArgument;
begin
  Result := nil;
  case pColumnType of
    ctyByte: Result := TValueArgument.CreateAsByte(pValue);
    ctySmallint: Result := TValueArgument.CreateAsSmallint(pValue);
    ctyInteger: Result := TValueArgument.CreateAsInteger(pValue);
    ctyInt64: Result := TValueArgument.CreateAsInt64(pValue);
    ctyChar, ctyString: Result := TValueArgument.CreateAsString(pValue);
    ctySingle: Result := TValueArgument.CreateAsSingle(pValue);
    ctyDouble: Result := TValueArgument.CreateAsDouble(pValue);
    ctyCurrency: Result := TValueArgument.CreateAsCurrency(pValue);
    ctyExtended: Result := TValueArgument.CreateAsExtended(pValue);
    ctyDate: Result := TValueArgument.CreateAsDate(pValue);
    ctyTime: Result := TValueArgument.CreateAsTime(pValue);
    ctyDateTime: Result := TValueArgument.CreateAsDateTime(pValue);
  end;
end;

end.
