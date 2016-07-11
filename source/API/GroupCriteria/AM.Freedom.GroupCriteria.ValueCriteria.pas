unit AM.Freedom.GroupCriteria.ValueCriteria;

interface

uses
  AM.Freedom.Helper.Variant,
  AM.Freedom.EnumerationTypes;

type
  TValueCriteria = class sealed
  strict private
    FValue: Variant;
    FColumnType: TColumnType;
    function GetAsCurrency: Currency;
    function GetAsDate: TDate;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsExtended: Extended;
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    function GetAsSmallint: Smallint;
    function GetAsString: String;
    function GetAsTime: TTime;
    constructor Create(pValue: Variant; pColumnType: TColumnType);
  public
    class function CreateAsString(pValue: String): TValueCriteria;
    class function CreateAsSmallint(pValue: Smallint): TValueCriteria;
    class function CreateAsInteger(pValue: Integer): TValueCriteria;
    class function CreateAsInt64(pValue: Int64): TValueCriteria;
    class function CreateAsDouble(pValue: Double): TValueCriteria;
    class function CreateAsExtended(pValue: Extended): TValueCriteria;
    class function CreateAsCurrency(pValue: Currency): TValueCriteria;
    class function CreateAsDate(pValue: TDate): TValueCriteria;
    class function CreateAsTime(pValue: TTime): TValueCriteria;
    class function CreateAsDateTime(pValue: TDateTime): TValueCriteria;

    property ColumnType: TColumnType read FColumnType;
    property AsString: String read GetAsString;
    property AsSmallint: Smallint read GetAsSmallint;
    property AsInteger: Integer read GetAsInteger;
    property AsInt64: Int64 read GetAsInt64;
    property AsDouble: Double read GetAsDouble;
    property AsExtended: Extended read GetAsExtended;
    property AsCurrency: Currency read GetAsCurrency;
    property AsDate: TDate read GetAsDate;
    property AsTime: TTime read GetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime;
    property Value: Variant read FValue;
  end;

implementation

{ TValueCriteria }

constructor TValueCriteria.Create(pValue: Variant; pColumnType: TColumnType);
begin
  FValue := pValue;
  FColumnType := pColumnType;
end;

class function TValueCriteria.CreateAsCurrency(pValue: Currency): TValueCriteria;
begin
  Result := Self.Create(pValue, ctyCurrency);
end;

class function TValueCriteria.CreateAsDate(pValue: TDate): TValueCriteria;
begin
  Result := Self.Create(pValue, ctyDate);
end;

class function TValueCriteria.CreateAsDateTime(pValue: TDateTime): TValueCriteria;
begin
  Result := Self.Create(pValue, ctyDateTime);
end;

class function TValueCriteria.CreateAsDouble(pValue: Double): TValueCriteria;
begin
  Result := Self.Create(pValue, ctyDouble);
end;

class function TValueCriteria.CreateAsExtended(pValue: Extended): TValueCriteria;
begin
  Result := Self.Create(pValue, ctyExtended);
end;

class function TValueCriteria.CreateAsInt64(pValue: Int64): TValueCriteria;
begin
  Result := Self.Create(pValue, ctyInt64);
end;

class function TValueCriteria.CreateAsInteger(pValue: Integer): TValueCriteria;
begin
  Result := Self.Create(pValue, ctyInteger);
end;

class function TValueCriteria.CreateAsSmallint(pValue: Smallint): TValueCriteria;
begin
  Result := Self.Create(pValue, ctySmallint);
end;

class function TValueCriteria.CreateAsString(pValue: String): TValueCriteria;
begin
  Result := Self.Create(pValue, ctyString);
end;

class function TValueCriteria.CreateAsTime(pValue: TTime): TValueCriteria;
begin
  Result := Self.Create(pValue, ctyTime);
end;

function TValueCriteria.GetAsCurrency: Currency;
begin
  Result := FValue.TryToCurrency;
end;

function TValueCriteria.GetAsDate: TDate;
begin
  Result := FValue.TryToDate;
end;

function TValueCriteria.GetAsDateTime: TDateTime;
begin
  Result := FValue.TryToDateTime;
end;

function TValueCriteria.GetAsDouble: Double;
begin
  Result := FValue.TryToDouble;
end;

function TValueCriteria.GetAsExtended: Extended;
begin
  Result := FValue.TryToExtended;
end;

function TValueCriteria.GetAsInt64: Int64;
begin
  Result := FValue.TryToInt64;
end;

function TValueCriteria.GetAsInteger: Integer;
begin
  Result := FValue.TryToInt;
end;

function TValueCriteria.GetAsSmallint: Smallint;
begin
  Result := FValue.TryToInt;
end;

function TValueCriteria.GetAsString: String;
begin
  Result := FValue.ToString;
end;

function TValueCriteria.GetAsTime: TTime;
begin
  Result := FValue.TryToTime;
end;

end.
