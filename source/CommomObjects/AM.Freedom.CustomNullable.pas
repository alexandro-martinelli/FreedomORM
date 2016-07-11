unit AM.Freedom.CustomNullable;

interface

type
  TNullableCompareClass = class of TCustomNullableCompare;

  TCustomNullableCompare = class
  strict protected
    function GetDateTimeNullable: TDateTime; virtual;
    function GetFloatNullable: Double; virtual;
    function GetIntegerNullable: Integer; virtual;
    function GetStringNullable: String; virtual;
  public
    property IntegerNullable: Integer read GetIntegerNullable;
    property StringNullable: String read GetStringNullable;
    property DateTimeNullable: TDateTime read GetDateTimeNullable;
    property FloatNullable: Double read GetFloatNullable;
  end;

implementation

{ TCustomNullable }

function TCustomNullableCompare.GetDateTimeNullable: TDateTime;
begin
  Result := 0;
end;

function TCustomNullableCompare.GetFloatNullable: Double;
begin
  Result := 0.0;
end;

function TCustomNullableCompare.GetIntegerNullable: Integer;
begin
  Result := 0;
end;

function TCustomNullableCompare.GetStringNullable: String;
begin
  Result := '';
end;

end.
