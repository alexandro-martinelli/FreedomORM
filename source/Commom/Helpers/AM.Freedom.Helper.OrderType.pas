unit AM.Freedom.Helper.OrderType;

interface

uses
  AM.Freedom.EnumerationTypes;


type
  TOrderTypeHelper = record Helper for TOrderType
  public
    function ToString: String;
  end;

implementation

{ TOrderTypeHelper }

function TOrderTypeHelper.ToString: String;
begin
  case Self of
    Asc: Result:= 'Asc';
    Desc: Result := 'Desc';
  end;
end;

end.
