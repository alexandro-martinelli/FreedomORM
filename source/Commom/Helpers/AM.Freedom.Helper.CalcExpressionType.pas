unit AM.Freedom.Helper.CalcExpressionType;

interface

uses
  AM.Freedom.EnumerationTypes;

type
  TCalcExpressionTypeHelper = record helper for TCalcExpressionType
  public
    function ToString: String;
  end;




implementation

{ TCalcExpressionTypeHelper }

function TCalcExpressionTypeHelper.ToString: String;
begin
  case Self of
    Addition: Result := '+';
    Subtract: Result := '-';
    Multiply: Result := '*';
    Divide: Result := '/';
  end;
end;

end.
