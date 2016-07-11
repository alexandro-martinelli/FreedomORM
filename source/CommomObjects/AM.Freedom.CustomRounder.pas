unit AM.Freedom.CustomRounder;

interface

uses
  System.SysUtils,
  System.Math,
  AM.Freedom.RoundObject;

type
  TRounderClass = class of TCustomRounder;

  TCustomRounder = class
  public
    function RoundValue(pRoundObject: TRoundObject): Extended; virtual;
  end;


implementation

{ TCustomRounder }

uses
  AM.Freedom.EnumerationTypes;

function TCustomRounder.RoundValue(pRoundObject: TRoundObject): Extended;
var
  lOldRoundMode: TRoundingMode;
begin
  {
    rmNearest = Arredonda para o valor mais próximo e é o modo default.
    rmDown = Arredonda para baixo.
    rmUp = Arredonda para cima.
    rmTruncate = Trunca o valor.
  }
  Result := pRoundObject.RoundValue;
  lOldRoundMode := GetRoundMode;
  case pRoundObject.RoundDecimalsMode of
    rdmApproach: SetRoundMode(rmNearest);
    rdmDown: SetRoundMode(rmDown);
    rdmUp: SetRoundMode(rmUp);
    rdmTrunc: SetRoundMode(rmTruncate);
  end;
  Result := SimpleRoundTo(Result, -pRoundObject.RoundDecimals);
  SetRoundMode(lOldRoundMode);
end;

end.