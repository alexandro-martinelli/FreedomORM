unit AM.Freedom.Helper.JoinKind;

interface

uses
  AM.Freedom.EnumerationTypes;

type
  TJoinKindHelper = record helper for TJoinKind
  public
    function ToSQL: string;
  end;

implementation

{ TJoinKindHelper }

function TJoinKindHelper.ToSQL: string;
begin
  case Self of
    jkJoin:
      Result := 'join';
    jkLeft:
      Result := 'left join';
    jkRigth:
      Result := 'rigth join';
    jkInner:
      Result := 'inner join';
    jkCross:
      Result := 'cross join';
    jkOuter:
      Result := 'outer join';
    jkLeftOuter:
      Result := 'left outer join';
    jkRigthOuter:
      Result := 'rigth outer join';
    jkCrossOuter:
      Result := 'cross outer join';
  end;
end;

end.

