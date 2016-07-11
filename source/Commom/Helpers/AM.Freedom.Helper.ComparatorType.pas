unit AM.Freedom.Helper.ComparatorType;

interface

uses
  AM.Freedom.EnumerationTypes;

type
  TComparatorTypeHelper = record helper for TComparatorType
  public
    function ToString: String;
  end;

implementation

{ TComparatorTypeHelper }

function TComparatorTypeHelper.ToString: String;
begin
  case Self of
    cpDifferent:  Result := '<> %s';
    cpGreaterThan: Result := '> %s';
    cpLessThan: Result := '< %s';
    cpGreaterThanOrEqualTo: Result := '>= %s';
    cpLessThanOrEqualTo: Result := '<= %s';
    cpContaining: Result := 'containing (%s)';
    cpStartingWith: Result := 'starting with (%s)';
    cpLikeLeft, cpLikeMidle, cpLikeRigth: Result := 'like (%s)';
    cpBetween: Result := 'between %s';
    cpIn: Result := 'in (%s)';
    cpExists: Result := 'exists (%s)';
    else
      Result := '= %s'
  end;
end;

end.
