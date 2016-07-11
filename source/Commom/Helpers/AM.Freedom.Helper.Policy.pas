unit AM.Freedom.Helper.Policy;

interface

uses
  AM.Freedom.EnumerationTypes;

type
  TPolicyHelper = record helper for TPolicy
  public
    function ToString: String;
    function ToDescriptor: String;
  end;

implementation

{ TPolicyHelper }

function TPolicyHelper.ToDescriptor: String;
begin
  case Self of
    poAnd: Result := 'poAnd';
    poOr:  Result := 'poOr';
    poAndNot:  Result := 'poAndNot';
    poOrNot:  Result := 'poOrNot';
  end;
end;

function TPolicyHelper.ToString: String;
begin
  case Self of
    poAnd: Result := ' and ';
    poOr:  Result := ' or ';
    poAndNot:  Result := ' and not ';
    poOrNot:  Result := ' or not ';
  end;
end;

end.
