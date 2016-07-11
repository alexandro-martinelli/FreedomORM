unit AM.UnitReader.Helper.MemberVisibility;

interface

uses
  AM.UnitReader.Enumerations;

type
  TMemberVisibilityHelper = record Helper for TVisibilityScope
  public
    function ToString: String;
  end;

implementation

{ TMemberVisibilityHelper }

function TMemberVisibilityHelper.ToString: String;
begin
  case Self of
    vsStrictPrivate: Result := 'strict private';
    vsPrivate: Result := 'private';
    vsStrictProtected: Result := 'strict protected';
    vsProtected: Result := 'protected';
    vsPublic: Result := 'public';
    vsPublished: Result := 'published';
    else
      Result := '';
  end;
end;

end.
