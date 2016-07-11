unit AM.Freedom.Helper.NullOption;

interface

uses
  System.SysUtils,
  AM.Freedom.EnumerationTypes;

type
  TNullOptionHelper = record Helper for TNullOption
  public
    function ToString: String;
  end;


implementation

{ TNullOptionHelper }

function TNullOptionHelper.ToString: String;
begin
  case Self of
    noFirst: Result := 'nulls first';
    noLast: Result := 'nulls last';
    else
      Result := '';
  end;
end;

end.