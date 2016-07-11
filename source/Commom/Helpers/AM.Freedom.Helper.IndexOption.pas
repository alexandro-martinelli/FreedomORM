unit AM.Freedom.Helper.IndexOption;

interface

uses
  System.SysUtils,
  AM.Freedom.EnumerationTypes;

type
  TIndexOptionHelper = record helper for TIndexOption
  public
    function ToString: string;
  end;

implementation

{ TIndexOptionHelper }

function TIndexOptionHelper.ToString: string;
begin
  case self of
    ioClustered: Result := 'clustered';
    ioConcurrently: Result := 'concurrently';
    ioDescending: Result := 'descending';
    else
      Result := '';
  end;
end;

end.
