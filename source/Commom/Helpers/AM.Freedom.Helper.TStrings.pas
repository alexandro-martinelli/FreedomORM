unit AM.Freedom.Helper.TStrings;

interface

uses
  System.Generics.Collections,
  System.Classes;

type
  TStringsHelper = class helper for TStrings
  public
    function ToList: TLIst<string>;
  end;

implementation

{ TStringsHelper }

function TStringsHelper.ToList: TLIst<string>;
var
  lString: String;
begin
  Result := TList<String>.Create;
  for lString in Self do
  begin
    Result.Add(lString);
  end;
end;

end.
