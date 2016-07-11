unit AM.UnitReader.Helper.WordType;

interface

uses
  AM.UnitReader.Enumerations;

type
  TWordTypeHelper = record helper for TWordType
  public
    function ToString: String;
  end;

implementation

{ TWordTypeHelper }

function TWordTypeHelper.ToString: String;
begin
  case Self of
    wtMethod: Result := 'Method';
    wtParameter: Result := 'Parameter';
    wtVariable: Result := 'Variable';
    wtConstant: Result := 'Constant';
  end;
end;

end.
