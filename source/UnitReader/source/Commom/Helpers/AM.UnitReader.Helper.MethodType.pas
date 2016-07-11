unit AM.UnitReader.Helper.MethodType;

interface

uses
  AM.UnitReader.Enumerations;

type
  TMethodTypeHelper = record helper for TMethodType
  public
    function ToString: String;
  end;

implementation

{ TMethodTypeHelper }

function TMethodTypeHelper.ToString: String;
begin
  case Self of
    mtProcedure: Result := 'procedure';
    mtFunction: Result := 'function';
    mtConstructor: Result := 'constructor';
    mtDestructor: Result := 'destructor';
  end;
end;

end.
