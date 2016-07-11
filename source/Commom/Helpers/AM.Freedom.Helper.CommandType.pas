unit AM.Freedom.Helper.CommandType;

interface

uses
  AM.Freedom.EnumerationTypes;

type
  TCommandTypeHelper = record Helper for TCommandType
  public
    function ToString: String;
  end;

implementation

{ TCommandTypeHelper }

function TCommandTypeHelper.ToString: String;
begin
  case Self of
    Select: Result := 'select';
    Add: Result := 'add';
    Alter: Result := 'alter';
    Drop: Result := 'drop';
    Create: Result := 'create';
  end;
end;

end.
