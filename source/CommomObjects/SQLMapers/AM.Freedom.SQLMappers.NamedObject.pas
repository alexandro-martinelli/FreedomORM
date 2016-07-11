unit AM.Freedom.SQLMappers.NamedObject;

interface

uses
  AM.Freedom.INamed,
  AM.Freedom.JSONFreedomObject;

type
  TNamedObject = class(TJSONFreedomObject, INamed)
  strict private
    FName: String;
  strict protected
    function GetName: String;
    procedure SetName(const pName: String);
    property Name: String read GetName write SetName;
  end;

implementation

{ TNamedObject }

function TNamedObject.GetName: String;
begin
  Result := FName;
end;

procedure TNamedObject.SetName(const pName: String);
begin
  FName := pName;
end;

end.
