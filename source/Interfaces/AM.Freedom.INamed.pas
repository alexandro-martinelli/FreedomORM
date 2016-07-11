unit AM.Freedom.INamed;

interface

type
  INamed = interface
  ['{7A158C04-F8D2-4472-A3D1-6A87B76A91C8}']
    function GetName: String;
    procedure SetName(const pName: String);

    property Name: String read GetName write SetName;
  end;

implementation

end.
