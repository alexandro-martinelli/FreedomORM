unit AM.Freedom.IAliasable;

interface

uses
  AM.Freedom.INamed;

type
  IAliasable = interface(INamed)
  ['{CB096311-98B1-457A-B841-8A949CDDEE50}']
    function GetAlias: String;
    procedure SetAlias(const pAlias: String);

    property Alias: String read GetAlias write SetAlias;
  end;

implementation

end.
