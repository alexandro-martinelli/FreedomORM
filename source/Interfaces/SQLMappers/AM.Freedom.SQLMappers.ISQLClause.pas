unit AM.Freedom.SQLMappers.ISQLClause;

interface

uses
  AM.Freedom.SQLMapper.CustomArgument;

type
  ISQLClause = interface
  ['{B8744184-C622-4027-A0C1-6F7F890E8411}']
    function GetArgument: TCustomArgument;
    procedure SetArgument(const pArgument: TCustomArgument);

    property Argument: TCustomArgument read GetArgument write SetArgument;
end;

implementation

end.
