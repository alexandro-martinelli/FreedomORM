unit AM.Freedom.SQLMappers.IJoin;

interface

uses
  AM.Freedom.GroupCriteria,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLMappers.ISQLClause;

type
  IJoin = interface(ISQLClause)
  ['{0BFAF10D-D823-4A23-BCF7-B96466363DEA}']
    function GetJoinOn: TGroupCriteria;
    function GetJoinKind: TJoinKind;
    procedure SetJoinKind(const pKind: TJoinKind);

    property Kind: TJoinKind read GetJoinKind write SetJoinKind;
    property JoinOn: TGroupCriteria read GetJoinOn;
  end;

implementation

end.
