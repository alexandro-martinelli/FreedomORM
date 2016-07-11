unit AM.Freedom.GroupCriteria.IGroupCriteria;

interface

uses
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria.Criteria;

type
  IGroupCriteria = interface
  ['{03258DBB-5401-4593-88E1-2093386EC6C5}']
    function GetPolicy: TPolicy;
    procedure SetPolicy(const aPolicy: TPolicy);
    function GetLimitRows: UInt32;
    procedure SetLimitRows(const aLimitRows: UInt32);
    function GetCriterias: TListCriterias;

    property Policy: TPolicy read GetPolicy write SetPolicy;
    property LimitRows: UInt32 read GetLimitRows write SetLimitRows;
    property Criterias: TListCriterias read GetCriterias;
  end;

implementation

end.
