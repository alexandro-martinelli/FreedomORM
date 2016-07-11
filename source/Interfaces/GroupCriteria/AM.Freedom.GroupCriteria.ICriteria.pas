unit AM.Freedom.GroupCriteria.ICriteria;

interface

uses
  AM.Freedom.GroupCriteria.Comparators,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria.ValueCriteria,
  AM.Freedom.SQLMapper.CustomArgument;

type
  ICriteria = interface
  ['{AB7A30C3-7AAA-4E27-BF01-85CF277BE5E4}']
    function GetLeftArgument: TCustomArgument;
    function GetComparator: TCustomComparator;
    function GetRigthArgument: TCustomArgument;

    procedure SetLeftArgument(const pLeftArgument: TCustomArgument);
    procedure SetComparator(const pComparator: TCustomComparator);
    procedure SetRigthArgument(const pRigthArgument: TCustomArgument);

    property LeftArgument: TCustomArgument read GetLeftArgument write SetLeftArgument;
    property Comparator: TCustomComparator read GetComparator write SetComparator;
    property RigthArgument: TCustomArgument read GetRigthArgument write SetRigthArgument;
  end;

implementation

end.
