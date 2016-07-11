unit AM.Freedom.GroupCriteria.IComparator;

interface

uses
  AM.Freedom.EnumerationTypes;

type
  IComparator = interface
  ['{355CDD56-B6AC-48BA-9E05-F722A28F5C16}']
    function GetAllowsSQLAfter: Boolean;
    function GetComparatorType: TComparatorType;

    function Comparator: string;
    property ComparatorType: TComparatorType read GetComparatorType;
    property AllowsSQLAfter: Boolean read GetAllowsSQLAfter;
  end;

implementation

end.
