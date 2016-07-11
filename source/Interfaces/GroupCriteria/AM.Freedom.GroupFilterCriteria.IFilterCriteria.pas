unit AM.Freedom.GroupFilterCriteria.IFilterCriteria;

interface

uses
  System.SysUtils;

type
  IFilterCriteria = interface
  ['{58F53548-9795-4003-9E5C-F3D1F585B4B3}']
    procedure SetPropertyValue(pValue: Variant);
    function CompareValues: Boolean;
  end;


implementation

end.