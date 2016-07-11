unit AM.Freedom.SQLMappers.ISelect;

interface

uses
  AM.Freedom.SQLMappers.SQLFieldList;

type
  ISelect = interface
  ['{44AA4F4D-3DE7-4E23-9C79-2822DB864999}']
    function GetFieldList: TSQLFieldList;

    property FieldList: TSQLFieldList read GetFieldList;
  end;



implementation

end.
