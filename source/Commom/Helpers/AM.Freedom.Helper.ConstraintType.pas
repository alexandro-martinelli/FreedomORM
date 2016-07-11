unit AM.Freedom.Helper.ConstraintType;

interface

uses
  AM.Freedom.EnumerationTypes;

type
  TConstraintTypeHelper = record Helper for TConstraintType
  public
    function ToString: String;
  end;


implementation

{ TCOnstraintTypeHelper }

function TConstraintTypeHelper.ToString: String;
begin
  case Self of
    PrimaryKey: Result := 'primary key';
    ForeignKey: Result := 'foreign key';
    UniqueKey: Result := 'unique'
  end;
end;

end.
