unit AM.UnitReader.Members.ArgumentMember;

interface

uses
  System.Generics.Collections,
  AM.UnitReader.Members.CustomMember, AM.UnitReader.Enumerations;

type
  TArgumentMember = class(TCustomMember)
  public
    constructor Create(pName: string); reintroduce;
  end;

  TArgumentList = class(TObjectList<TArgumentMember>);

implementation

{ TArgumentMember }

constructor TArgumentMember.Create(pName: string);
begin
  inherited Create(pName, vsStrictPrivate);
end;

end.
