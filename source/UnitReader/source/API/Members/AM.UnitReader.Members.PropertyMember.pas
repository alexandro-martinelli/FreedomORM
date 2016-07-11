unit AM.UnitReader.Members.PropertyMember;

interface

uses
  AM.UnitReader.Members.CustomMember,
  AM.UnitReader.Enumerations;

type
  TPropertyMember = class(TCustomMember)
  private
    FPropertyType: TMemberType;
    FPropertyTypeName: String;
  public
    constructor Create(pName, pPropertyTypeName: String; pPropertyType: TMemberType = mtUnknow; pVisibility: TVisibilityScope = vsStrictPrivate); reintroduce;
    property PropertyType: TMemberType read FPropertyType write FPropertyType;
    property PropertyTypeName: String read FPropertyTypeName write FPropertyTypeName;
  end;

  TPropertyList = class(TCustomMemberList<TPropertyMember>)
  public
    function FindProperty(pPropertyName: String): Boolean;
  end;

implementation

uses
  System.SysUtils;

{ TPropertyMember }

constructor TPropertyMember.Create(pName, pPropertyTypeName: String; pPropertyType: TMemberType; pVisibility: TVisibilityScope);
begin
  inherited Create(pName, pVisibility);
  FPropertyType := pPropertyType;
  FPropertyTypeName := pPropertyTypeName;
end;

{ TPropertyList }

function TPropertyList.FindProperty(pPropertyName: String): Boolean;
var
  lProperty: TPropertyMember;
begin
  Result := False;
  for lProperty in Self do
  begin
    Result := SameText(lProperty.Name, pPropertyName);
    if (Result) then
    begin
      Break;
    end;
  end;
end;

end.
