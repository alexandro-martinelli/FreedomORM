unit AM.Freedom.SQLMappers.AliasableObject;

interface

uses
  AM.Freedom.IAliasable,
  AM.Freedom.SQLMappers.NamedObject,
  AM.Freedom.EnumerationTypes;

type
  TAliasableObjectClass  = class of TAliasableObject;

  TAliasableObject = class abstract(TNamedObject, IAliasable)
  strict private
    FAlias: string;
  strict protected
    function GetAlias: string;
    procedure SetAlias(const pAlias: string);
    property Alias: string read GetAlias write SetAlias;
  end;

implementation

{ TAliasableObject }

function TAliasableObject.GetAlias: string;
begin
  Result := FAlias;
end;

procedure TAliasableObject.SetAlias(const pAlias: string);
begin
  FAlias := pAlias
end;

end.
