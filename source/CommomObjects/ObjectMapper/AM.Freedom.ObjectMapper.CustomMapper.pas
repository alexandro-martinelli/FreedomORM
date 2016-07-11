unit AM.Freedom.ObjectMapper.CustomMapper;

interface

uses
  System.Generics.Collections,
  AM.Freedom.SQLMappers.AliasableObject;

type
  TCustomMapper = class abstract(TAliasableObject)
  public
    property Name;
  end;

implementation

end.
