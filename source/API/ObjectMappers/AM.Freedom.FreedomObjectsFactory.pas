unit AM.Freedom.FreedomObjectsFactory;

interface

uses
  System.SysUtils;

type
  TFreedomObjectsFactory = class sealed
  strict private
    class var FAlreadRegistered: Boolean;
  public
    class procedure RegisterFreedomObject;
  end;

implementation

uses
  AM.Freedom.ObjectFactory,
  AM.Freedom.FreedomObjectConstructor,
  AM.Freedom.StringsConstructor,
  AM.Freedom.StreamsConstructor;

{ TFreedomObjectsFactory }

class procedure TFreedomObjectsFactory.RegisterFreedomObject;
begin
  if (not FAlreadRegistered) then
  begin
    TObjectFactory.RegisterObjectConstructor(TFreedomObjectConstructor);
    TObjectFactory.RegisterObjectConstructor(TStringsConstructor);
    TObjectFactory.RegisterObjectConstructor(TStreamsConstructor);
    FAlreadRegistered := True;
  end;
end;

end.