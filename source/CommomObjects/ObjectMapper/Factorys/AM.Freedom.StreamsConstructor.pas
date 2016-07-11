unit AM.Freedom.StreamsConstructor;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.ObjectFactory;

type
  TStreamsConstructor = class(TCustomObjectConstructor)
  public
    function CreateNewObject(pClass: TClass): TObject; override;
    class function IsConstructorOfClass(pClass: TClass): Boolean; override;
  end;


implementation

{ TStreamsConstructor }

function TStreamsConstructor.CreateNewObject(pClass: TClass): TObject;
begin
  Result := TMemoryStream.Create;
end;

class function TStreamsConstructor.IsConstructorOfClass(pClass: TClass): Boolean;
begin
  Result := pClass.InheritsFrom(TStream);
end;

end.
