unit AM.Freedom.StringsConstructor;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.ObjectFactory;

type
  TStringsConstructor = class(TCustomObjectConstructor)
  public
    function CreateNewObject(pClass: TClass): TObject; override;
    class function IsConstructorOfClass(pClass: TClass): Boolean; override;
  end;


implementation

{ TStringsConstructor }

function TStringsConstructor.CreateNewObject(pClass: TClass): TObject;
begin
  Result := TStringList.Create;
end;

class function TStringsConstructor.IsConstructorOfClass(pClass: TClass): Boolean;
begin
  Result := pClass.InheritsFrom(TStrings);
end;

end.