unit AM.Freedom.FreedomObjectConstructor;

interface

uses
  AM.Freedom.FreedomObject,
  AM.Freedom.ObjectFactory,
  System.SysUtils;

type
  TFreedomObjectConstructor = class(TCustomObjectConstructor)
  public
    function CreateNewObject(pClass: TClass): TObject; override;
    class function IsConstructorOfClass(pClass: TClass): Boolean; override;
  end;

implementation

{ TFreedomObjectConstructor }

uses
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.ILazy;

function TFreedomObjectConstructor.CreateNewObject(pClass: TClass): TObject;
begin
  Result := nil;
  if IsConstructorOfClass(pClass) then
  begin
    Result := pClass.Create;
  end;
end;

class function TFreedomObjectConstructor.IsConstructorOfClass(pClass: TClass): Boolean;
begin
  Result := pClass.InheritsFrom(TFreedomObject) or Supports(pClass, IFreedomObjectList) or Supports(pClass, ILazy);
end;

end.
