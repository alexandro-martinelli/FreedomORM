unit AM.Freedom.ObjectFactory;

interface

uses
  System.Generics.Collections,
  System.SysUtils;

type
  TCustomObjectConstructorClass  = class of TCustomObjectConstructor;

  TCustomObjectConstructor = class abstract
  public
    function CreateNewObject(pClass: TClass): TObject; virtual;
    class function IsConstructorOfClass(pClass: TClass): Boolean; virtual;
  end;

  TObjectFactory = class sealed
  strict private
    class var FObjectConstructorList: TList<TCustomObjectConstructorClass>;
  private
    class procedure CreateList;
    class procedure DestroyList;
  public
    class procedure RegisterObjectConstructor(pObjectConstructorClass: TCustomObjectConstructorClass);
    class function ObjectConstructorClass(pClass: TClass): TCustomObjectConstructorClass;
    class function CreateNewObject(pClass: TClass): TObject;
  end;

implementation

uses
  AM.Freedom.Exceptions;

{ TCustomObjectConstructor<T> }

function TCustomObjectConstructor.CreateNewObject(pClass: TClass): TObject;
begin
  raise EInvalidMethodCallOnClass.Create('CreateNewObject', ClassName);
end;

{ TObjectConstructorList }

class procedure TObjectFactory.CreateList;
begin
  FObjectConstructorList := TList<TCustomObjectConstructorClass>.Create;
end;

class function TObjectFactory.CreateNewObject(pClass: TClass): TObject;
var
  lObjectConstructorClass: TCustomObjectConstructorClass;
  lObjectConstructor: TCustomObjectConstructor;
begin
  Result := nil;
  lObjectConstructorClass := ObjectConstructorClass(pClass);
  if (Assigned(lObjectConstructorClass)) then
  begin
    lObjectConstructor := lObjectConstructorClass.Create;
    try
      Result := lObjectConstructor.CreateNewObject(pClass);
    finally
      lObjectConstructor.Free;
    end;
  end;
end;

class procedure TObjectFactory.DestroyList;
begin
  FObjectConstructorList.Free;
end;

class function TObjectFactory.ObjectConstructorClass(pClass: TClass): TCustomObjectConstructorClass;
var
  lObjectConstructorClass: TCustomObjectConstructorClass;
begin
  Result := nil;
  for lObjectConstructorClass in FObjectConstructorList do
  begin
    if lObjectConstructorClass.IsConstructorOfClass(pClass) then
    begin
      Result := lObjectConstructorClass;
      Break;
    end;
  end;
end;


class procedure TObjectFactory.RegisterObjectConstructor(pObjectConstructorClass: TCustomObjectConstructorClass);
begin
  if not FObjectConstructorList.Contains(pObjectConstructorClass) then
  begin
    FObjectConstructorList.Add(pObjectConstructorClass);
  end;
end;

class function TCustomObjectConstructor.IsConstructorOfClass(pClass: TClass): Boolean;
begin
  raise EInvalidMethodCallOnClass.Create('IsConstructorOfClass', ClassName);
end;

initialization
  TObjectFactory.CreateList;

finalization
  TObjectFactory.DestroyList;

end.
