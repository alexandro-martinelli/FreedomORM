unit AM.Freedom.JSONClassConverterRegister;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  AM.Freedom.JSONConverter;

type
  TConverterDictionary = class
  private
    FMetaClass: TClass;
    FMetaClassName: String;
    FJSONConverterClass: TClassJSONConverterClass;
    procedure SetMetaClass(const Value: TClass);
    procedure SetMetaClassName(const Value: String);
  public
    property MetaClass: TClass read FMetaClass write SetMetaClass;
    property MetaClassName: String read FMetaClassName write SetMetaClassName;
    property JSONConverterClass: TClassJSONConverterClass read FJSONConverterClass write FJSONConverterClass;
  end;

  TConverterDictionaryList = class(TObjectList<TConverterDictionary>)
  public
    function FindConverterForClass(pForClass: TClass): TConverterDictionary;
    function FindConverterForClassName(pForClassName: String): TConverterDictionary;
  end;


  TJSONClassConverterRegister = class sealed
  private
    class var FConverters: TConverterDictionaryList;
    class procedure CreateList;
    class procedure DestroyList;
  public
    class procedure RegisterClassConverter(pConverterClass: TClassJSONConverterClass; pForClass: TClass);
    class procedure RegisterClassNameConverter(pConverterClass: TClassJSONConverterClass; pForClassName: String);
    class function GetClassConverterForClass(pForClass: TClass): TClassJSONConverterClass;
    class function GetClassConverterForClassName(pForClassName: String): TClassJSONConverterClass;
    class function GetConverterForClassOrClassName(pForClass: TClass; pForClassName: String): TClassJSONConverterClass;
  end;

implementation

{ TJSONClassConverterRegister }

class procedure TJSONClassConverterRegister.RegisterClassConverter(pConverterClass: TClassJSONConverterClass;
  pForClass: TClass);
var
  lDictionary: TConverterDictionary;
begin
  CreateList;
  lDictionary := FConverters.FindConverterForClass(pForClass);
  if ((lDictionary = nil) or (pForClass = nil)) then
  begin
    if (pForClass <> nil) then
    begin
      lDictionary := TConverterDictionary.Create;
      lDictionary.MetaClass := pForClass;
      lDictionary.JSONConverterClass := pConverterClass;
      FConverters.Add(lDictionary)
    end
    else
    begin
      lDictionary.MetaClass := pForClass;
      lDictionary.JSONConverterClass := pConverterClass;
    end;
  end;
end;

class function TJSONClassConverterRegister.GetClassConverterForClass(pForClass: TClass): TClassJSONConverterClass;
var
  lDictionary: TConverterDictionary;
  lClass: TClass;
begin
  Result := nil;
  lDictionary := nil;
  lClass := pForClass;
  while Assigned(lClass) and (not Assigned(lDictionary)) do
  begin
    lDictionary := FConverters.FindConverterForClass(lClass);
    lClass := lClass.ClassParent;
  end;
  if (Assigned(lDictionary) and (lDictionary.MetaClassName = '')) then
  begin
    Result := lDictionary.JSONConverterClass;
  end;
  if (Result = nil) then
  begin
    for lDictionary in FConverters do
    begin
      if (lDictionary.MetaClass = pForClass) and (lDictionary.MetaClassName = '') then
      begin
        Result := lDictionary.JSONConverterClass;
        Break;
      end;
    end;
  end;
end;

class procedure TJSONClassConverterRegister.CreateList;
begin
  if (not Assigned(FConverters)) then
  begin
    FConverters := TConverterDictionaryList.Create;
  end;
end;

class procedure TJSONClassConverterRegister.DestroyList;
begin
  FreeAndNil(FConverters);
end;

class procedure TJSONClassConverterRegister.RegisterClassNameConverter(pConverterClass: TClassJSONConverterClass; pForClassName: String);
var
  lDictionary: TConverterDictionary;
begin
  if (Pos('<', pForClassName) > 0) then
  begin
    Delete(pForClassName, Pos('<', pForClassName) - 1, Length(pForClassName));
  end;
  CreateList;
  lDictionary := FConverters.FindConverterForClassName(pForClassName);
  if ((lDictionary = nil) or (pForClassName = '')) then
  begin
    if (pForClassName <> '') then
    begin
      lDictionary := TConverterDictionary.Create;
      lDictionary.MetaClassName := pForClassName;
      lDictionary.JSONConverterClass := pConverterClass;
      FConverters.Add(lDictionary)
    end
    else
    begin
      lDictionary.MetaClassName := pForClassName;
      lDictionary.JSONConverterClass := pConverterClass;
    end;
  end;

end;

class function TJSONClassConverterRegister.GetClassConverterForClassName(
  pForClassName: String): TClassJSONConverterClass;
var
  lDictionary: TConverterDictionary;
begin
  Result := nil;
  if (Pos('<', pForClassName) > 0) then
  begin
    Delete(pForClassName, Pos('<', pForClassName), Length(pForClassName));
  end;
  lDictionary := FConverters.FindConverterForClassName(pForClassName);
  if (Assigned(lDictionary) and (lDictionary.MetaClass = nil)) then
  begin
    Result := lDictionary.JSONConverterClass;
  end;
  if (Result = nil) then
  begin
    for lDictionary in FConverters do
    begin
      if (lDictionary.MetaClassName = pForClassName) and (lDictionary.MetaClass = nil) then
      begin
        Result := lDictionary.JSONConverterClass;
        Break;
      end;
    end;
  end;
end;

class function TJSONClassConverterRegister.GetConverterForClassOrClassName(pForClass: TClass; pForClassName: String): TClassJSONConverterClass;
begin
  Result := GetClassConverterForClass(pForClass);
  if (Result = nil) then
  begin
    Result := GetClassConverterForClassName(pForClassName);
    if (Result = nil) then
    begin
      Result := TClassJSONConverter;
    end;
  end;
end;

{ TConverterDictionary }

procedure TConverterDictionary.SetMetaClass(const Value: TClass);
begin
  FMetaClass := Value;
  FMetaClassName := '';
end;

procedure TConverterDictionary.SetMetaClassName(const Value: String);
begin
  FMetaClassName := Value;
  FMetaClass := nil;
end;

{ TConverterDictionaryList }

function TConverterDictionaryList.FindConverterForClass(pForClass: TClass): TConverterDictionary;
var
  lDictionary: TConverterDictionary;
begin
  Result := nil;
  for lDictionary in Self do
  begin
    if (lDictionary.MetaClass = pForClass) then
    begin
      Result := lDictionary;
      Break;
    end;
  end;
end;

function TConverterDictionaryList.FindConverterForClassName(pForClassName: String): TConverterDictionary;
var
  lDictionary: TConverterDictionary;
begin
  Result := nil;
  for lDictionary in Self do
  begin
    if (lDictionary.MetaClassName = pForClassName) then
    begin
      Result := lDictionary;
      Break;
    end;
  end;
end;

initialization

finalization
  TJSONClassConverterRegister.DestroyList;

end.