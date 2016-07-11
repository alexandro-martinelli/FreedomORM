unit AM.Freedom.LazyJSONConverter;

interface

uses
  System.SysUtils,
  AM.Freedom.JSONConverter,
  AM.Freedom.JSONObjects;

type
  TLazyJSONConverter = class(TClassJSONConverter)
  public
    function Convert(pObject: TObject): TJSONValue; override;
  end;

implementation

uses
  System.Variants,
  AM.Freedom.ILazy,
  AM.Freedom.JSONClassConverterRegister;

{ TLazyJSONConverter }

function TLazyJSONConverter.Convert(pObject: TObject): TJSONValue;
var
  lLazy: ILazy;
  lLazyID: Variant;
  lJSONObject: TJSONObject;
  lObject: TObject;
  lConverter: TClassJSONConverter;
  lJSONValue: TJSONValue;
begin
  Result := nil;
  lJSONObject := TJSONObject.Create;
  if Supports(pObject, ILazy, lLazy) then
  begin
    lLazyID := lLazy.GetLazyID;
    if (not VarIsNull(lLazyID)) then
    begin
      lJSONObject.AddElement(TJSONPair.Create('ID', TJSONInteger.Create(lLazyID)));
    end;
    if lLazy.Loaded then
    begin
      lObject := lLazy.GetObject;
      lConverter := TJSONClassConverterRegister.GetConverterForClassOrClassName(lObject.ClassType, lObject.ClassName).Create;
      lJSONValue := lConverter.Convert(lObject);
      lJSONObject.AddElement(TJSONPair.Create('fields', lJSONValue));
    end;
  end;
  if (lJSONObject.Elements.Count = 0) then
  begin
    lJSONObject.Free;
  end
  else
  begin
    Result := lJSONObject;
  end;
end;

initialization
  TJSONClassConverterRegister.RegisterClassNameConverter(TLazyJSONConverter, 'TLazy');

end.