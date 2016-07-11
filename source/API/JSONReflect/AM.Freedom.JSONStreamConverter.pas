unit AM.Freedom.JSONStreamConverter;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.JSONObjects,
  AM.Freedom.JSONConverter;

type
  TJSONStreamConverter = class(TClassJSONConverter)
  public
    function Convert(pObject: TObject): TJSONValue; override;
  end;

implementation

uses
  AM.Freedom.JSONClassConverterRegister;

{ TJSONStreamConverter }

function TJSONStreamConverter.Convert(pObject: TObject): TJSONValue;
var
  lByteArray: TArray<Byte>;
begin
  Result := nil;
  if (pObject.InheritsFrom(TStringStream)) then
  begin
    Result := TJSONString.Create(TStringStream(pObject).DataString);
  end
  else if (pObject.InheritsFrom(TMemoryStream)) then
  begin
    TMemoryStream(pObject).Position := 0;
    TMemoryStream(pObject).WriteData(lByteArray, 0);
    Result := TJSONArray.Create;
    TJSONArray(Result).AddElement(lByteArray);
  end;
end;

initialization
  TJSONClassConverterRegister.RegisterClassConverter(TJSONStreamConverter, TStream);

end.