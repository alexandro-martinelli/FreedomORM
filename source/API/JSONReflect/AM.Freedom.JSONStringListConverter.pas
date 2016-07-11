unit AM.Freedom.JSONStringListConverter;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.JSONObjects,
  AM.Freedom.JSONConverter;

type
  TJSONStringListConverter = class(TClassJSONConverter)
  public
    function Convert(pObject: TObject): TJSONValue; override;
  end;

implementation

uses
  AM.Freedom.JSONClassConverterRegister;

{ TJSONStringListConverter }

function TJSONStringListConverter.Convert(pObject: TObject): TJSONValue;
begin
  Result := TJSONString.Create(TStringList(pObject).Text);
end;

initialization
  TJSONClassConverterRegister.RegisterClassConverter(TJSONStringListConverter, TStrings);

end.
