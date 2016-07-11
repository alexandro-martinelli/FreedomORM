unit AM.Freedom.TextGenerator.FieldCommandTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomFieldCommandTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TSizedFieldCommandTextGenerator = class(TCustomFieldCommandTextGenerator)
  strict protected
    function GetFieldTypeAsString(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TScaledFieldCommandTextGenerator = class(TCustomFieldCommandTextGenerator)
  strict protected
    function GetFieldTypeAsString(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.SQLCommands.Fields,
  AM.Freedom.SQLCommands.SizedFieldCommand,
  AM.Freedom.SQLCommands.ScaledFieldCommand;

{ TSizedFieldCommandTextGenerator }

function TSizedFieldCommandTextGenerator.GetFieldTypeAsString(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := inherited +
      '(' +  IntToStr(TSizedFieldCommand(pObject).Size) + ')';
end;

{ TScaledFieldCommandTextGenerator }

function TScaledFieldCommandTextGenerator.GetFieldTypeAsString(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := inherited +
      '(' +  IntToStr(TScaledFieldCommand(pObject).Size) + ', ' + IntToStr(TScaledFieldCommand(pObject).Scale) + ')';
end;

end.
