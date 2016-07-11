unit AM.Freedom.TextGenerator.SchemaCommandsTextGenerator;

interface

uses
  AM.Freedom.SQLCommands.SchemaCommands,
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TCreateSchemaCommandTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TDropSchemaCommandTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

implementation

uses
  System.SysUtils;

{ TCreateSchemaCommandTextGenerator }

function TCreateSchemaCommandTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := Format('create schema %s', [TCreateSchemaCommand(pObject).Name]);
end;

{ TDropSchemaCommandTextGenerator }

function TDropSchemaCommandTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := Format('drop schema %s', [TCreateSchemaCommand(pObject).Name]);
end;

end.
