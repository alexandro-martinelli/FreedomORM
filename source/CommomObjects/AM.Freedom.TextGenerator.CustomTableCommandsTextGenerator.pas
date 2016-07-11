unit AM.Freedom.TextGenerator.CustomTableCommandsTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TCustomTableFieldCommandTextGenerator = class(TCustomTextGenerator)
  strict protected
    function GetStrCommand(pObject: TObject; pParams: TGenerateTextParams = nil): string; virtual; abstract;
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TCustomTableConstraintCommandTextGenerator = class(TCustomTextGenerator)
  strict protected
    function GetStrCommand(pObject: TObject; pParams: TGenerateTextParams = nil): string; virtual; abstract;
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TCustomTableCommandTextGenerator = class(TCustomTextGenerator)
  strict protected
    function GetStrCommand(pObject: TObject; pParams: TGenerateTextParams = nil): string; virtual; abstract;
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TCustomTableRowCommandTextGenerator = class(TCustomTextGenerator)
  strict protected
    function GetStrCommand(pObject: TObject; pParams: TGenerateTextParams = nil): string; virtual; abstract;
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;


implementation

uses
  AM.Freedom.SQLMapper.CustomArgument;

{ TCustomTableCommandTextGenerator }

function TCustomTableFieldCommandTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lCommand: TCustomCommand;
begin
  lCommand := TCustomCommand(pObject);
  Result := GetSQLMapper.TableFieldCommandToString(lCommand.CommandType) + ' ' + GetStrCommand(pObject, pParams);
end;

{ TCustomTableCommandTextGenerator }

function TCustomTableCommandTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := GetSQLMapper.TableCommandToString(TCustomCommand(pObject).CommandType) + ' ' + GetStrCommand(pObject, pParams);
end;

{ TCustomConstraintCommandTextGenerator }

function TCustomTableConstraintCommandTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lCommand: TCustomCommand;
begin
  lCommand := TCustomCommand(pObject);
  Result := GetSQLMapper.TableConstraintCommandToString(lCommand.CommandType) + ' ' + GetStrCommand(pObject, pParams);
end;

{ TCustomTableRowCommandTextGenerator }

function TCustomTableRowCommandTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := GetSQLMapper.TableRowCommandToString(TCustomCommand(pObject).CommandType) + ' ';
  if TCustomCommand(pObject).Schema <> '' then
  begin
    Result := Result + TCustomCommand(pObject).Schema + '.';
  end;
  Result := Result + GetStrCommand(pObject, pParams);
end;

end.
