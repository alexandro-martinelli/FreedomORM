unit AM.Freedom.TextGenerator.SequenceCommandsTextGenerators;

interface

uses
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.SQLCommands.SequenceCommands;

type
  TCreateSequenceCommandTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TAlterSequenceCommandTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TDropSequenceCommandTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.EnumerationTypes;

{ TCreateSequenceTextGenerator }

function TCreateSequenceCommandTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lCreateSequence: TCreateSequenceCommand;
begin
  lCreateSequence := TCreateSequenceCommand(pObject);
  Result := Format('create sequence %s', [ifthen(lCreateSequence.Schema <> '', lCreateSequence.Schema + '.') + lCreateSequence.Name]);
end;

{ TAlterSequenceTextGenerator }

function TAlterSequenceCommandTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lAlterSequence: TAlterSequenceCommand;
begin
  lAlterSequence := TAlterSequenceCommand(pObject);
  Result := GetSQLMapper.SequenceSQL(lAlterSequence.Name, lAlterSequence.Schema, lAlterSequence.RestartWith, TCommandType.Alter);
end;

{ TDropSequenceCommandTextGenerator }

function TDropSequenceCommandTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lDropSequence: TDropSequenceCommand;
begin
  lDropSequence := TDropSequenceCommand(pObject);
  Result := Format('drop sequence %s', [ifthen(lDropSequence.Schema <> '', lDropSequence.Schema + '.') + lDropSequence.Name]);
end;

end.
