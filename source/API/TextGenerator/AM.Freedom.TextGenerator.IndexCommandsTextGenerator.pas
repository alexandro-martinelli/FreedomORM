unit AM.Freedom.TextGenerator.IndexCommandsTextGenerator;

interface

uses
  System.SysUtils,
  AM.Freedom.SQLCommands.IndexCommands,
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.Helper.CommandType,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.TextGenerator.GenerateTextParams, AM.Freedom.IndexColumn;

type
  TCreateIndexCommandTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TDropIndexCommandTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

implementation

uses
  System.StrUtils,
  AM.Freedom.Helper.NullOption,
  AM.Freedom.Helper.IndexOption;

{ TCreateIndexCommandTextGenerator }

function TCreateIndexCommandTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lCreateIndex: TCreateIndexCommand;
  lColumn: TIndexColumn;
  lColumns: string;
begin
  lCreateIndex := TCreateIndexCommand(pObject);
  GetSQLMapper.AdjustIndexCommand(lCreateIndex);
  lColumns := '';
  for lColumn in lCreateIndex.Columns do
  begin
    lColumns := lColumns + ifthen(lColumns <> '', ', ') + lColumn.Name;
    if lColumn.SortType = Desc then
    begin
      lColumns := lColumns + ' desc';
    end;
    if (lColumn.NullOption <> noNone) then
    begin
      lColumns := lColumns + ' ' + lColumn.NullOption.ToString;
    end;
  end;
  Result := lCreateIndex.CommandType.ToString;
  if not (lCreateIndex.Option in [ioConcurrently, ioNone]) then
  begin
    Result := Result + ' ' + lCreateIndex.Option.ToString;
  end;
  Result := Result + ' index';
  if (lCreateIndex.Option = ioConcurrently) then
  begin
    Result := Result + ' ' + lCreateIndex.Option.ToString;
  end;
  Result := Format('%s %s on %s%s (%s)', [Result, GetSQLMapper.AdjustNameLength(lCreateIndex.Name), ifthen(lCreateIndex.Schema <> '', lCreateIndex.Schema + '.'),
      lCreateIndex.OnTable, lColumns]);
end;

{ TDropIndexCommandTextGenerator }

function TDropIndexCommandTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lDropIndex: TDropIndexCommand;
begin
  lDropIndex := TDropIndexCommand(pObject);
  Result := GetSQLMapper.GenerateDropIndexCommand(lDropIndex);
end;

end.
