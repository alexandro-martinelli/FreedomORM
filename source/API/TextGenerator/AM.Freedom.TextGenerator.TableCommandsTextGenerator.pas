unit AM.Freedom.TextGenerator.TableCommandsTextGenerator;

interface

uses
  System.Generics.Collections,
  AM.Freedom.TextGenerator.CustomTableCommandsTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.SQLCommands.TableCommands,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLCommands.Constraints,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLCommands.CustomTableCommand,
  AM.Freedom.SQLCommands.TableRowCommands;

type
  TAddOrAlterFieldCommandTextGenerator = class(TCustomTableFieldCommandTextGenerator)
  strict protected
    function GetTextParams(pObject: TObject): TGenerateTextParams; virtual;
    function GetStrCommand(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TAlterFieldCommandTextGenerator = class(TAddOrAlterFieldCommandTextGenerator)
  strict protected
    function GetTextParams(pObject: TObject): TGenerateTextParams; override;
  end;

  TDropFieldCommandTextGenerator = class(TCustomTableFieldCommandTextGenerator)
  strict protected
    function GetStrCommand(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TAddConstraintCommandTextGenerator = class(TCustomTableConstraintCommandTextGenerator)
  strict protected
    function GetStrCommand(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TDropConstraintCommandTextGenerator = class(TCustomTableConstraintCommandTextGenerator)
  strict protected
    function GetStrCommand(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TCreateTableCommandTextGenerator = class(TCustomTableCommandTextGenerator)
  strict private
    function GetFields(const pFields: TObjectList<TCustomFieldCommand>; pSchema: String = ''): String;
    function GetForeigns(const pForeigns: TObjectList<TForeignKey>): String;
    function GetUniques(const pUniques: TObjectList<TUniqueKey>): String;
  strict protected
    function GetStrCommand(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TAlterTableCommandTextGenerator = class(TCustomTableCommandTextGenerator)
  strict private
    function GetCommandStr(pCommand: TCustomTableCommand): String;
    function GetStrCommands(pCommands: TObjectList<TCustomTableCommand>): String;
  strict protected
    function GetStrCommand(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TDropTableCommandTextGenerator = class(TCustomTableCommandTextGenerator)
  strict protected
    function GetStrCommand(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TUpdateCommandTextGenerator = class(TCustomTableRowCommandTextGenerator)
  strict private
    function GetFieldsValues(pFieldsValues: TObjectList<TFieldValueArgument>): String;
  strict protected
    function GetStrCommand(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TInsertCommandTextGenerator = class(TCustomTableRowCommandTextGenerator)
  strict private
    function GetFields(pFields: TObjectList<TFieldArgument>): String;
    function GetParamsOrValues(pFields: TObjectList<TFieldArgument>; pValues: TObjectList<TCustomArgument>): String;
  strict protected
    function GetStrCommand(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

  TDeleteCommandTextGenerator = class(TCustomTableRowCommandTextGenerator)
  strict protected
    function GetStrCommand(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.SQLCommands.TableFieldCommands,
  AM.Freedom.EnumerationTypes;

{ TAddOrAlterFieldCommandTextGenerator }

function TAddOrAlterFieldCommandTextGenerator.GetStrCommand(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := GetTextFromGenerator(TAddAlterFieldCommand(pObject).Field, GetTextParams(pObject));
end;

function TAddOrAlterFieldCommandTextGenerator.GetTextParams(pObject: TObject): TGenerateTextParams;
begin
  Result := nil;
end;

{ TDropFieldCommandTextGenerator }

function TDropFieldCommandTextGenerator.GetStrCommand(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := TDropFieldCommand(pObject).Name;
end;
{ TAddConstraintCommandTextGenerator }

function TAddConstraintCommandTextGenerator.GetStrCommand(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := GetTextFromGenerator(TAddConstraintCommand(pObject).Constraint);
end;
{ TDropConstraintCommandTextGenerator }

function TDropConstraintCommandTextGenerator.GetStrCommand(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := TDropConstraintCommand(pObject).Name;
end;
{ TCreateTableCommandTextGenerator }

function TCreateTableCommandTextGenerator.GetFields(const pFields: TObjectList<TCustomFieldCommand>; pSchema: String): String;
var
  lField: TCustomFieldCommand;
  lParams: TGenerateTextParams;
begin
  Result := EmptyStr;
  for lField in pFields do
  begin
    lParams := TGenerateTextParams.Create(False, False);
    lParams.Schema := pSchema;
    Result := Result + ifthen(Result <> EmptyStr, ', ') + GetTextFromGenerator(lField, lParams);
  end;
end;

function TCreateTableCommandTextGenerator.GetForeigns(const pForeigns: TObjectList<TForeignKey>): String;
var
  lForeign: TForeignKey;
begin
  Result := EmptyStr;
  for lForeign in pForeigns do
  begin
    Result := Result + ifthen(Result <> EmptyStr, ', ') + GetTextFromGenerator(lForeign);
  end;
end;

function TCreateTableCommandTextGenerator.GetStrCommand(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lCommand: TCreateTableCommand;
  lConstraints: String;
begin
  lCommand := TCreateTableCommand(pObject);
  if lCommand.Schema <> '' then
  begin
    Result := lCommand.Schema + '.';
  end;
  Result := Result + lCommand.Name + ' (' + GetFields(lCommand.Fields, lCommand.Schema);
  if Assigned(lCommand.PrimaryKey) then
  begin
    Result := Result + ', ' + GetTextFromGenerator(lCommand.PrimaryKey);
  end;
  lConstraints := GetForeigns(lCommand.ForeignKeys);
  if lConstraints <> EmptyStr then
  begin
    Result := Result + ', ' + lConstraints;
  end;
  lConstraints := GetUniques(lCommand.UniqueKeys);
  if lConstraints <> EmptyStr then
  begin
    Result := Result + ', ' + lConstraints;
  end;
  Result := Result + ')';
end;

function TCreateTableCommandTextGenerator.GetUniques(const pUniques: TObjectList<TUniqueKey>): String;
var
  lUnique: TUniqueKey;
begin
  Result := EmptyStr;
  for lUnique in pUniques do
  begin
    Result := Result + ifthen(Result <> EmptyStr, ', ') + GetTextFromGenerator(lUnique);
  end;
end;
{ TUpdateCommandTextGenerator }

function TUpdateCommandTextGenerator.GetFieldsValues(pFieldsValues: TObjectList<TFieldValueArgument>): String;
var
  lFieldValue: TFieldValueArgument;
  lFieldName: string;
  function RemoveAlias(pFieldName: String): String;
  begin
    Result := pFieldName;
    if Pos('.', pFieldName) > 0 then
    begin
      Result := Copy(pFieldName, Pos('.', pFieldName) + 1, Length(pFieldName));
    end;
  end;

begin
  Result := EmptyStr;
  for lFieldValue in pFieldsValues do
  begin
    Result := Result + ifthen(Result <> EmptyStr, ', ');
    if lFieldValue.Value.InheritsFrom(TValueArgument) then
    begin
      lFieldName := RemoveAlias(GetTextFromGenerator(lFieldValue.Field));
      Result := Result + Format('%0:s = :%0:s', [lFieldName]);
    end
    else
    begin
      Result := Result + Format('%s = %s', [GetTextFromGenerator(lFieldValue.Field),
          GetTextFromGenerator(lFieldValue.Value, TGenerateTextParams.Create(False, True))]);
    end;
  end;
end;

function TUpdateCommandTextGenerator.GetStrCommand(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lCommand: TUpdateCommand;
  lWhere: string;
begin
  lCommand := TUpdateCommand(pObject);
  Result := GetTextFromGenerator(lCommand.Table, TGenerateTextParams.Create(True, False)) + ' set ' + GetFieldsValues(lCommand.FieldsValues);
  lWhere := GetTextFromGenerator(lCommand.WhereClause);
  if lWhere <> EmptyStr then
  begin
    Result := Result + ' where ' + lWhere;
  end;
end;
{ TInsertCommandTextGenerator }

function TInsertCommandTextGenerator.GetFields(pFields: TObjectList<TFieldArgument>): String;
var
  lField: TFieldArgument;
begin
  Result := EmptyStr;
  for lField in pFields do
  begin
    Result := Result + ifthen(Result <> EmptyStr, ', ') + GetTextFromGenerator(lField);
  end;
end;

function TInsertCommandTextGenerator.GetParamsOrValues(pFields: TObjectList<TFieldArgument>; pValues: TObjectList<TCustomArgument>): String;
var
  lCounter: Integer;
  lFieldName: string;
  function RemoveAlias(pFieldName: String): String;
  begin
    Result := pFieldName;
    if Pos('.', pFieldName) > 0 then
    begin
      Result := Copy(pFieldName, Pos('.', pFieldName) + 1, Length(pFieldName));
    end;
  end;

begin
  Result := EmptyStr;
  for lCounter := 0 to pFields.Count - 1 do
  begin
    if pValues.Items[lCounter].InheritsFrom(TValueArgument) then
    begin
      lFieldName := RemoveAlias(pFields.Items[lCounter].Name);
      Result := Result + ifthen(Result <> EmptyStr, ', ') + ':' + lFieldName;
    end
    else
    begin
      Result := Result + ifthen(Result <> EmptyStr, ', ') + GetTextFromGenerator(pValues.Items[lCounter]);
    end;
  end;
end;

function TInsertCommandTextGenerator.GetStrCommand(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lCommand: TInsertCommand;
begin
  lCommand := TInsertCommand(pObject);
  Result := Format('%s (%s) ', [GetTextFromGenerator(lCommand.Into), GetFields(lCommand.Fields)]);
  if lCommand.SelectClause = nil then
  begin
    Result := Result + Format('values (%s)', [GetParamsOrValues(lCommand.Fields, lCommand.Values)]);
  end
  else
  begin
    Result := Result + GetTextFromGenerator(lCommand.SelectClause, TGenerateTextParams.Create(False, True));
  end;
  if lCommand.ReturningFields.Count > 0 then
  begin
    GetSQLMapper.IncludeReturningClause(Result, lCommand.ReturningFields);
  end;
end;
{ TAlterTableCommandTextGenerator }

function TAlterTableCommandTextGenerator.GetCommandStr(pCommand: TCustomTableCommand): String;
begin
  Result := GetTextFromGenerator(pCommand);
end;

function TAlterTableCommandTextGenerator.GetStrCommand(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lCommand: TAlterTableCommand;
  lTableCommand: TCustomTableCommand;
  function GetSchemaName: String;
  begin
    if lCommand.Schema <> '' then
    begin
      Result := lCommand.Schema + '.';
    end;
  end;
begin
  lCommand := TAlterTableCommand(pObject);
  Result := '';
  if not GetSQLMapper.AlterTableSingleCommand then
  begin
    Result := GetSchemaName + lCommand.Name + ' ' + GetStrCommands(lCommand.Commands);
  end
  else
  begin
    for lTableCommand in lCommand.Commands do
    begin
      Result := Result + ifthen(Result <> EmptyStr, '; ' + GetSQLMapper.TableCommandToString(lCommand.CommandType) + ' ') +
          GetSchemaName + lCommand.Name + ' ' + GetCommandStr(lTableCommand);
    end;
    Result := Result + ';';
  end;
end;

function TAlterTableCommandTextGenerator.GetStrCommands(pCommands: TObjectList<TCustomTableCommand>): String;
var
  lCommand: TCustomTableCommand;
begin
  Result := EmptyStr;
  for lCommand in pCommands do
  begin
    Result := Result + ifthen(Result <> EmptyStr, ', ') + GetCommandStr(lCommand);
  end;
end;
{ TDeleteCommandTextGenerator }

function TDeleteCommandTextGenerator.GetStrCommand(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lCommand: TDeleteCommand;
  lWhere: String;
begin
  lCommand := TDeleteCommand(pObject);
  Result := GetTextFromGenerator(lCommand.FromTable);
  lWhere := GetTextFromGenerator(lCommand.WhereClause);
  if lWhere <> EmptyStr then
  begin
    Result := Result + ' where ' + lWhere;
  end;
end;
{ TDropTableCommandTextGenerator }

function TDropTableCommandTextGenerator.GetStrCommand(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := TDropTableCommand(pObject).Name;
  if TDropTableCommand(pObject).Schema <> '' then
  begin
    Result := TDropTableCommand(pObject).Schema + '.' + Result;
  end;
end;

{ TAlterFieldCommandTextGenerator }

function TAlterFieldCommandTextGenerator.GetTextParams(pObject: TObject): TGenerateTextParams;
begin
  Result := TGenerateTextParams.Create(False, False);
  if GetSQLMapper <> nil then
  begin
    Result.AlterFieldCommandParams := GetSQLMapper.AlterFieldCommandParams;
  end;
  Result.ChangedProperties := TAlterFieldCommand(pObject).ChangedProperties;
end;

end.
