unit AM.Freedom.TextGeneratorRegister;

interface

uses
  System.Generics.Collections,
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.SQLMappers.CustomSQLMapper,
  AM.Freedom.GeneratorTextList,
  AM.Freedom.SQLCommands.SchemaCommands,
  AM.Freedom.TextGenerator.SchemaCommandsTextGenerator;

type
  TTextGeneratorRegister = class sealed
  strict private
    class var FGeneratorsList: TGeneratorTextList;
  private
    class procedure CreateList;
    class procedure DestroyList;
  public
    class function FindTextGenerator(const pClass: TClass; const pSQLMapperClass: TSQLMapperClass = nil): TTextGeneratorClass;
    class procedure RegisterTextGenerator(const pTextGeneratorClass: TTextGeneratorClass; const pClass: TClass; const pSQLMapperClass: TSQLMapperClass = nil);
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.TextGenerator.FieldTextGenerator,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.TextGenerator.CustomLiteralArgumentTextGenerator,
  AM.Freedom.TextGenerator.GroupCriteriaTextGenerator,
  AM.Freedom.GroupCriteria,
  AM.Freedom.TextGenerator.TableArgumentTextGenerator,
  AM.Freedom.TextGenerator.ValueArgumentTextGenerator,
  AM.Freedom.TextGenerator.CriteriaTextGenerator,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.TextGenerator.CustomSelectTextGenerator,
  AM.Freedom.SQLMappers.CustomSelect,
  AM.Freedom.SQLMappers.WhereClause,
  AM.Freedom.SQLMappers.HavingClause,
  AM.Freedom.SQLMappers.Expressions,
  AM.Freedom.TextGenerator.ExpressionsTextGenerator,
  AM.Freedom.SQLMappers.FromClause,
  AM.Freedom.TextGenerator.FromClauseTextGenerator,
  AM.Freedom.SQLMappers.OrderByClause,
  AM.Freedom.TextGenerator.OrderByClauseTextGenerator,
  AM.Freedom.SQLMappers.GroupByClause,
  AM.Freedom.TextGenerator.GroupByClauseTextGenerator,
  AM.Freedom.SQLMappers.JoinClause,
  AM.Freedom.TextGenerator.JoinClauseTextGenerator,
  AM.Freedom.TextGenerator.SimpleSelectTextGenerator,
  AM.Freedom.SQLGenerator.SimpleSQLGenerator,
  AM.Freedom.SQLCommands.SizedFieldCommand,
  AM.Freedom.SQLCommands.ScaledFieldCommand,
  AM.Freedom.TextGenerator.CustomFieldCommandTextGenerator,
  AM.Freedom.TextGenerator.TableCommandsTextGenerator,
  AM.Freedom.SQLCommands.TableFieldCommands,
  AM.Freedom.SQLCommands.TableRowCommands,
  AM.Freedom.TextGenerator.BetweenArgumentTextGenerator,
  AM.Freedom.TextGenerator.InArgumentTextGenerator,
  AM.Freedom.TextGenerator.FieldCommandTextGenerator,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLCommands.TableCommands,
  AM.Freedom.TextGenerator.CustomConstraintTextGenerator,
  AM.Freedom.SQLCommands.Constraints,
  AM.Freedom.TextGenerator.ForeignKeyTextGenerator,
  AM.Freedom.TextGenerator.CustomFieldOptionsTextGenerator,
  AM.Freedom.SQLCommands.FieldOptions,
  AM.Freedom.SQLCommands.DomainCommands,
  AM.Freedom.TextGenerator.DomainTextGenerator,
  AM.Freedom.SQLMappers.FBExpressions,
  AM.Freedom.TextGenerator.FBExpressionsTextGenerator,
  AM.Freedom.SQLCommands.SequenceCommands,
  AM.Freedom.TextGenerator.SequenceCommandsTextGenerators,
  AM.Freedom.SQLMappers.MSSQLExpressions,
  AM.Freedom.TextGenerator.MSSQLExpressionsTextGenerator,
  AM.Freedom.TextGenerator.IndexCommandsTextGenerator,
  AM.Freedom.SQLCommands.IndexCommands;

class procedure TTextGeneratorRegister.CreateList;
begin
  if not Assigned(FGeneratorsList) then
  begin
    FGeneratorsList := TGeneratorTextList.Create;
  end;
end;

class procedure TTextGeneratorRegister.DestroyList;
begin
  FreeAndNil(FGeneratorsList);
end;

class function TTextGeneratorRegister.FindTextGenerator(const pClass: TClass; const pSQLMapperClass: TSQLMapperClass): TTextGeneratorClass;
var
  lClass: TClass;
begin
  Result := FGeneratorsList.FindTextGeneratorClass(pClass, pSQLMapperClass);
  if not Assigned(Result) then
  begin
    lClass := pClass.ClassParent;
    while (lClass <> TObject) and (Result = nil) do
    begin
      Result := FGeneratorsList.FindTextGeneratorClass(lClass, pSQLMapperClass);
      lClass := lClass.ClassParent;
    end;
  end;
end;

class procedure TTextGeneratorRegister.RegisterTextGenerator(const pTextGeneratorClass: TTextGeneratorClass; const pClass: TClass; const pSQLMapperClass: TSQLMapperClass);
begin
  if not Assigned(FGeneratorsList.FindTextGeneratorClass(pClass, pSQLMapperClass)) then
  begin
    FGeneratorsList.Add(TGeneratorTextItem.Create(pClass, pTextGeneratorClass, pSQLMapperClass));
  end;
end;

initialization
  TTextGeneratorRegister.CreateList;
    // arguments
  TTextGeneratorRegister.RegisterTextGenerator(TFieldTextGenerator, TFieldArgument);
  TTextGeneratorRegister.RegisterTextGenerator(TCustomLiteralArgumentTextGenerator, TCustomLiteralArgument);
  TTextGeneratorRegister.RegisterTextGenerator(TValueArgumentTextGenerator, TValueArgument);
  TTextGeneratorRegister.RegisterTextGenerator(TTableArgumentTextGenerator, TTableArgument);
  TTextGeneratorRegister.RegisterTextGenerator(TSimpleSelectTextGenerator, TSimpleSQLGenerator);
  TTextGeneratorRegister.RegisterTextGenerator(TBetweenArgumentTextGenerator, TBetweenArgument);
  TTextGeneratorRegister.RegisterTextGenerator(TInArgumentTextGenerator, TInArgument);
    // criteria
  TTextGeneratorRegister.RegisterTextGenerator(TGroupCriteriaTextGenerator, TGroupCriteria);
  TTextGeneratorRegister.RegisterTextGenerator(TCriteriaTextGenerator, TCriteria);
    // Expressions
  TTextGeneratorRegister.RegisterTextGenerator(TSumTextGenerator, TSum);
  TTextGeneratorRegister.RegisterTextGenerator(TMinTextGenerator, TMin);
  TTextGeneratorRegister.RegisterTextGenerator(TMaxTextGenerator, TMax);
  TTextGeneratorRegister.RegisterTextGenerator(TAvgTextGenerator, TAvg);
  TTextGeneratorRegister.RegisterTextGenerator(TCountTextGenerator, TCount);
  TTextGeneratorRegister.RegisterTextGenerator(TUpperTextGenerator, TUpper);
  TTextGeneratorRegister.RegisterTextGenerator(TLowerTextGenerator, TLower);
  TTextGeneratorRegister.RegisterTextGenerator(TCoalesceTextGenerator, TCoalesce);
  TTextGeneratorRegister.RegisterTextGenerator(TCastTextGenerator, TCast);
  TTextGeneratorRegister.RegisterTextGenerator(TCalcTextGenerator, TCalc);
  TTextGeneratorRegister.RegisterTextGenerator(TCaseWhenTextGenerator, TCaseWhen);
  TTextGeneratorRegister.RegisterTextGenerator(TFBListTextGenerator, TFBList);
  TTextGeneratorRegister.RegisterTextGenerator(TMSSQLDateAddTextGenerator, TMSSQLDateAdd);
  TTextGeneratorRegister.RegisterTextGenerator(TMSSQLGetDateTextGenerator, TMSSQLGetDate);
    // Clausulas
  TTextGeneratorRegister.RegisterTextGenerator(TCustomSelectTextGenerator, TCustomSelect);
  TTextGeneratorRegister.RegisterTextGenerator(TFromClauseTextGenerator, TFromClause);
  TTextGeneratorRegister.RegisterTextGenerator(TJoinClauseTextGenerator, TJoinClause);
  TTextGeneratorRegister.RegisterTextGenerator(TGroupCriteriaTextGenerator, TWhereClause);
  TTextGeneratorRegister.RegisterTextGenerator(TGroupByClauseTextGenerator, TGroupByClause);
  TTextGeneratorRegister.RegisterTextGenerator(TGroupCriteriaTextGenerator, THavingClause);
  TTextGeneratorRegister.RegisterTextGenerator(TOrderByClauseTextGenerator, TOrderByClause);
    // Fieldcommands
  TTextGeneratorRegister.RegisterTextGenerator(TCustomFieldCommandTextGenerator, TCustomFieldCommand);
  TTextGeneratorRegister.RegisterTextGenerator(TSizedFieldCommandTextGenerator, TSizedFieldCommand);
  TTextGeneratorRegister.RegisterTextGenerator(TScaledFieldCommandTextGenerator, TScaledFieldCommand);
    // FieldOptions
  TTextGeneratorRegister.RegisterTextGenerator(TFieldOptionsTextGenerator, TFieldOptions);
  TTextGeneratorRegister.RegisterTextGenerator(TCustomFieldOptionsTextGenerator, TCustomFieldOptions);
    // table constraints
  TTextGeneratorRegister.RegisterTextGenerator(TCustomConstraintTextGenerator, TCustomConstraint);
  TTextGeneratorRegister.RegisterTextGenerator(TForeignKeyTextGenerator, TForeignKey);
    // TableFieldCommands
  TTextGeneratorRegister.RegisterTextGenerator(TAddOrAlterFieldCommandTextGenerator, TAddAlterFieldCommand);
  TTextGeneratorRegister.RegisterTextGenerator(TAlterFieldCommandTextGenerator, TAlterFieldCommand);
  TTextGeneratorRegister.RegisterTextGenerator(TDropFieldCommandTextGenerator, TDropFieldCommand);
    // TableConstraintsCommands
  TTextGeneratorRegister.RegisterTextGenerator(TAddConstraintCommandTextGenerator, TAddConstraintCommand);
  TTextGeneratorRegister.RegisterTextGenerator(TDropConstraintCommandTextGenerator, TDropConstraintCommand);
    // TableRowsCommands
  TTextGeneratorRegister.RegisterTextGenerator(TUpdateCommandTextGenerator, TUpdateCommand);
  TTextGeneratorRegister.RegisterTextGenerator(TInsertCommandTextGenerator, TInsertCommand);
  TTextGeneratorRegister.RegisterTextGenerator(TDeleteCommandTextGenerator, TDeleteCommand);
    // tableCommands
  TTextGeneratorRegister.RegisterTextGenerator(TCreateTableCommandTextGenerator, TCreateTableCommand);
  TTextGeneratorRegister.RegisterTextGenerator(TAlterTableCommandTextGenerator, TAlterTableCommand);
  TTextGeneratorRegister.RegisterTextGenerator(TDropTableCommandTextGenerator, TDropTableCommand);
    // Index Commands
  TTextGeneratorRegister.RegisterTextGenerator(TCreateIndexCommandTextGenerator, TCreateIndexCommand);
  TTextGeneratorRegister.RegisterTextGenerator(TDropIndexCommandTextGenerator, TDropIndexCommand);
    // Sequence Commands
  TTextGeneratorRegister.RegisterTextGenerator(TCreateSequenceCommandTextGenerator, TCreateSequenceCommand);
  TTextGeneratorRegister.RegisterTextGenerator(TAlterSequenceCommandTextGenerator, TAlterSequenceCommand);
  TTextGeneratorRegister.RegisterTextGenerator(TDropSequenceCommandTextGenerator, TDropSequenceCommand);
    // domain commands
  TTextGeneratorRegister.RegisterTextGenerator(TDomainTextGenerator, TCustomDomainCommand);
    // Schema Commands
  TTextGeneratorRegister.RegisterTextGenerator(TCreateSchemaCommandTextGenerator, TCreateSchemaCommand);
  TTextGeneratorRegister.RegisterTextGenerator(TDropSchemaCommandTextGenerator, TDropSchemaCommand);

finalization

TTextGeneratorRegister.DestroyList;

end.
