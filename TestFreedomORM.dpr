program TestFreedomORM;

{$I FreedomORM.inc}

{$R *.res}

uses
  System.Classes,
  Data.SqlExpr,
  DUnitTestRunner,
  FireDac.Comp.Client,
  AM.Freedom.TODO in 'AM.Freedom.TODO.pas',
  uObjectsTests in 'source\tests\uObjectsTests.pas',
  uTestArguments in 'source\tests\uTestArguments.pas',
  uTestExpressions in 'source\tests\uTestExpressions.pas',
  uTestComparators in 'source\tests\uTestComparators.pas',
  uTestCriteria in 'source\tests\uTestCriteria.pas',
  uTestGroupCriteria in 'source\tests\uTestGroupCriteria.pas',
  uTestCreateTableCommand in 'source\tests\uTestCreateTableCommand.pas',
  uTestAlterTableCommand in 'source\tests\uTestAlterTableCommand.pas',
  uTestCustomFieldOptions in 'source\tests\uTestCustomFieldOptions.pas',
  uTestFields in 'source\tests\uTestFields.pas',
  uTestSQLFieldList in 'source\tests\uTestSQLFieldList.pas',
  uTestContraints in 'source\tests\uTestContraints.pas',
  uTestFromClause in 'source\tests\uTestFromClause.pas',
  uTestGroupByClause in 'source\tests\uTestGroupByClause.pas',
  uTestTOrderByClause in 'source\tests\uTestTOrderByClause.pas',
  uTestTableCommands in 'source\tests\uTestTableCommands.pas',
  uTestExpressionsTextGenerator in 'source\tests\uTestExpressionsTextGenerator.pas',
  uTestCriteriaTextGenerator in 'source\tests\uTestCriteriaTextGenerator.pas',
  uTestGroupCriteriaTextGenerator in 'source\tests\uTestGroupCriteriaTextGenerator.pas',
  uTestConstraintsTextGenerator in 'source\tests\uTestConstraintsTextGenerator.pas',
  uTestCustomFieldOptionsTextGenerator in 'source\tests\uTestCustomFieldOptionsTextGenerator.pas',
  uTestFieldCommandTextGenerator in 'source\tests\uTestFieldCommandTextGenerator.pas',
  uTestSelectClauseTextGenerator in 'source\tests\uTestSelectClauseTextGenerator.pas',
  uTestTableCommandsTextGenerator in 'source\tests\uTestTableCommandsTextGenerator.pas',
  uTestSimpleSQLGenerator in 'source\tests\uTestSimpleSQLGenerator.pas',
  uTestColumnMappers in 'source\tests\uTestColumnMappers.pas',
  uTestObjectMapper in 'source\tests\uTestObjectMapper.pas',
  uTestPersistentCursor in 'source\tests\uTestPersistentCursor.pas',
  uTestObjectMapperToDDLEntity in 'source\tests\uTestObjectMapperToDDLEntity.pas',
  uTestSequenceCommands in 'source\tests\uTestSequenceCommands.pas',
  uTestSequenceTextGenerator in 'source\tests\uTestSequenceTextGenerator.pas',
  uTestIndexCommands in 'source\tests\uTestIndexCommands.pas',
  uTestIndexCommandsTextGenerator in 'source\tests\uTestIndexCommandsTextGenerator.pas',
  uTestSQLMapper in 'source\tests\uTestSQLMapper.pas',
  uTestDBPersistent in 'source\tests\uTestDBPersistent.pas',
  uTestFreedomObject in 'source\tests\uTestFreedomObject.pas',
  uTestFreedomObjectList in 'source\tests\uTestFreedomObjectList.pas',
  uTestSQLLinker in 'source\tests\uTestSQLLinker.pas',
  uTestfrmNewDBObjectEditor in 'source\tests\uTestfrmNewDBObjectEditor.pas',
  uTestFreedomObjectUnit in 'source\tests\uTestFreedomObjectUnit.pas',
  uTestNewListCriterias in 'source\tests\uTestNewListCriterias.pas',
  uTestFreedomObjectListToDataSet in 'source\tests\uTestFreedomObjectListToDataSet.pas',
  ufrmTestFreedomObjectListToDataSet in 'source\tests\ufrmTestFreedomObjectListToDataSet.pas' {frmTestFreedomObjectListToDataSet},
  ufrmTestFreedomObjectDataSet in 'source\tests\ufrmTestFreedomObjectDataSet.pas' {frmTestFreedomObjectDataSet},
  uTestFreedomObjectDataSet in 'source\tests\uTestFreedomObjectDataSet.pas',
  AM.Freedom.INamed in 'source\Interfaces\AM.Freedom.INamed.pas',
  AM.Freedom.IAliasable in 'source\Interfaces\AM.Freedom.IAliasable.pas',
  AM.Freedom.TextGenerator.ITextGenerator in 'source\Interfaces\AM.Freedom.TextGenerator.ITextGenerator.pas',
  AM.Freedom.CustomPersistent in 'source\CommomObjects\AM.Freedom.CustomPersistent.pas',
  AM.Freedom.CustomFreedomObject in 'source\CommomObjects\AM.Freedom.CustomFreedomObject.pas',
  AM.Freedom.FreedomObject in 'source\API\ObjectMappers\AM.Freedom.FreedomObject.pas',
  AM.Freedom.SQLMappers.IJoin in 'source\Interfaces\SQLMappers\AM.Freedom.SQLMappers.IJoin.pas',
  AM.Freedom.SQLMappers.ISelect in 'source\Interfaces\SQLMappers\AM.Freedom.SQLMappers.ISelect.pas',
  AM.Freedom.SQLMappers.ISQLClause in 'source\Interfaces\SQLMappers\AM.Freedom.SQLMappers.ISQLClause.pas',
  AM.Freedom.SQLMappers.ISQLMapper in 'source\Interfaces\SQLMappers\AM.Freedom.SQLMappers.ISQLMapper.pas',
  AM.Freedom.GroupCriteria.IComparator in 'source\Interfaces\GroupCriteria\AM.Freedom.GroupCriteria.IComparator.pas',
  AM.Freedom.GroupCriteria.ICriteria in 'source\Interfaces\GroupCriteria\AM.Freedom.GroupCriteria.ICriteria.pas',
  AM.Freedom.GroupCriteria.IGroupCriteria in 'source\Interfaces\GroupCriteria\AM.Freedom.GroupCriteria.IGroupCriteria.pas',
  AM.Freedom.TextGenerator.CustomTextGenerator in 'source\CommomObjects\AM.Freedom.TextGenerator.CustomTextGenerator.pas',
  AM.Freedom.SQLMapper.CustomArgument in 'source\CommomObjects\SQLMapers\AM.Freedom.SQLMapper.CustomArgument.pas',
  AM.Freedom.SQLMappers.AliasableObject in 'source\CommomObjects\SQLMapers\AM.Freedom.SQLMappers.AliasableObject.pas',
  AM.Freedom.SQLMappers.CustomSQLMapper in 'source\CommomObjects\SQLMapers\AM.Freedom.SQLMappers.CustomSQLMapper.pas',
  AM.Freedom.SQLMappers.NamedObject in 'source\CommomObjects\SQLMapers\AM.Freedom.SQLMappers.NamedObject.pas',
  AM.Freedom.SQLMappers.SQLClause in 'source\CommomObjects\SQLMapers\AM.Freedom.SQLMappers.SQLClause.pas',
  AM.Freedom.SQLMappers.SQLFieldList in 'source\CommomObjects\SQLMapers\AM.Freedom.SQLMappers.SQLFieldList.pas',
  AM.Freedom.EnumerationTypes in 'source\Commom\AM.Freedom.EnumerationTypes.pas',
  AM.Freedom.Helper.ComparatorType in 'source\Commom\Helpers\AM.Freedom.Helper.ComparatorType.pas',
  AM.Freedom.Helper.Policy in 'source\Commom\Helpers\AM.Freedom.Helper.Policy.pas',
  AM.Freedom.Helper.Variant in 'source\Commom\Helpers\AM.Freedom.Helper.Variant.pas',
  AM.Freedom.GroupCriteria.Comparators in 'source\API\GroupCriteria\AM.Freedom.GroupCriteria.Comparators.pas',
  AM.Freedom.GroupCriteria.Criteria in 'source\API\GroupCriteria\AM.Freedom.GroupCriteria.Criteria.pas',
  AM.Freedom.GroupCriteria in 'source\API\GroupCriteria\AM.Freedom.GroupCriteria.pas',
  AM.Freedom.GroupCriteria.ValueCriteria in 'source\API\GroupCriteria\AM.Freedom.GroupCriteria.ValueCriteria.pas',
  AM.Freedom.SQLMappers.Arguments in 'source\API\SQLMappers\AM.Freedom.SQLMappers.Arguments.pas',
  AM.Freedom.SQLMappers.Expressions in 'source\API\SQLMappers\AM.Freedom.SQLMappers.Expressions.pas',
  AM.Freedom.SQLMappers.FromClause in 'source\API\SQLMappers\AM.Freedom.SQLMappers.FromClause.pas',
  AM.Freedom.SQLMappers.GroupByClause in 'source\API\SQLMappers\AM.Freedom.SQLMappers.GroupByClause.pas',
  AM.Freedom.SQLMappers.JoinClause in 'source\API\SQLMappers\AM.Freedom.SQLMappers.JoinClause.pas',
  AM.Freedom.SQLMappers.OrderByClause in 'source\API\SQLMappers\AM.Freedom.SQLMappers.OrderByClause.pas',
  AM.Freedom.SQLMappers.SelectClause in 'source\API\SQLMappers\AM.Freedom.SQLMappers.SelectClause.pas',
  AM.Freedom.SQLMappers.WhereClause in 'source\API\SQLMappers\AM.Freedom.SQLMappers.WhereClause.pas',
  AM.Freedom.SQLMappers.MSSQLMapper in 'source\API\SQLMappers\DBMappers\AM.Freedom.SQLMappers.MSSQLMapper.pas',
  AM.Freedom.TextGenerator.CriteriaTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.CriteriaTextGenerator.pas',
  AM.Freedom.TextGenerator.FieldTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.FieldTextGenerator.pas',
  AM.Freedom.TextGenerator.GenerateTextParams in 'source\API\TextGenerator\AM.Freedom.TextGenerator.GenerateTextParams.pas',
  AM.Freedom.TextGenerator.GroupCriteriaTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.GroupCriteriaTextGenerator.pas',
  AM.Freedom.TextGenerator.TableArgumentTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.TableArgumentTextGenerator.pas',
  AM.Freedom.TextGeneratorRegister in 'source\CommomObjects\AM.Freedom.TextGeneratorRegister.pas',
  AM.Freedom.TextGenerator.CustomSelectTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.CustomSelectTextGenerator.pas',
  AM.Freedom.SQLMappers.HavingClause in 'source\API\SQLMappers\AM.Freedom.SQLMappers.HavingClause.pas',
  AM.Freedom.SQLMappers.CustomExpression in 'source\CommomObjects\AM.Freedom.SQLMappers.CustomExpression.pas',
  AM.Freedom.TextGenerator.CustomExpressionTextGenerator in 'source\CommomObjects\AM.Freedom.TextGenerator.CustomExpressionTextGenerator.pas',
  AM.Freedom.TextGenerator.ExpressionsTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.ExpressionsTextGenerator.pas',
  AM.Freedom.TextGenerator.CustomClauseTextGenerator in 'source\CommomObjects\AM.Freedom.TextGenerator.CustomClauseTextGenerator.pas',
  AM.Freedom.TextGenerator.FromClauseTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.FromClauseTextGenerator.pas',
  AM.Freedom.TextGenerator.OrderByClauseTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.OrderByClauseTextGenerator.pas',
  AM.Freedom.TextGenerator.GroupByClauseTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.GroupByClauseTextGenerator.pas',
  AM.Freedom.TextGenerator.JoinClauseTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.JoinClauseTextGenerator.pas',
  AM.Freedom.Helper.JoinKind in 'source\Commom\Helpers\AM.Freedom.Helper.JoinKind.pas',
  AM.Freedom.TextGenerator.SimpleSelectTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.SimpleSelectTextGenerator.pas',
  AM.Freedom.SQLGenerator.SimpleSQLGenerator in 'source\API\SQLGenerator\AM.Freedom.SQLGenerator.SimpleSQLGenerator.pas',
  AM.Freedom.TextGenerator.AbstractTextGenerator in 'source\CommomObjects\AM.Freedom.TextGenerator.AbstractTextGenerator.pas',
  AM.Freedom.SQLMappers.ExistsArgument in 'source\API\SQLMappers\AM.Freedom.SQLMappers.ExistsArgument.pas',
  AM.Freedom.TextGenerator.BetweenArgumentTextGenerator in 'source\API\TextGenerator\Arguments\AM.Freedom.TextGenerator.BetweenArgumentTextGenerator.pas',
  AM.Freedom.TextGenerator.CustomLiteralArgumentTextGenerator in 'source\API\TextGenerator\Arguments\AM.Freedom.TextGenerator.CustomLiteralArgumentTextGenerator.pas',
  AM.Freedom.TextGenerator.InArgumentTextGenerator in 'source\API\TextGenerator\Arguments\AM.Freedom.TextGenerator.InArgumentTextGenerator.pas',
  AM.Freedom.TextGenerator.ValueArgumentTextGenerator in 'source\API\TextGenerator\Arguments\AM.Freedom.TextGenerator.ValueArgumentTextGenerator.pas',
  AM.Freedom.Exceptions in 'source\CommomObjects\AM.Freedom.Exceptions.pas',
  AM.Freedom.Helper.CalcExpressionType in 'source\Commom\Helpers\AM.Freedom.Helper.CalcExpressionType.pas',
  AM.Freedom.SQLMappers.ISQLFormatter in 'source\Interfaces\SQLMappers\AM.Freedom.SQLMappers.ISQLFormatter.pas',
  AM.Freedom.SQLMappers.SQLFormatter in 'source\API\SQLMappers\AM.Freedom.SQLMappers.SQLFormatter.pas',
  AM.Freedom.Consts in 'source\Commom\AM.Freedom.Consts.pas',
  AM.Freedom.ClassFactory in 'source\CommomObjects\AM.Freedom.ClassFactory.pas',
  AM.Freedom.SQLCommands.TableCommands in 'source\API\SQLCommands\AM.Freedom.SQLCommands.TableCommands.pas',
  AM.Freedom.SQLCommands.Constraints in 'source\API\SQLCommands\Commom\AM.Freedom.SQLCommands.Constraints.pas',
  AM.Freedom.SQLCommands.Fields in 'source\API\SQLCommands\Commom\AM.Freedom.SQLCommands.Fields.pas',
  AM.Freedom.SQLCommands.ScaledFieldCommand in 'source\API\SQLCommands\Commom\AM.Freedom.SQLCommands.ScaledFieldCommand.pas',
  AM.Freedom.SQLCommands.SizedFieldCommand in 'source\API\SQLCommands\Commom\AM.Freedom.SQLCommands.SizedFieldCommand.pas',
  AM.Freedom.SQLCommands.CustomFieldCommand in 'source\CommomObjects\AM.Freedom.SQLCommands.CustomFieldCommand.pas',
  AM.Freedom.SQLCommands.TableFieldCommands in 'source\API\SQLCommands\AM.Freedom.SQLCommands.TableFieldCommands.pas',
  AM.Freedom.SQLCommands.CustomTableCommand in 'source\CommomObjects\AM.Freedom.SQLCommands.CustomTableCommand.pas',
  AM.Freedom.GeneratorTextList in 'source\CommomObjects\AM.Freedom.GeneratorTextList.pas',
  AM.Freedom.TextGenerator.CustomFieldCommandTextGenerator in 'source\CommomObjects\AM.Freedom.TextGenerator.CustomFieldCommandTextGenerator.pas',
  AM.Freedom.TextGenerator.FieldCommandTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.FieldCommandTextGenerator.pas',
  AM.Freedom.TextGenerator.CustomConstraintTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.CustomConstraintTextGenerator.pas',
  AM.Freedom.TextGenerator.ForeignKeyTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.ForeignKeyTextGenerator.pas',
  AM.Freedom.Helper.ForeignOption in 'source\Commom\Helpers\AM.Freedom.Helper.ForeignOption.pas',
  AM.Freedom.Helper.ConstraintType in 'source\Commom\Helpers\AM.Freedom.Helper.ConstraintType.pas',
  AM.Freedom.Helper.CommandType in 'source\Commom\Helpers\AM.Freedom.Helper.CommandType.pas',
  AM.Freedom.TextGenerator.CustomTableCommandsTextGenerator in 'source\CommomObjects\AM.Freedom.TextGenerator.CustomTableCommandsTextGenerator.pas',
  AM.Freedom.TextGenerator.TableCommandsTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.TableCommandsTextGenerator.pas',
  AM.Freedom.SQLCommands.TableRowCommands in 'source\API\SQLCommands\AM.Freedom.SQLCommands.TableRowCommands.pas',
  AM.Freedom.TextGenerator.MasterTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.MasterTextGenerator.pas',
  AM.Freedom.Attributes in 'source\API\ObjectMappers\AM.Freedom.Attributes.pas',
  AM.Freedom.ITrigger in 'source\Interfaces\AM.Freedom.ITrigger.pas',
  AM.Freedom.ObjectMapper.CustomTrigger in 'source\CommomObjects\ObjectMapper\AM.Freedom.ObjectMapper.CustomTrigger.pas',
  AM.Freedom.ObjectMapper.TriggerMapper in 'source\API\ObjectMappers\AM.Freedom.ObjectMapper.TriggerMapper.pas',
  AM.Freedom.IMethodControl in 'source\Interfaces\AM.Freedom.IMethodControl.pas',
  AM.Freedom.ObjectMapper.CustomMethodControl in 'source\CommomObjects\ObjectMapper\AM.Freedom.ObjectMapper.CustomMethodControl.pas',
  AM.Freedom.ObjectMapper.MethodMapper in 'source\API\ObjectMappers\AM.Freedom.ObjectMapper.MethodMapper.pas',
  AM.Freedom.TextGenerator.CustomFieldOptionsTextGenerator in 'source\CommomObjects\AM.Freedom.TextGenerator.CustomFieldOptionsTextGenerator.pas',
  AM.Freedom.SQLCommands.MSSQLFields in 'source\API\SQLCommands\Commom\AM.Freedom.SQLCommands.MSSQLFields.pas',
  AM.Freedom.SQLCommands.FBFields in 'source\API\SQLCommands\Commom\AM.Freedom.SQLCommands.FBFields.pas',
  AM.Freedom.SQLMappers.FBSQLMapper in 'source\API\SQLMappers\DBMappers\AM.Freedom.SQLMappers.FBSQLMapper.pas',
  AM.Freedom.ObjectMapper.CustomMapper in 'source\CommomObjects\ObjectMapper\AM.Freedom.ObjectMapper.CustomMapper.pas',
  AM.Freedom.ObjectMapper.CustomColumnMapper in 'source\API\ObjectMappers\AM.Freedom.ObjectMapper.CustomColumnMapper.pas',
  AM.Freedom.ObjectMapper.CustomColumnReader in 'source\API\ObjectMappers\AM.Freedom.ObjectMapper.CustomColumnReader.pas',
  AM.Freedom.Helper.RttiType in 'source\Commom\Helpers\AM.Freedom.Helper.RttiType.pas',
  AM.Freedom.ObjectMapper.ObjectToMapper in 'source\API\ObjectMappers\Readers\AM.Freedom.ObjectMapper.ObjectToMapper.pas',
  AM.Freedom.ObjectMapper in 'source\API\ObjectMappers\AM.Freedom.ObjectMapper.pas',
  AM.Freedom.Helper.ColumnType in 'source\Commom\Helpers\AM.Freedom.Helper.ColumnType.pas',
  AM.Freedom.ColumnReaderRegister in 'source\CommomObjects\AM.Freedom.ColumnReaderRegister.pas',
  AM.Freedom.ColumnMappersList in 'source\CommomObjects\AM.Freedom.ColumnMappersList.pas',
  AM.Freedom.ObjectMapper.ColumnReaders in 'source\API\ObjectMappers\Readers\AM.Freedom.ObjectMapper.ColumnReaders.pas',
  AM.Freedom.ObjectMapper.ColumnMappers in 'source\API\ObjectMappers\AM.Freedom.ObjectMapper.ColumnMappers.pas',
  AM.Freedom.DefaultsClassRegister in 'source\CommomObjects\AM.Freedom.DefaultsClassRegister.pas',
  AM.Freedom.ObjectMapper.CustomAutoMappingValueGetter in 'source\CommomObjects\ObjectMapper\AM.Freedom.ObjectMapper.CustomAutoMappingValueGetter.pas',
  AM.Freedom.ILazy in 'source\Interfaces\AM.Freedom.ILazy.pas',
  AM.Freedom.Lazy in 'source\API\ObjectMappers\AM.Freedom.Lazy.pas',
  AM.Freedom.Persistent.Cursor in 'source\CommomObjects\AM.Freedom.Persistent.Cursor.pas',
  AM.Freedom.MasterMethodControl in 'source\CommomObjects\AM.Freedom.MasterMethodControl.pas',
  AM.Freedom.ObjectMapper.ColumnOptions in 'source\CommomObjects\ObjectMapper\AM.Freedom.ObjectMapper.ColumnOptions.pas',
  AM.Freedom.Helper.RttiField in 'source\Commom\Helpers\AM.Freedom.Helper.RttiField.pas',
  AM.Freedom.IFreedomObject in 'source\Interfaces\AM.Freedom.IFreedomObject.pas',
  AM.Freedom.IFreedomObjectList in 'source\Interfaces\AM.Freedom.IFreedomObjectList.pas',
  AM.Freedom.IPersistent in 'source\Interfaces\AM.Freedom.IPersistent.pas',
  AM.Freedom.FreedomAttributes in 'source\API\ObjectMappers\AM.Freedom.FreedomAttributes.pas',
  AM.Freedom.InterfacedObjects in 'source\CommomObjects\AM.Freedom.InterfacedObjects.pas',
  AM.Freedom.ObjectMapper.MapperToObject in 'source\API\ObjectMappers\Readers\AM.Freedom.ObjectMapper.MapperToObject.pas',
  AM.Freedom.IDBPersistent in 'source\Interfaces\AM.Freedom.IDBPersistent.pas',
  AM.Freedom.CustomDBPersistent in 'source\CommomObjects\AM.Freedom.CustomDBPersistent.pas',
  AM.Freedom.ObjectMapper.MapperToSelectClause in 'source\API\ObjectMappers\Readers\AM.Freedom.ObjectMapper.MapperToSelectClause.pas',
  AM.Freedom.CustomFreedomObjectList in 'source\CommomObjects\AM.Freedom.CustomFreedomObjectList.pas',
  AM.Freedom.FreedomObjectList in 'source\API\ObjectMappers\AM.Freedom.FreedomObjectList.pas',
  AM.Freedom.LazyList in 'source\API\ObjectMappers\AM.Freedom.LazyList.pas',
  AM.Freedom.ObjectMapper.CustomColumn in 'source\CommomObjects\ObjectMapper\AM.Freedom.ObjectMapper.CustomColumn.pas',
  AM.Freedom.ObjectMapper.DDLObjects in 'source\API\ObjectMappers\AM.Freedom.ObjectMapper.DDLObjects.pas',
  AM.Freedom.ObjectMapperToDDLEntity in 'source\API\ObjectMappers\AM.Freedom.ObjectMapperToDDLEntity.pas',
  AM.Freedom.ObjectMapper.DDLCompare in 'source\API\ObjectMappers\AM.Freedom.ObjectMapper.DDLCompare.pas',
  AM.Freedom.ObjectMapper.DDLColumnCompare in 'source\API\ObjectMappers\AM.Freedom.ObjectMapper.DDLColumnCompare.pas',
  AM.Freedom.FieldCommandFactory in 'source\CommomObjects\AM.Freedom.FieldCommandFactory.pas',
  AM.Freedom.SQLMappers.IDDLExtracter in 'source\Interfaces\SQLMappers\AM.Freedom.SQLMappers.IDDLExtracter.pas',
  AM.Freedom.SQLMappers.CustomDDLExtracter in 'source\CommomObjects\SQLMapers\AM.Freedom.SQLMappers.CustomDDLExtracter.pas',
  AM.Freedom.ObjectMapper.FBDDLExtracter in 'source\API\ObjectMappers\DDLExtracters\AM.Freedom.ObjectMapper.FBDDLExtracter.pas',
  AM.Freedom.SQLMappers.FBExpressions in 'source\API\SQLMappers\AM.Freedom.SQLMappers.FBExpressions.pas',
  AM.Freedom.TextGenerator.FBExpressionsTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.FBExpressionsTextGenerator.pas',
  AM.Freedom.ObjectMapper.DDLConstraintCompare in 'source\API\ObjectMappers\AM.Freedom.ObjectMapper.DDLConstraintCompare.pas',
  AM.Freedom.SQLCommands.SequenceCommands in 'source\API\SQLCommands\AM.Freedom.SQLCommands.SequenceCommands.pas',
  AM.Freedom.TextGenerator.SequenceCommandsTextGenerators in 'source\API\TextGenerator\AM.Freedom.TextGenerator.SequenceCommandsTextGenerators.pas',
  AM.Freedom.Persistent.SetCursorResult in 'source\CommomObjects\AM.Freedom.Persistent.SetCursorResult.pas',
  AM.Freedom.Persistent.DataSnapPersistent in 'source\API\Persistents\AM.Freedom.Persistent.DataSnapPersistent.pas',
  AM.Freedom.ValueArgumentFactory in 'source\CommomObjects\AM.Freedom.ValueArgumentFactory.pas',
  AM.Freedom.DBPersistent.DBParam in 'source\API\Persistents\DBPersistents\AM.Freedom.DBPersistent.DBParam.pas',
  AM.Freedom.DBPersistent.IDBConnector in 'source\Interfaces\AM.Freedom.DBPersistent.IDBConnector.pas',
  AM.Freedom.Persistent.CustomDBConnector in 'source\CommomObjects\AM.Freedom.Persistent.CustomDBConnector.pas',
  AM.Freedom.Persistent.CustomDBStatement in 'source\CommomObjects\AM.Freedom.Persistent.CustomDBStatement.pas',
  AM.Freedom.Helper.FieldType in 'source\Commom\Helpers\AM.Freedom.Helper.FieldType.pas',
  AM.Freedom.SQLMappers.SQLLinker in 'source\API\SQLMappers\AM.Freedom.SQLMappers.SQLLinker.pas',
  AM.Freedom.SQLMappers.CustomSelect in 'source\CommomObjects\SQLMapers\AM.Freedom.SQLMappers.CustomSelect.pas',
  AM.Freedom.ObjectMapper.MSSQLDDLExtracter in 'source\API\ObjectMappers\DDLExtracters\AM.Freedom.ObjectMapper.MSSQLDDLExtracter.pas',
  AM.Freedom.ObjectMapper.ConstraintMapper in 'source\API\ObjectMappers\AM.Freedom.ObjectMapper.ConstraintMapper.pas',
  AM.Freedom.SQLMappers.PGSQLMapper in 'source\API\SQLMappers\DBMappers\AM.Freedom.SQLMappers.PGSQLMapper.pas',
  AM.Freedom.ObjectMapper.PGDDLExtracter in 'source\API\ObjectMappers\DDLExtracters\AM.Freedom.ObjectMapper.PGDDLExtracter.pas',
  AM.Freedom.SQLCommands.SchemaCommands in 'source\API\SQLCommands\AM.Freedom.SQLCommands.SchemaCommands.pas',
  AM.Freedom.SQLCommands.DomainCommands in 'source\API\SQLCommands\AM.Freedom.SQLCommands.DomainCommands.pas',
  AM.Freedom.SQLCommands.FieldOptions in 'source\API\SQLCommands\Commom\AM.Freedom.SQLCommands.FieldOptions.pas',
  AM.Freedom.TextGenerator.DomainTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.DomainTextGenerator.pas',
  AM.Freedom.ObjectMapper.Schemas in 'source\CommomObjects\ObjectMapper\AM.Freedom.ObjectMapper.Schemas.pas',
  AM.Freedom.ObjectMapper.ObjectMapperToSchemaMapper in 'source\API\ObjectMappers\Readers\AM.Freedom.ObjectMapper.ObjectMapperToSchemaMapper.pas',
  AM.Freedom.TextGenerator.SchemaCommandsTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.SchemaCommandsTextGenerator.pas',
  AM.Freedom.DBPersistent.DBExpress in 'source\API\Persistents\DBPersistents\AM.Freedom.DBPersistent.DBExpress.pas',
  AM.Freedom.DBPersistent.FireDac in 'source\API\Persistents\DBPersistents\AM.Freedom.DBPersistent.FireDac.pas',
  AM.Freedom.DBPersistent.FireDac.PropertyNames in 'source\API\Persistents\DBPersistents\AM.Freedom.DBPersistent.FireDac.PropertyNames.pas',
  AM.Freedom.Persistent.FBPersistent in 'source\API\Persistents\DBPersistents\AM.Freedom.Persistent.FBPersistent.pas',
  AM.Freedom.Persistent.MSSQLPersistent in 'source\API\Persistents\DBPersistents\AM.Freedom.Persistent.MSSQLPersistent.pas',
  AM.Freedom.Persistent.PGPersistent in 'source\API\Persistents\DBPersistents\AM.Freedom.Persistent.PGPersistent.pas',
  AM.Freedom.SQLMappers.SQLFormatSettings in 'source\API\SQLMappers\AM.Freedom.SQLMappers.SQLFormatSettings.pas',
  AM.Freedom.Helper.TStrings in 'source\Commom\Helpers\AM.Freedom.Helper.TStrings.pas',
  AM.Freedom.CustomNullable in 'source\CommomObjects\AM.Freedom.CustomNullable.pas',
  AM.Freedom.dmConnection in 'source\API\FreedomObjectUnit\AM.Freedom.dmConnection.pas' {dmConnection: TDataModule},
  AM.Freedom.frmBase in 'source\API\FreedomObjectUnit\AM.Freedom.frmBase.pas' {frmBase},
  AM.Freedom.frmNewClass in 'source\API\FreedomObjectUnit\AM.Freedom.frmNewClass.pas' {frmNewClass},
  AM.Freedom.frmNewDBObjectEditor in 'source\API\FreedomObjectUnit\AM.Freedom.frmNewDBObjectEditor.pas' {frmNewDBObjectEditor},
  AM.Freedom.frmNewProperty in 'source\API\FreedomObjectUnit\AM.Freedom.frmNewProperty.pas' {frmNewProperty},
  AM.Freedom.frmNewUnit in 'source\API\FreedomObjectUnit\AM.Freedom.frmNewUnit.pas' {frmNewUnit},
  AM.Freedom.frmOptions in 'source\API\FreedomObjectUnit\AM.Freedom.frmOptions.pas' {frmOptions},
  AM.Freedom.FreedomObjectDescriptor in 'source\API\FreedomObjectUnit\AM.Freedom.FreedomObjectDescriptor.pas',
  AM.Freedom.TableSchemas in 'source\API\FreedomObjectUnit\AM.Freedom.TableSchemas.pas',
  AM.Freedom.frmNewDBProject in 'source\API\FreedomObjectUnit\AM.Freedom.frmNewDBProject.pas' {frmNewDBProject},
  AM.Freedom.Helpers.WinControl in 'source\API\FreedomObjectUnit\Helpers\AM.Freedom.Helpers.WinControl.pas',
  AM.Freedom.Helper.OrderType in 'source\Commom\Helpers\AM.Freedom.Helper.OrderType.pas',
  AM.Freedom.Persistent.CustomDBQuery in 'source\CommomObjects\AM.Freedom.Persistent.CustomDBQuery.pas',
  AM.Freedom.ICursor in 'source\Interfaces\AM.Freedom.ICursor.pas',
  AM.Freedom.NullableCompare in 'source\CommomObjects\AM.Freedom.NullableCompare.pas',
  AM.Freedom.TextGenerator.MSSQLExpressionsTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.MSSQLExpressionsTextGenerator.pas',
  AM.Freedom.SQLMappers.MSSQLExpressions in 'source\API\SQLMappers\AM.Freedom.SQLMappers.MSSQLExpressions.pas',
  AM.Freedom.frmNewListCriteria in 'source\API\GroupCriteria\Editor\AM.Freedom.frmNewListCriteria.pas' {frmNewListCriteria},
  AM.Freedom.frmNewGroupCriteria in 'source\API\GroupCriteria\Editor\AM.Freedom.frmNewGroupCriteria.pas' {frmNewGroupCriteria},
  AM.Freedom.GroupCriteriaDescriptor in 'source\API\GroupCriteria\Editor\AM.Freedom.GroupCriteriaDescriptor.pas',
  AM.Freedom.frmClassUnitName in 'source\API\FreedomObjectUnit\AM.Freedom.frmClassUnitName.pas' {frmClassUnitName},
  AM.Freedom.frmNewCriteria in 'source\API\GroupCriteria\Editor\AM.Freedom.frmNewCriteria.pas' {frmNewCriteria},
  AM.Freedom.frmBaseCriteria in 'source\API\GroupCriteria\Editor\AM.Freedom.frmBaseCriteria.pas' {frmBaseCriteria},
  AM.Freedom.frmBaseArgumentParameter in 'source\API\GroupCriteria\Editor\AM.Freedom.frmBaseArgumentParameter.pas' {frmBaseArgumentParameter},
  AM.Freedom.frmNewArgument in 'source\API\GroupCriteria\Editor\AM.Freedom.frmNewArgument.pas' {frmNewArgument},
  AM.Freedom.ArgumentDesignerRegister in 'source\API\GroupCriteria\Editor\AM.Freedom.ArgumentDesignerRegister.pas',
  AM.Freedom.SQLCommands.IndexCommands in 'source\API\SQLCommands\AM.Freedom.SQLCommands.IndexCommands.pas',
  AM.Freedom.TextGenerator.IndexCommandsTextGenerator in 'source\API\TextGenerator\AM.Freedom.TextGenerator.IndexCommandsTextGenerator.pas',
  AM.Freedom.Helper.NullOption in 'source\Commom\Helpers\AM.Freedom.Helper.NullOption.pas',
  AM.Freedom.Helper.IndexOption in 'source\Commom\Helpers\AM.Freedom.Helper.IndexOption.pas',
  AM.Freedom.IndexColumn in 'source\CommomObjects\AM.Freedom.IndexColumn.pas',
  AM.Freedom.frmNewForeign in 'source\API\FreedomObjectUnit\AM.Freedom.frmNewForeign.pas' {frmNewForeign},
  AM.Freedom.ObjectMapper.LiveRefresh in 'source\CommomObjects\ObjectMapper\AM.Freedom.ObjectMapper.LiveRefresh.pas',
  AM.Freedom.FreedomObjectListToDataSet in 'source\API\ObjectDataSet\AM.Freedom.FreedomObjectListToDataSet.pas',
  AM.Freedom.FreedomObjectDataSet in 'source\API\ObjectDataSet\AM.Freedom.FreedomObjectDataSet.pas',
  AM.Freedom.Nullable in 'source\API\ObjectMappers\AM.Freedom.Nullable.pas',
  AM.Freedom.INullable in 'source\Interfaces\AM.Freedom.INullable.pas',
  AM.Freedom.Helper.RttiProperty in 'source\Commom\Helpers\AM.Freedom.Helper.RttiProperty.pas',
  AM.Freedom.BooleanValueOptions in 'source\CommomObjects\AM.Freedom.BooleanValueOptions.pas',
  AM.Freedom.Nullable.Types in 'source\API\ObjectMappers\AM.Freedom.Nullable.Types.pas',
  AM.Freedom.CustomRounder in 'source\CommomObjects\AM.Freedom.CustomRounder.pas',
  AM.Freedom.RoundObject in 'source\API\ObjectMappers\AM.Freedom.RoundObject.pas',
  AM.Freedom.ObjectMapper.ColumnsList in 'source\API\ObjectMappers\AM.Freedom.ObjectMapper.ColumnsList.pas',
  AM.Freedom.ObjectMapper.ObjectMappingExplorer in 'source\API\ObjectMappers\AM.Freedom.ObjectMapper.ObjectMappingExplorer.pas',
  AM.Freedom.ObjectMapper.ColumnValueItem in 'source\API\ObjectMappers\AM.Freedom.ObjectMapper.ColumnValueItem.pas',
  AM.Freedom.ObjectMapper.ObjectWriter in 'source\API\ObjectMappers\Readers\AM.Freedom.ObjectMapper.ObjectWriter.pas',
  AM.Freedom.frmRenameProperty in 'source\API\FreedomObjectUnit\AM.Freedom.frmRenameProperty.pas' {frmRenameProperty},
  uTestPermissao in 'source\tests\uTestPermissao.pas',
  AM.Freedom.ObjectMapper.ObjectToMapperAssigner in 'source\API\ObjectMappers\Readers\AM.Freedom.ObjectMapper.ObjectToMapperAssigner.pas',
  AM.Freedom.ObjectFactory in 'source\API\ObjectMappers\AM.Freedom.ObjectFactory.pas',
  AM.Freedom.FreedomObjectConstructor in 'source\CommomObjects\ObjectMapper\Factorys\AM.Freedom.FreedomObjectConstructor.pas',
  AM.Freedom.FreedomObjectsFactory in 'source\API\ObjectMappers\AM.Freedom.FreedomObjectsFactory.pas',
  AM.Freedom.StringsConstructor in 'source\CommomObjects\ObjectMapper\Factorys\AM.Freedom.StringsConstructor.pas',
  AM.Freedom.StreamsConstructor in 'source\CommomObjects\ObjectMapper\Factorys\AM.Freedom.StreamsConstructor.pas',
  AM.Freedom.FindPropertyItem in 'source\CommomObjects\AM.Freedom.FindPropertyItem.pas',
  AM.Freedom.GroupFilterCriteria in 'source\API\GroupCriteria\AM.Freedom.GroupFilterCriteria.pas',
  AM.Freedom.GroupFilterCriteria.FilterCriteria in 'source\API\GroupCriteria\AM.Freedom.GroupFilterCriteria.FilterCriteria.pas',
  AM.Freedom.GroupFilterCriteria.IFilterCriteria in 'source\Interfaces\GroupCriteria\AM.Freedom.GroupFilterCriteria.IFilterCriteria.pas',
  AM.Freedom.FilteredItem in 'source\API\GroupCriteria\AM.Freedom.FilteredItem.pas',
  AM.Freedom.ObjectPropertyValueFinder in 'source\CommomObjects\AM.Freedom.ObjectPropertyValueFinder.pas',
  uTestObjectMapperToSelectClause in 'source\tests\uTestObjectMapperToSelectClause.pas',
  AM.Freedom.ObjectMapper.CustomObjectCursor in 'source\CommomObjects\ObjectMapper\AM.Freedom.ObjectMapper.CustomObjectCursor.pas',
  AM.Freedom.ObjectMapper.CustomDBObjectCursor in 'source\CommomObjects\ObjectMapper\AM.Freedom.ObjectMapper.CustomDBObjectCursor.pas',
  AM.Freedom.IXML in 'source\Interfaces\AM.Freedom.IXML.pas',
  AM.Freedom.XML in 'source\API\ObjectMappers\AM.Freedom.XML.pas',
  AM.Freedom.ColumnTypeDBConverter in 'source\CommomObjects\AM.Freedom.ColumnTypeDBConverter.pas',
  AM.Freedom.RegisterUserFieldCommandToString in 'source\CommomObjects\AM.Freedom.RegisterUserFieldCommandToString.pas',
  AM.Freedom.LiveRefreshObject in 'source\CommomObjects\AM.Freedom.LiveRefreshObject.pas',
  AM.Freedom.IJSONObject in 'source\Interfaces\AM.Freedom.IJSONObject.pas',
  AM.Freedom.JSONFreedomObject in 'source\CommomObjects\AM.Freedom.JSONFreedomObject.pas',
  TestJSONFreedomObject in 'source\tests\TestJSONFreedomObject.pas',
  TestComplexJSONFreedomObject in 'source\tests\TestComplexJSONFreedomObject.pas',
  TestBlobJSONFreedomObject in 'source\tests\TestBlobJSONFreedomObject.pas',
  TestStreamJSONFreedomObject in 'source\tests\TestStreamJSONFreedomObject.pas',
  uTestStringsJSONFreedomObject in 'source\tests\uTestStringsJSONFreedomObject.pas',
  uTestJSONTFreedomObject in 'source\tests\uTestJSONTFreedomObject.pas',
  AM.Freedom.dclFreedomORMConfig in 'source\API\Designers\AM.Freedom.dclFreedomORMConfig.pas',
  AM.Freedom.CustomFreedomConfig in 'source\API\Designers\AM.Freedom.CustomFreedomConfig.pas',
  AM.Freedom.Persistent.CustomDSCursor in 'source\CommomObjects\AM.Freedom.Persistent.CustomDSCursor.pas',
  AM.Freedom.Persistent.IDSConnector in 'source\Interfaces\AM.Freedom.Persistent.IDSConnector.pas',
  AM.Freedom.DSPersistent.FireDac in 'source\API\Persistents\DSPersistents\AM.Freedom.DSPersistent.FireDac.pas',
  AM.Freedom.FreedomObjectCloner in 'source\CommomObjects\AM.Freedom.FreedomObjectCloner.pas',
  AM.Freedom.INotificator in 'source\Interfaces\AM.Freedom.INotificator.pas',
  AM.Freedom.JSONAttributes in 'source\API\JSONReflect\AM.Freedom.JSONAttributes.pas',
  uObjectsTestsJSON in 'source\tests\uObjectsTestsJSON.pas',
  AM.Freedom.JSONConverter in 'source\API\JSONReflect\AM.Freedom.JSONConverter.pas',
  AM.Freedom.JSONConsts in 'source\API\JSONReflect\AM.Freedom.JSONConsts.pas',
  uTestObjectsTestsJSON in 'source\tests\uTestObjectsTestsJSON.pas',
  AM.Freedom.JSONUtils in 'source\API\JSONReflect\AM.Freedom.JSONUtils.pas',
  AM.Freedom.JSONObjects in 'source\API\JSONReflect\AM.Freedom.JSONObjects.pas',
  AM.Freedom.JSONValueFactory in 'source\API\JSONReflect\AM.Freedom.JSONValueFactory.pas',
  AM.Freedom.JSONClassConverterRegister in 'source\API\JSONReflect\AM.Freedom.JSONClassConverterRegister.pas',
  AM.Freedom.LazyJSONConverter in 'source\API\JSONReflect\AM.Freedom.LazyJSONConverter.pas',
  AM.Freedom.JSONStreamConverter in 'source\API\JSONReflect\AM.Freedom.JSONStreamConverter.pas',
  AM.Freedom.JSONStringListConverter in 'source\API\JSONReflect\AM.Freedom.JSONStringListConverter.pas';

var
  DBPersistent: TCustomDBPersistent;
  Connection: TFDConnection;

function GetDBType: TConnectorType;
begin
  {$IFDEF FIREBIRD}
  Result := Firebird;
  {$IFEND}
  {$IFDEF MSSQL}
  Result := SQLServer;
  {$IFEND}
  {$IFDEF POSTGRE}
  Result := PostGree;
  {$IFEND}
end;

function IsMSSQL: Boolean;
begin
  Result := GetDBType = SQLServer;
end;

function IsPostGree: Boolean;
begin
  Result := GetDBType = PostGree;
end;

function IsFireBird: Boolean;
begin
  Result := GetDBType = Firebird;
end;

function OnGetObjectStateFromSchemaControl(pCurrentObjectState: TObjectState; pDefaultSchemaName, pCurrentSchemaName: String): TObjectState;
begin
  Result := pCurrentObjectState;
  if (not IsFireBird) then
  begin
    if pCurrentSchemaName <> pDefaultSchemaName then
    begin
      Result := TObjectState.Inserted;
    end;
  end;
end;

procedure DoSetFireBirdParams;
begin
  Connection.DriverName := 'FB';
  Connection.Params.Values[TFDPropertyNames.User_Name] := 'SYSDBA';
  Connection.Params.Values[TFDPropertyNames.Password] := 'masterkey';
  Connection.Params.Values[TFDPropertyNames.Database] := 'D:\Alex\Delphi\FreedomORM\branches\source\tests\FDB\FREEDOMORM.FDB';
  Connection.Params.Values[TFDPropertyNames.Server] := 'localhost';
  Connection.Params.Values[TFDPropertyNames.Port] := '3050';
  Connection.Params.Values[TFDPropertyNames.CharacterSet] := 'WIN1252';
  Connection.Params.Values[TFDPropertyNames.PageSize] := '16384';
end;

procedure DoSetMSSQLParams;
begin
  Connection.DriverName := 'MSSQL';
  Connection.Params.Values[TFDPropertyNames.Database] := 'alexandro';
  Connection.Params.Values[TFDPropertyNames.Server] := '.';
  Connection.Params.Values[TFDPropertyNames.OSAuthent] := 'Yes';
end;

procedure DoSetPGParams;
begin
  Connection.DriverName := 'PG';
  Connection.Params.Values[TFDPropertyNames.Database] := 'alexandro';
  Connection.Params.Values[TFDPropertyNames.Server] := 'localhost';
  Connection.Params.Values[TFDPropertyNames.CharacterSet] := 'WIN1252';
  Connection.Params.Values[TFDPropertyNames.User_Name] := 'postgres';
  Connection.Params.Values[TFDPropertyNames.Password] := 'masterkey';
  Connection.Params.Values[TFDPropertyNames.Port] := '5432';
end;

function CreateDBConnector: TFireDacDBConnector;
begin
  Result := TFireDacDBConnector.Create(Connection);
  Result.UseTransactions := not IsMSSQL;
  Result.DBLogTypes := TConsts.cDBLogTypeAll;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  Connection := TFDConnection.Create(nil);
  DBPersistent := nil;
  if (IsMSSQL) then
  begin
    DoSetMSSQLParams;
    DBPersistent := TMSSQLPersistent.Create(CreateDBConnector, True);
    TMSSQLPersistent(DBPersistent).SelectOptions.WithNoLock := True;
  end
  else if (IsPostGree) then
  begin
    DoSetPGParams;
    DBPersistent := TPGPersistent.Create(CreateDBConnector, True);
  end
  else if (IsFireBird) then
  begin
    DoSetFireBirdParams;
    DBPersistent := TFBPersistent.Create(CreateDBConnector, True);
  end;
  // DBPersistent.UpdateObjectMode := uomOnPersist;
  DBPersistent.OnGetObjectStateFromSchemaControl := OnGetObjectStateFromSchemaControl;
  DBPersistent.MakeForeignConstraintsWithJoinColumn := False;
  DUnitTestRunner.RunRegisteredTests;
  DBPersistent.Free;
end.
