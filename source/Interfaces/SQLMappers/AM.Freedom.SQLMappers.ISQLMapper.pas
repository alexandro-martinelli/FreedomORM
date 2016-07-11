unit AM.Freedom.SQLMappers.ISQLMapper;

interface

uses
  System.Generics.Collections,
  AM.Freedom.Helper.Variant,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLCommands.DomainCommands,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLMappers.SQLFormatSettings;

type
  TOnGetUserFieldClassTypeToString = function(pClassType: TClass): String of Object;
  TOnGetUserColumnTypeToFieldCommandClass = function (pColumnType: TColumnType): TFieldCommandClass of object;

  ISQLMapper = interface
    ['{2CE3DD49-EF75-4B03-A750-5E5B5760311D}']
    procedure IncludeLimitRows(var pSQL: String; const pLimitRows: Integer);
    procedure FormatSQLText(var pSQL: String);
    function FieldClassTypeToString(pClassType: TClass): String;
    function GetOnGetUserFieldClassTypeToString: TOnGetUserFieldClassTypeToString;
    procedure SetOnGetUserFieldClassTypeToString(pValue: TOnGetUserFieldClassTypeToString);
    function GetOnGetUserColumnTypeToFieldCommandClass: TOnGetUserColumnTypeToFieldCommandClass;
    procedure SetOnGetUserColumnTypeToFieldCommandClass(pValue: TOnGetUserColumnTypeToFieldCommandClass);
    function TableFieldCommandToString(pCommandType: TCommandType): String;
    function TableConstraintCommandToString(pCommandType: TCommandType): String;
    function TableCommandToString(pCommandType: TCommandType): String;
    function TableRowCommandToString(pCommandType: TCommandType): String;
    function GetAlterTableSingleCommand: Boolean;
    function DescriptionText(pDescriptionTextType: TDescriptionTextType): String;
    function GenerateCommand(pObjectMapper: TObjectMapper; pObjectState: TObjectState): TCustomCommand;
    procedure IncludeReturningClause(var pSQLText: String; const pFieldNames: TList<String>);
    function IncludeDirectives(pFrom: String): String;
    function SequenceSQL(pSequenceName, pSchemaName: String; pRestartWith: Integer; pCommandType: TCommandType): String;
    function GetSQLForCreateDomain: String;
    function GetSQLForDropDomain: String;
    function AdjustNameLength(pConstraintName: String): String;
    function FormatFieldCommand(pFieldCommandText: String): String;
    function FormatLikeExpression(pExpression: String): String;
    function AlterFieldCommandParams: TAlterFieldCommandParams;
    function GetAlwaysAlterDataTypeOnChangeProperties: Boolean;
    function ColumnTypeToFieldCommandClass(pColumnType: TColumnType): TFieldCommandClass;
    function GetSQLFormatSettings: TSQLFormatSettings;
    procedure AdjustIndexCommand(lIndexCommand: TCustomCommand);
    function GenerateDropIndexCommand(lIndexCommand: TCustomCommand): String;
    function CurrentDateTimeExpression: String;
    function CurrentDateExpression: String;
    function CurrentTimeExpression: String;
    function BooleanToDefaultExpression(pBooleanValue: Boolean): Variant;
    function FixIntColumnTypeForDDLColumn(pColumnType: TColumnType): TColumnType;
    function AllowedColumnTypeConversion(pSourceColumnType, pDestinyColumnType: TColumnType): Boolean;
    property AlterTableSingleCommand: Boolean read GetAlterTableSingleCommand;
    property AlwaysAlterDataTypeOnChangeProperties: Boolean read GetAlwaysAlterDataTypeOnChangeProperties;
    property OnGetUserFieldClassTypeToString: TOnGetUserFieldClassTypeToString read GetOnGetUserFieldClassTypeToString write SetOnGetUserFieldClassTypeToString;
    property OnGetUserColumnTypeToFieldCommandClass: TOnGetUserColumnTypeToFieldCommandClass read GetOnGetUserColumnTypeToFieldCommandClass
      write SetOnGetUserColumnTypeToFieldCommandClass;
  end;

implementation

end.

