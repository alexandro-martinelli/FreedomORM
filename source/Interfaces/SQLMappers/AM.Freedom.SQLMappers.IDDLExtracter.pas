unit AM.Freedom.SQLMappers.IDDLExtracter;

interface

uses
  System.Generics.Collections,
  AM.Freedom.ObjectMapper,
  AM.Freedom.ObjectMapper.DDLObjects,
  AM.Freedom.IDBPersistent,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Consts,
  AM.Freedom.SQLCommands.CustomTableCommand;

type
  IDDLExtracter = interface
    ['{24F97907-45A6-4C3C-9CD6-E79A14A8201D}']
    function ExtractDDL(pObjectMapper: TObjectMapper; pSchemaName: String = ''; pDDLOptions: TDDLOptions = TConsts.cDDLAll): TDDLEntity;
    function ExtractObjectMapper(pName: String; pSchemaName: String = ''; pDDLOptions: TDDLOptions = TConsts.cDDLAll): TObjectMapper;
    function ExtractDDLDomains(pSchemaName: String = ''): TDDLDomains;
    function ExtractDDLSchemas: TList<String>;
    function VerifyDropField(pTableName, pFieldName: String): TList<TCustomTableCommand>;
    function GetDBPersistent: IDBPersistent;
    property DBPersistent: IDBPersistent read GetDBPersistent;
  end;

implementation

end.
