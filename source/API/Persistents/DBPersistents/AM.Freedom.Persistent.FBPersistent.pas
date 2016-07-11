unit AM.Freedom.Persistent.FBPersistent;

interface

uses
  AM.Freedom.CustomDBPersistent,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.SQLMappers.FBSQLMapper,
  AM.Freedom.ObjectMapper.DDLObjects,
  AM.Freedom.SQLMappers.IDDLExtracter,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLCommands.FBFields,
  AM.Freedom.SQLCommands.Fields,
  AM.Freedom.SQLMapper.CustomArgument;

type
  TFBPersistent = class(TCustomDBPersistent)
  strict protected
    function CreateSQLMapper(pAsDefault: Boolean): ISQLMapper; override;
    function CreateDDLExtracter: IDDLExtracter; override;
    function SupportedCommand(pCommand: TCustomCommand): Boolean; override;
  end;

implementation

uses
  AM.Freedom.ObjectMapper.FBDDLExtracter,
  AM.Freedom.SQLCommands.SchemaCommands,
  AM.Freedom.Exceptions;

{ TFBPersistent }

function TFBPersistent.CreateDDLExtracter: IDDLExtracter;
begin
  Result := TFBDDLExtracter.Create(Self);
end;

function TFBPersistent.CreateSQLMapper(pAsDefault: Boolean): ISQLMapper;
begin
  Result := TFBSQLMapper.Create(pAsDefault);
end;

function TFBPersistent.SupportedCommand(pCommand: TCustomCommand): Boolean;
begin
  Result := not pCommand.InheritsFrom(TCustomSchemaCommand);
end;

end.
