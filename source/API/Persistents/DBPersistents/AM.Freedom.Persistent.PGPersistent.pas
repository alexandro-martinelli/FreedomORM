unit AM.Freedom.Persistent.PGPersistent;

interface

uses
  AM.Freedom.CustomDBPersistent,
  AM.Freedom.SQLMappers.IDDLExtracter,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.CustomFieldCommand;

type
  TPGPersistent = class(TCustomDBPersistent)
  strict protected
    function CreateDDLExtracter: IDDLExtracter; override;
    function CreateSQLMapper(pAsDefault: Boolean): ISQLMapper; override;
  end;

implementation

uses
  AM.Freedom.SQLMappers.PGSQLMapper,
  AM.Freedom.ObjectMapper.PGDDLExtracter,
  AM.Freedom.SQLCommands.Fields;

{ TPGPersistent }

function TPGPersistent.CreateDDLExtracter: IDDLExtracter;
begin
  Result := TPGDDLExtracter.Create(Self);
end;

function TPGPersistent.CreateSQLMapper(pAsDefault: Boolean): ISQLMapper;
begin
  Result := TPGSQLMapper.Create(pAsDefault);
end;

end.
