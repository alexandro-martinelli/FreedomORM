unit AM.Freedom.Persistent.MSSQLPersistent;

interface

uses
  AM.Freedom.CustomDBPersistent,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.SQLMappers.IDDLExtracter,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLMappers.MSSQLMapper,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.DBPersistent.IDBConnector;

type
  TMSSQLPersistent = class(TCustomDBPersistent)
  private
    FSelectOptions: TSelectOptions;
    procedure OnSelectOptionsChange(Sender: TObject);
  strict protected
    function CreateDDLExtracter: IDDLExtracter; override;
    function SupportedCommand(pCommand: TCustomCommand): Boolean; override;
    function CreateSQLMapper(pAsDefault: Boolean): ISQLMapper; override;
  public
    constructor Create(pDBConnector: IDBConnector; pAsDefault: Boolean = False); override;
    destructor Destroy; override;
    property SelectOptions: TSelectOptions read FSelectOptions;
  end;

implementation

uses
  AM.Freedom.ObjectMapper.MSSQLDDLExtracter,
  AM.Freedom.SQLCommands.SequenceCommands,
  AM.Freedom.SQLCommands.Fields;

{ TMSSQLPersistent }

constructor TMSSQLPersistent.Create(pDBConnector: IDBConnector;
  pAsDefault: Boolean);
begin
  inherited Create(pDBConnector, pAsDefault);
  FSelectOptions := TSelectOptions.Create;
  FSelectOptions.OnChange := OnSelectOptionsChange;
end;

function TMSSQLPersistent.CreateDDLExtracter: IDDLExtracter;
begin
  Result := TMSSQLDDLExtracter.Create(Self);
end;

function TMSSQLPersistent.CreateSQLMapper(pAsDefault: Boolean): ISQLMapper;
begin
  Result := TMSSQLMapper.Create(pAsDefault);
end;

destructor TMSSQLPersistent.Destroy;
begin
  FSelectOptions.Free;
  inherited;
end;

procedure TMSSQLPersistent.OnSelectOptionsChange(Sender: TObject);
begin
  TMSSQLMapper(GetSQLMapper).SelectOptions.Assign(FSelectOptions);
end;

function TMSSQLPersistent.SupportedCommand(pCommand: TCustomCommand): Boolean;
begin
  Result := not pCommand.InheritsFrom(TCustomSequenceCommand);
end;

end.
