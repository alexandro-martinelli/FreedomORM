unit AM.Freedom.SQLCommands.SchemaCommands;

interface

uses
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.EnumerationTypes;

type
  TCustomSchemaCommand = class(TCustomCommand)
  {$HINTS OFF}
  strict private
    property Alias;
    property Schema;
  {$HINTS ON}
  public
    constructor Create(pSchemaName: String); reintroduce;
    property Name;
  end;

  TCreateSchemaCommand = class(TCustomSchemaCommand)
  strict protected
    function GetCommandType: TCommandType; override;
  end;

  TDropSchemaCommand = class(TCustomSchemaCommand)
  strict private
    FCascade: Boolean;
  protected
    function GetCommandType: TCommandType; override;
  public
    constructor Create(pSchemaName: String; pCascade: Boolean = False); reintroduce;
    property Cascade: Boolean read FCascade write FCascade;
  end;

implementation

{ TCreateSchemaCommand }

function TCreateSchemaCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Create;
end;
{ TDropSchemaCommand }

constructor TDropSchemaCommand.Create(pSchemaName: String; pCascade: Boolean);
begin
  inherited Create(pSchemaName);
  FCascade := pCascade;
end;

function TDropSchemaCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Drop;
end;

{ TCustomSchemaCommand }

constructor TCustomSchemaCommand.Create(pSchemaName: String);
begin
  inherited Create;
  Name := pSchemaName;
end;

end.
