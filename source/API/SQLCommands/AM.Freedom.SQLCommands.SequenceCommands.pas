unit AM.Freedom.SQLCommands.SequenceCommands;

interface

uses
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.EnumerationTypes;

type
  TCustomSequenceCommand = class(TCustomCommand)
  strict protected
    property Alias;
  public
    constructor Create(pSequenceName: String; pSchemaName: String = ''); reintroduce; overload;
    property Name;
  end;

  TCreateSequenceCommand = class(TCustomSequenceCommand)
  strict protected
    function GetCommandType: TCommandType; override;
  end;

  TAlterSequenceCommand = class(TCustomSequenceCommand)
  strict private
    FRestartWith: Integer; protected
    function GetCommandType: TCommandType; override;
  public
    constructor Create(pSequenceName: String; pRestartWith: Integer; pSchemaName: String = ''); overload;
    property RestartWith: Integer read FRestartWith write FRestartWith;
  end;

  TDropSequenceCommand = class(TCustomSequenceCommand)
  strict protected
    function GetCommandType: TCommandType; override;
  end;

implementation

{ TCreateSequenceCommand }

function TCreateSequenceCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Create;
end;

{ TRestartSequenceCommand }

constructor TAlterSequenceCommand.Create(pSequenceName: String; pRestartWith: Integer; pSchemaName: String);
begin
  inherited Create(pSequenceName, pSchemaName);
  FRestartWith := pRestartWith;
end;

function TAlterSequenceCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Alter;
end;

{ TDropSequenceCommand }

function TDropSequenceCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Drop;
end;

{ TCustomSequenceCommand }

constructor TCustomSequenceCommand.Create(pSequenceName, pSchemaName: String);
begin
  inherited Create;
  Name := pSequenceName;
  Schema := pSchemaName;
end;

end.
