unit AM.Freedom.SQLCommands.TableFieldCommands;

interface

uses
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLCommands.Constraints,
  AM.Freedom.SQLCommands.CustomTableCommand,
  AM.Freedom.EnumerationTypes;

type
  TAddAlterFieldCommand = class abstract(TCustomTableCommand)
  strict private
    FField: TCustomFieldCommand;
  strict protected
    procedure SetField(const pField: TCustomFieldCommand);
  public
    constructor Create(const pField: TCustomFieldCommand); reintroduce; overload;
    destructor Destroy; override;
    property Field: TCustomFieldCommand read FField write SetField;
  end;

  TAddFieldCommand = class(TAddAlterFieldCommand)
  strict protected
    function GetCommandType: TCommandType; override;
  end;

  TAlterFieldCommand = class(TAddAlterFieldCommand)
  strict private
    FChangedProperties: TChangedProperties;
  strict protected
    function GetCommandType: TCommandType; override;
  public
    constructor Create(const pField: TCustomFieldCommand; pChangedProperties: TChangedProperties); reintroduce; overload;
    property ChangedProperties: TChangedProperties read FChangedProperties write FChangedProperties;
  end;

  TDropFieldCommand = class(TCustomTableCommand)
  strict protected
    function GetCommandType: TCommandType; override;
  public
    constructor Create(const pName: String); reintroduce;
    property Name;
  end;

  TAddConstraintCommand = class(TCustomTableCommand)
  strict private
    FConstraint: TCustomConstraint;
  strict protected
    procedure SetConstraint(const pConstraint: TCustomConstraint);
    function GetCommandType: TCommandType; override;
  public
    constructor Create(const pConstraint: TCustomConstraint); reintroduce;
    destructor Destroy; override;
    property Constraint: TCustomConstraint read FConstraint write SetConstraint;
  end;

  TDropConstraintCommand = class(TCustomTableCommand)
  strict protected
    function GetCommandType: TCommandType; override;
  public
    constructor Create(const pName: String; pSchemaName: String = ''); reintroduce; overload;
    property Name;
  end;

implementation

uses
  System.SysUtils;

{ TAddFieldCommand }

constructor TAddAlterFieldCommand.Create(const pField: TCustomFieldCommand);
begin
  inherited Create('');
  SetField(pField);
end;

destructor TAddAlterFieldCommand.Destroy;
begin
  FreeAndNil(FField);
  inherited;
end;

procedure TAddAlterFieldCommand.SetField(const pField: TCustomFieldCommand);
begin
  if FField <> pField then
  begin
    FreeAndNil(FField);
  end;
  FField := pField;
end;

{ TAddFieldCommand }

function TAddFieldCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Add;
end;

{ TAddConstraintCommand }

constructor TAddConstraintCommand.Create(const pConstraint: TCustomConstraint);
begin
  inherited Create('');
  SetConstraint(pConstraint);
end;

destructor TAddConstraintCommand.Destroy;
begin
  FreeAndNil(FConstraint);
  inherited;
end;

function TAddConstraintCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Add;
end;

procedure TAddConstraintCommand.SetConstraint(const pConstraint: TCustomConstraint);
begin
  if FConstraint <> pConstraint then
  begin
    FreeAndNil(FConstraint);
  end;
  FConstraint := pConstraint;
end;

{ TDropFieldCommand }

constructor TDropFieldCommand.Create(const pName: String);
begin
  inherited Create('');
  Name := pName;
end;

function TDropFieldCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Drop;
end;

{ TDropConstraintCommand }

constructor TDropConstraintCommand.Create(const pName: String; pSchemaName: String);
begin
  inherited Create(pSchemaName);
  Name := pName;
end;

function TDropConstraintCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Drop;
end;

{ TAlterFieldCommand }

constructor TAlterFieldCommand.Create(const pField: TCustomFieldCommand; pChangedProperties: TChangedProperties);
begin
  inherited Create(pField);
  FChangedProperties := pChangedProperties;
end;

function TAlterFieldCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Alter;
end;

end.
