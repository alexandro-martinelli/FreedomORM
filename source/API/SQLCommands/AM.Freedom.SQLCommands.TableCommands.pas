unit AM.Freedom.SQLCommands.TableCommands;

interface

uses
  System.Generics.Collections,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLCommands.Constraints,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.CustomTableCommand,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.SQLMappers.WhereClause,
  AM.Freedom.GroupCriteria.Criteria;

type
  TCreateTableCommand = class(TCustomCommand)
  strict private
    FFields: TObjectList<TCustomFieldCommand>;
    FPrimaryKey: TPrimaryKey;
    FForeignKeys: TObjectList<TForeignKey>;
    FUniqueKeys: TObjectList<TUniqueKey>;
  strict protected
    procedure SetPrimaryKey(const pPrimaryKey: TPrimaryKey);
    function GetCommandType: TCommandType; override;
  public
    constructor Create(pSchemaName: string = ''); override;
    destructor Destroy; override;
    function AddField(pField: TCustomFieldCommand): TCreateTableCommand;
    function AddPrimaryKey(pPrimaryKey: TPrimaryKey): TCreateTableCommand;
    function AddForeignKey(pForeignKey: TForeignKey): TCreateTableCommand;
    function AddUniqueKey(pUniqueKey: TUniqueKey): TCreateTableCommand;
    property Name;
    property Fields: TObjectList<TCustomFieldCommand> read FFields;
    property PrimaryKey: TPrimaryKey read FPrimaryKey write SetPrimaryKey;
    property ForeignKeys: TObjectList<TForeignKey> read FForeignKeys;
    property UniqueKeys: TObjectList<TUniqueKey> read FUniqueKeys;
  end;

  TAlterTableCommand = class(TCustomCommand)
  strict private
    FCommands: TObjectList<TCustomTableCommand>;
  strict protected
    function GetCommandType: TCommandType; override;
  public
    constructor Create(pSchemaName: string = ''); overload; override;
    constructor Create(pSchemaName: string; const pName: String); reintroduce; overload;
    destructor Destroy; override;
    function AddField(const pField: TCustomFieldCommand): TAlterTableCommand;
    function AlterField(const pField: TCustomFieldCommand; pChangedProperties: TChangedProperties): TAlterTableCommand;
    function DropField(const pFieldName: String): TAlterTableCommand;
    function AddConstraint(const pConstraint: TCustomConstraint): TAlterTableCommand;
    function DropConstraint(const pConstraintName: String): TAlterTableCommand;
    property Name;
    property Commands: TObjectList<TCustomTableCommand> read FCommands;
  end;

  TDropTableCommand = class(TCustomCommand)
  strict protected
    function GetCommandType: TCommandType; override;
  public
    constructor Create(pName: String; const pSchemaName: string = ''); reintroduce; overload;
    property Name;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.SQLCommands.TableFieldCommands;

{ TCreateTableCommand }

function TCreateTableCommand.AddField(pField: TCustomFieldCommand): TCreateTableCommand;
begin
  FFields.Add(pField);
  Result := Self;
end;

function TCreateTableCommand.AddForeignKey(pForeignKey: TForeignKey): TCreateTableCommand;
begin
  FForeignKeys.Add(pForeignKey);
  Result := Self;
end;

function TCreateTableCommand.AddPrimaryKey(pPrimaryKey: TPrimaryKey): TCreateTableCommand;
begin
  SetPrimaryKey(pPrimaryKey);
  Result := Self;
end;

function TCreateTableCommand.AddUniqueKey(pUniqueKey: TUniqueKey): TCreateTableCommand;
begin
  FUniqueKeys.Add(pUniqueKey);
  Result := Self;
end;

constructor TCreateTableCommand.Create(pSchemaName: string);
begin
  inherited;
  FFields := TObjectList<TCustomFieldCommand>.Create;
  FForeignKeys := TObjectList<TForeignKey>.Create;
  FUniqueKeys := TObjectList<TUniqueKey>.Create;
end;

destructor TCreateTableCommand.Destroy;
begin
  FFields.Free;
  FPrimaryKey.Free;
  FForeignKeys.Free;
  FUniqueKeys.Free;
  inherited;
end;

function TCreateTableCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Create;
end;

procedure TCreateTableCommand.SetPrimaryKey(const pPrimaryKey: TPrimaryKey);
begin
  if FPrimaryKey <> pPrimaryKey then
  begin
    FreeAndNil(FPrimaryKey);
  end;
  FPrimaryKey := pPrimaryKey;
end;
{ TAlterTableCommand }

function TAlterTableCommand.AddConstraint(const pConstraint: TCustomConstraint): TAlterTableCommand;
begin
  Result := Self;
  FCommands.Add(TAddConstraintCommand.Create(pConstraint));
end;

function TAlterTableCommand.AddField(const pField: TCustomFieldCommand): TAlterTableCommand;
begin
  Result := Self;
  FCommands.Add(TAddFieldCommand.Create(pField));
end;

function TAlterTableCommand.AlterField(const pField: TCustomFieldCommand; pChangedProperties: TChangedProperties): TAlterTableCommand;
begin
  Result := Self;
  FCommands.Add(TAlterFieldCommand.Create(pField, pChangedProperties));
end;

constructor TAlterTableCommand.Create(pSchemaName: string; const pName: String);
begin
  Create(pSchemaName);
  Name := pName;
end;

constructor TAlterTableCommand.Create(pSchemaName: string);
begin
  inherited Create;
  FCommands := TObjectList<TCustomTableCommand>.Create;
end;

destructor TAlterTableCommand.Destroy;
begin
  FreeAndNil(FCommands);
  inherited;
end;

function TAlterTableCommand.DropConstraint(const pConstraintName: String): TAlterTableCommand;
var
  lCommand: TCustomCommand;
  lCanAdd: Boolean;
begin
  Result := Self;
  lCanAdd := True;
  for lCommand in FCommands do
  begin
    if lCommand.InheritsFrom(TDropConstraintCommand) then
    begin
      lCanAdd := not SameText(TDropConstraintCommand(lCommand).Name, pConstraintName);
      if not lCanAdd then
      begin
        Break;
      end;
    end;
  end;
  if lCanAdd then
  begin
    FCommands.Add(TDropConstraintCommand.Create(pConstraintName));
  end;
end;

function TAlterTableCommand.DropField(const pFieldName: string): TAlterTableCommand;
begin
  Result := Self;
  FCommands.Add(TDropFieldCommand.Create(pFieldName));
end;

function TAlterTableCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Alter;
end;
{ TDropTableCommand }

constructor TDropTableCommand.Create(pName: String; const pSchemaName: string);
begin
  inherited Create(pSchemaName);
  Name := pName;
end;

function TDropTableCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Drop;
end;

end.
