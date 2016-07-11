unit AM.Freedom.SQLCommands.DomainCommands;

interface

uses
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLCommands.FieldOptions, AM.Freedom.EnumerationTypes;

type
  TCustomDomainCommand = class(TCustomCommand)
  {$HINTS OFF}
  strict private
    property Schema;
    property Alias;
  {$HINTS ON}
  public
    constructor Create(pName: String; pSchemaName: String = ''); reintroduce; overload;
    property Name;
  end;

  TCreateDomainCommand = class(TCustomDomainCommand)
  strict private
    FDomainOptions: TDomainOptions;
    FColumnType: TColumnType;
    FSize: UInt16;
    FScale: UInt8;
    procedure SetDomainOptions(const Value: TDomainOptions);
  strict protected
    function GetCommandType: TCommandType; override;
  public
    constructor Create; overload;
    constructor Create(pName: String; pDomainOptions: TDomainOptions; pColumnType: TColumnType; pSize: UInt16; pScale: UInt8;
        pSchemaName: String = ''); reintroduce; overload;
    destructor Destroy; override;
    property DomainOptions: TDomainOptions read FDomainOptions write SetDomainOptions;
    property ColumnType: TColumnType read FColumnType write FColumnType;
    property Size: UInt16 read FSize write FSize;
    property Scale: UInt8 read FScale write FScale;
  end;

  TDropDomainCommand = class(TCustomDomainCommand)
  strict protected
    function GetCommandType: TCommandType; override;
  end;

implementation

uses
  System.SysUtils;


{ TCustomDomainCommand }

constructor TCustomDomainCommand.Create(pName, pSchemaName: String);
begin
  inherited Create(pSchemaName);
  Name := pName;
end;

{ TCreateDomainCommand }

constructor TCreateDomainCommand.Create(pName: String; pDomainOptions: TDomainOptions; pColumnType: TColumnType; pSize: UInt16; pScale: UInt8;
        pSchemaName: String);
begin
  inherited Create(pName, pSchemaName);
  SetDomainOptions(pDomainOptions);
  FColumnType := pColumnType;
  FSize := pSize;
  FScale := pScale;
end;

constructor TCreateDomainCommand.Create;
begin
  inherited Create('', '');
  FDomainOptions := TDomainOptions.Create;
end;

destructor TCreateDomainCommand.Destroy;
begin
  FDomainOptions.Free;
  inherited;
end;

function TCreateDomainCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Create;
end;

procedure TCreateDomainCommand.SetDomainOptions(const Value: TDomainOptions);
begin
  if FDomainOptions <> Value then
  begin
    FreeAndnil(FDomainOptions);
  end;
  FDomainOptions := Value;
end;

{ TDropDomainCommand }

function TDropDomainCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Drop;
end;

end.
