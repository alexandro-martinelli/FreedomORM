unit AM.Freedom.SQLCommands.IndexCommands;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLMappers.NamedObject,
  AM.Freedom.IndexColumn;

type
  TCustomIndexCommand = class(TCustomCommand)
  private
    FOnTable: String;
  public
    property Schema;
    property Name;
    property OnTable: String read FOnTable write FOnTable;
  end;

  TCreateIndexCommand = class(TCustomIndexCommand)
  private
    FColumns: TIndexColumns;
    FOption: TIndexOption;
  strict protected
    function GetCommandType: TCommandType; override;
  public
    constructor Create(pSchemaName: string = ''); override;
    destructor Destroy; override;
    procedure AddColumn(pColumnName: String; pSortType: TSortType = Asc; pNullOption: TNullOption = noNone); overload;
    procedure AddColumn(pColumn: TIndexColumn); overload;
    property Columns: TIndexColumns read FColumns;
    property Option: TIndexOption read FOption write FOption default ioNone;
  end;

  TDropIndexCommand = class(TCustomIndexCommand)
  strict protected
    function GetCommandType: TCommandType; override;
  public
    constructor Create(pName, pOnTable: string; pSchemaName: String = ''); reintroduce; overload;
  end;

implementation

{ TCreateIndexCommand }

procedure TCreateIndexCommand.AddColumn(pColumnName: String; pSortType: TSortType; pNullOption: TNullOption);
begin
  FColumns.AddColumn(pColumnName, pSortType, pNullOption);
end;

procedure TCreateIndexCommand.AddColumn(pColumn: TIndexColumn);
begin
  FColumns.Add(pColumn);
end;

constructor TCreateIndexCommand.Create(pSchemaName: string);
begin
  inherited;
  FColumns := TIndexColumns.Create;
  FOption := ioNone;
end;

destructor TCreateIndexCommand.Destroy;
begin
  FColumns.Free;
  inherited;
end;

function TCreateIndexCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Create;
end;

{ TDropIndexCommand }

constructor TDropIndexCommand.Create(pName, pOnTable, pSchemaName: String);
begin
  inherited Create(pSchemaName);
  Name := pName;
  OnTable := pOnTable;
end;

function TDropIndexCommand.GetCommandType: TCommandType;
begin
  Result := TCommandType.Drop;
end;

end.
