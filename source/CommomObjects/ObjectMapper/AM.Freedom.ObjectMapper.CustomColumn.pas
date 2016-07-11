unit AM.Freedom.ObjectMapper.CustomColumn;

interface

uses
  AM.Freedom.ObjectMapper.CustomMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.ColumnOptions;

type
  TCustomColumn = class(TCustomMapper)
  strict private
    FColumnType: TColumnType;
    FColumnOptions: TColumnOptions;
    FIdOptions: TIdOptions;
    FDefaultValueOptions: TDefaultValueOptions;
    FDomain: String;
  strict protected
    property Domain: String read FDomain write FDomain;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property ColumnType: TColumnType read FColumnType write FColumnType;
    property ColumnOptions: TColumnOptions read FColumnOptions write FColumnOptions;
    property IdOptions: TIdOptions read FIdOptions;
    property DefaultValueOptions: TDefaultValueOptions read FDefaultValueOptions;
  end;

implementation

{ TCustomColumn }

constructor TCustomColumn.Create;
begin
  FIdOptions := TIdOptions.Create;
  FDefaultValueOptions := TDefaultValueOptions.Create;
  FColumnOptions := [];
end;

destructor TCustomColumn.Destroy;
begin
  FIdOptions.Free;
  FDefaultValueOptions.Free;
  inherited;
end;

end.
