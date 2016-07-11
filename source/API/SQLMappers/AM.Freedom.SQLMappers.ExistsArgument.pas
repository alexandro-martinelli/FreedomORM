unit AM.Freedom.SQLMappers.ExistsArgument;

interface

uses
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLMappers.SelectClause;

type
  TExistsArgument = class(TCustomArgument)
  strict private
    FSelect: TSelectClause;
  private
    procedure SetSelect(const pSelect: TSelectClause);
  public
    constructor Create(pSelect: TSelectClause); virtual;
    destructor Destroy; override;
    property Select: TSelectClause read FSelect write SetSelect;
  end;

implementation

uses
  System.SysUtils;

{ TExistsArgument }

constructor TExistsArgument.Create(pSelect: TSelectClause);
begin
  FSelect := pSelect;
end;

destructor TExistsArgument.Destroy;
begin
  FreeAndNil(FSelect);
  inherited;
end;

procedure TExistsArgument.SetSelect(const pSelect: TSelectClause);
begin
  if FSelect <> pSelect then
  begin
    FreeAndNil(FSelect);
  end;
  FSelect := pSelect;
end;

end.
