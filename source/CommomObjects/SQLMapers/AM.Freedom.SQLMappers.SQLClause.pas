unit AM.Freedom.SQLMappers.SQLClause;

interface

uses
  AM.Freedom.SQLMappers.ISQLClause,
  AM.Freedom.SQLMapper.CustomArgument;

type
  TSQLClause = class abstract (TInterfacedObject, ISQLClause)
  strict private
    FArgument: TCustomArgument;
  strict protected
    function GetArgument: TCustomArgument; virtual;
    procedure SetArgument(const pArgument: TCustomArgument); virtual;
  strict protected
    property Argument: TCustomArgument read GetArgument write SetArgument;
  public
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils;

{ TCustomLikerClause }

destructor TSQLClause.Destroy;
begin
  FreeAndNil(FArgument);
  inherited;
end;

function TSQLClause.GetArgument: TCustomArgument;
begin
  Result := FArgument;
end;

procedure TSQLClause.SetArgument(const pArgument: TCustomArgument);
begin
  if Assigned(FArgument) then
  begin
    FArgument.Free;
  end;
  FArgument := pArgument;
end;

end.
