unit AM.Freedom.SQLMappers.FromClause;

interface

uses
  System.SysUtils,
  AM.Freedom.SQLMappers.SQLClause,
  AM.Freedom.SQLMapper.CustomArgument;

type
  TFromClause = class(TSQLClause)
  strict protected
    procedure SetArgument(const pArgument: TCustomArgument); override;
  public
    constructor Create(pArgument: TCustomArgument = nil); virtual;
    property Argument;
  end;

implementation

uses
  AM.Freedom.SQLMappers.SelectClause,
  AM.Freedom.SQLMappers.Arguments, AM.Freedom.Exceptions;

{ TFromClause }

constructor TFromClause.Create(pArgument: TCustomArgument);
begin
  if Assigned(pArgument) then
  begin
    SetArgument(pArgument);
  end;
end;

procedure TFromClause.SetArgument(const pArgument: TCustomArgument);
begin
  if (pArgument.ClassType = TSelectClause) or (pArgument.ClassType = TTableArgument) then
  begin
    inherited;
  end else
  begin
    raise EInvalidArgument.Create(pArgument.ClassName, Self.ClassName);
  end;
end;

end.
