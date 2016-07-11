unit AM.Freedom.SQLMappers.CustomExpression;

interface

uses
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLMappers.Arguments;

type
  TExpressionClass = class of TCustomExpression;

  TCustomExpression = class abstract(TComplexArgument)
  strict private
    FArgument: TCustomArgument;
  strict protected
    procedure SetArgument(const pArgument: TCustomArgument); virtual;
  public
    constructor Create(pArgument: TCustomArgument; pAlias: String = ''); overload;
    destructor Destroy; override;
    property Argument: TCustomArgument read FArgument write SetArgument;
    property Alias;
  end;


implementation

uses
  System.SysUtils;

{ TCustomExpression }

constructor TCustomExpression.Create(pArgument: TCustomArgument; pAlias: String);
begin
  Inherited Create;
  Self.SetArgument(pArgument);
  Alias := pAlias;
end;


destructor TCustomExpression.Destroy;
begin
  FreeAndNil(FArgument);
  inherited;
end;

procedure TCustomExpression.SetArgument(const pArgument: TCustomArgument);
begin
  if Assigned(FArgument) then
  begin
    FArgument.Free;
  end;
  FArgument := pArgument;
end;

end.
