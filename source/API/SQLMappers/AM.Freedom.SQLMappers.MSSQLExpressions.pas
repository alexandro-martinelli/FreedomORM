unit AM.Freedom.SQLMappers.MSSQLExpressions;

interface

uses
  System.SysUtils,
  AM.Freedom.SQLMappers.CustomExpression,
  AM.Freedom.SQLMapper.CustomArgument;

type
  TAddType = (Second, Minute, Hour, Day, Month, Year);

  TMSSQLDateAdd = class(TCustomExpression)
  private
    FAddType: TAddType;
    FIncrement: UInt16;
  public
    constructor Create(pAddType: TAddType; pIncrement: UInt16; pArgument: TCustomArgument; pAlias: String = '');
    property AddType: TAddType read FAddType write FAddType;
    property Increment: UInt16 read FIncrement write FIncrement;
  end;

  TMSSQLGetDate = class(TCustomExpression)
  strict protected
    property Argument;
  public
    constructor Create(pAlias: String = ''); reintroduce;
  end;

implementation

{ TMSSQLDateAdd }

constructor TMSSQLDateAdd.Create(pAddType: TAddType; pIncrement: UInt16; pArgument: TCustomArgument; pAlias: String);
begin
  inherited Create(pArgument, pAlias);
  FAddType := pAddType;
  FIncrement := pIncrement;
end;

{ TMSSQLGetDate }

constructor TMSSQLGetDate.Create(pAlias: String);
begin
  inherited Create(nil, pAlias);
end;

end.
