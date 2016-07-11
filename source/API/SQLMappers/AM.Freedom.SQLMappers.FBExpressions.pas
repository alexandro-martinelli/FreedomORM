unit AM.Freedom.SQLMappers.FBExpressions;

interface

uses
  AM.Freedom.SQLMappers.CustomExpression,
  AM.Freedom.SQLMapper.CustomArgument;

type
  TFBList = class(TCustomExpression)
  strict private
    FDelimiter: String;
  public
    constructor Create(pArgument: TCustomArgument; pDelimiter: String = ''; pAlias: String = ''); reintroduce;
    property Delimiter: String read FDelimiter write FDelimiter;
  end;

implementation

{ TFBList }

constructor TFBList.Create(pArgument: TCustomArgument; pDelimiter, pAlias: String);
begin
  inherited Create(pArgument, pAlias);
  FDelimiter := pDelimiter;
end;

end.
