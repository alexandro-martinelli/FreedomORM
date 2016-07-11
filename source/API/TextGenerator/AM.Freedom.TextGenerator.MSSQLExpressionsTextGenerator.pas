unit AM.Freedom.TextGenerator.MSSQLExpressionsTextGenerator;

interface

uses
  System.SysUtils,
  AM.Freedom.SQLMappers.MSSQLExpressions,
  AM.Freedom.TextGenerator.CustomExpressionTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TMSSQLDateAddTextGenerator = class(TCustomExpressionTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
    function GetExpressionName: string; override;
  end;

  TMSSQLGetDateTextGenerator = class(TCustomExpressionTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
    function GetExpressionName: string; override;
  end;

implementation

{ TMSSQLDateAddTextGenerator }

function TMSSQLDateAddTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lExpression: TMSSQLDateAdd;
begin
  lExpression := TMSSQLDateAdd(pObject);
  Result := GetExpressionName + '(';
  case lExpression.AddType of
    Second:
      Result := Result + 'second';
    Minute:
      Result := Result + 'minute';
    Hour:
      Result := Result + 'hour';
    Day:
      Result := Result + 'day';
    Month:
      Result := Result + 'month';
    Year:
      Result := Result + 'year';
  end;
  Result := Result + ', ' + IntToStr(lExpression.Increment) + ', ' + GetTextArgument(lExpression) + ')';
  if Assigned(pParams) and pParams.WithAlias and (lExpression.Alias <> '') then
  begin
    Result := Result + ' as ' + lExpression.Alias;
    pParams.Free;
  end;
end;

function TMSSQLDateAddTextGenerator.GetExpressionName: string;
begin
  Result := 'dateadd';
end;

{ TMSSQLGetDateTextGenerator }

function TMSSQLGetDateTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := GetExpressionName;
  if Assigned(pParams) and pParams.WithAlias and (TMSSQLGetDate(pObject).Alias <> '') then
  begin
    Result := Result + ' as ' + TMSSQLGetDate(pObject).Alias;
    pParams.Free;
  end;
end;

function TMSSQLGetDateTextGenerator.GetExpressionName: string;
begin
  Result := 'getdate()';
end;

end.
