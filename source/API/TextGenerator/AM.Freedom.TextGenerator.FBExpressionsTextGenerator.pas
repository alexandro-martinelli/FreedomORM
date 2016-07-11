unit AM.Freedom.TextGenerator.FBExpressionsTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomExpressionTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TFBListTextGenerator = class(TCustomExpressionTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): string; override;
    function GetExpressionName: string; override;
  end;

implementation

uses
  AM.Freedom.SQLMappers.FBExpressions,
  System.SysUtils;

{ TFBListTextGenerator }

function TFBListTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lList: TFBList;
begin
  lList := TFBList(pObject);
  Result := GetExpressionName + '(' + GetTextArgument(lList);
  if lList.Delimiter <> '' then
  begin
    Result := Result + ', ' + QuotedStr(lList.Delimiter);
  end;
  Result := Result + ')';
  if Assigned(pParams) and pParams.WithAlias and (lList.Alias <> '') then
  begin
    Result := Result + ' as ' + lList.Alias;
    pParams.Free;
  end;
end;

function TFBListTextGenerator.GetExpressionName: string;
begin
  Result := 'list';
end;

end.
