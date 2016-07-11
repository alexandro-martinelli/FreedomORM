unit AM.Freedom.TextGenerator.ExpressionsTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomExpressionTextGenerator,
  AM.Freedom.Helper.CalcExpressionType,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TSumTextGenerator = class(TCustomExpressionTextGenerator)
  strict protected
    function GetExpressionName: string; override;
  end;

  TMinTextGenerator = class(TCustomExpressionTextGenerator)
  strict protected
    function GetExpressionName: string; override;
  end;

  TMaxTextGenerator = class(TCustomExpressionTextGenerator)
  strict protected
    function GetExpressionName: string; override;
  end;

  TAvgTextGenerator = class(TCustomExpressionTextGenerator)
  strict protected
    function GetExpressionName: string; override;
  end;

  TCountTextGenerator = class(TCustomExpressionTextGenerator)
  strict protected
    function GetExpressionName: string; override;
  end;

  TUpperTextGenerator = class(TCustomExpressionTextGenerator)
  strict protected
    function GetExpressionName: string; override;
  end;

  TLowerTextGenerator = class(TCustomExpressionTextGenerator)
  strict protected
    function GetExpressionName: string; override;
  end;

  TCoalesceTextGenerator = class(TCustomExpressionTextGenerator)
  strict protected
    function GetExpressionName: string; override;
    function GetTextArgument(pObject: TObject): string; override;
  end;

  TCastTextGenerator = class(TCustomExpressionTextGenerator)
  strict protected
    function GetExpressionName: string; override;
    function GetTextArgument(pObject: TObject): string; override;
  end;

  TCalcTextGenerator = class(TCustomExpressionTextGenerator)
  strict protected
    function GetExpressionName: string; override;
    function GetTextArgument(pObject: TObject): string; override;
  end;

  TCaseWhenTextGenerator = class(TCustomExpressionTextGenerator)
  strict protected
    function GetExpressionName: string; override;
    function GetTextArgument(pObject: TObject): string; override;
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLMappers.Expressions;

{ TSumTextGenerator }

function TSumTextGenerator.GetExpressionName: string;
begin
  Result := 'SUM';
end;

{ TMinTextGenerator }

function TMinTextGenerator.GetExpressionName: string;
begin
  Result := 'MIN';
end;

{ TMaxTextGenerator }

function TMaxTextGenerator.GetExpressionName: string;
begin
  Result := 'MAX';
end;

{ TAvgTextGenerator }

function TAvgTextGenerator.GetExpressionName: string;
begin
  Result := 'AVG';
end;

{ TCountTextGenerator }

function TCountTextGenerator.GetExpressionName: string;
begin
  Result := 'COUNT';
end;

{ TCoalesceTextGenerator }

function TCoalesceTextGenerator.GetExpressionName: string;
begin
  Result := 'COALESCE';
end;

function TCoalesceTextGenerator.GetTextArgument(pObject: TObject): string;
var
  lArgument: TCustomArgument;
begin
  Result := inherited;
  for lArgument in TCoalesce(pObject).AnotherArguments do
  begin
    Result := Result + ', ' + GetTextFromGenerator(lArgument, GetTextParams);
  end;
end;

{ TCastTextGenerator }

function TCastTextGenerator.GetExpressionName: string;
begin
  Result := 'CAST';
end;

function TCastTextGenerator.GetTextArgument(pObject: TObject): string;
var
  lCast: TCast;
begin
  lCast := TCast(pObject);
  Result := inherited + ' as ' + GetTextFromGenerator(lCast.CastAs);
end;

{ TUpperTextGenerator }

function TUpperTextGenerator.GetExpressionName: string;
begin
  Result := 'UPPER';
end;

{ TLowerTextGenerator }

function TLowerTextGenerator.GetExpressionName: string;
begin
  Result := 'LOWER';
end;

{ TCalcTextGenerator }

function TCalcTextGenerator.GetExpressionName: string;
begin
  Result := EmptyStr;
end;

function TCalcTextGenerator.GetTextArgument(pObject: TObject): string;
var
  lCalc: TCalc;
  lCalcItem: TCalcItem;
begin
  lCalc  := TCalc(pObject);
  Result := EmptyStr;
  for lCalcItem in lCalc.CalcItems do
  begin
    Result := Result + ifthen(Result <> EmptyStr, ' ' + lCalcItem.CalcType.ToString + ' ') +
      GetTextFromGenerator(lCalcItem.Argument, TGenerateTextParams.Create(False, True));
  end;
end;

{ TCaseWhenTextGenerator }

function TCaseWhenTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
begin
  Result := '(' + GetExpressionName + GetTextArgument(pObject) + ')';
  if Assigned(pParams) and pParams.WithAlias and (TCaseWhen(pObject).Alias <> '') then
  begin
    Result := Result + ' as ' + TCaseWhen(pObject).Alias;
  end;
end;

function TCaseWhenTextGenerator.GetExpressionName: string;
begin
  Result := 'case when';
end;

function TCaseWhenTextGenerator.GetTextArgument(pObject: TObject): string;
var
  lCaseWHen: TCaseWhen;
begin
  lCaseWHen := TCaseWhen(pObject);
  Result    := ' ' + GetTextFromGenerator(lCaseWHen.CaseWhen) + ' then ' +
    GetTextFromGenerator(lCaseWHen.CaseThen, TGenerateTextParams.Create(False, True));
  if Assigned(lCaseWHen.CaseElse) then
  begin
    Result := Result + ' else ' + GetTextFromGenerator(lCaseWHen.CaseElse,
      TGenerateTextParams.Create(False, True));
  end;
  Result := Result + ' end';
end;

end.
