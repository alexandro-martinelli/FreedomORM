unit AM.Freedom.TextGenerator.CriteriaTextGenerator;

interface

uses
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.SQLMapper.CustomArgument;

type
  TCriteriaTextGenerator = class(TCustomTextGenerator)
  strict private
    function GenerateTextArgument(pArgument: TCustomArgument; pParams: TGenerateTextParams = nil): String;
    procedure DoGenerate(var pResult: String; pCriteria: TCriteria);
    procedure EndGenerate(var pResult: String);
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Helper.Variant, AM.Freedom.SQLMappers.CustomSelect;

{ TCriteriaTextGenerator }

procedure TCriteriaTextGenerator.DoGenerate(var pResult: String; pCriteria: TCriteria);
var
  lResult: String;
begin
  if Assigned(pCriteria.LeftArgument) then
  begin
    pResult := GenerateTextArgument(pCriteria.LeftArgument, TGenerateTextParams.Create(False, pCriteria.LeftArgument.InheritsFrom(TCustomSelect)));
  end else
  begin
    pResult := EmptyStr;
  end;
  lResult := Format(pCriteria.Comparator.Comparator,
      [GenerateTextArgument(pCriteria.RigthArgument,
      TGenerateTextParams.Create(False, pCriteria.RigthArgument.InheritsFrom(TCustomSelect),
      pCriteria.Comparator.ComparatorType))]);
  if (SameText(lResult, '= null')) then
  begin
    lResult := 'is null';
  end
  else if (SameText(lResult, '<> null')) then
  begin
    lResult := 'is not null';
  end;
  pResult := pResult + ifthen(pResult <> EmptyStr, ' ') + lResult;
end;

procedure TCriteriaTextGenerator.EndGenerate(var pResult: String);
begin
  pResult := '(' + pResult + ')';
end;

function TCriteriaTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lCriteria: TCriteria;
begin
  lCriteria := TCriteria(pObject);
  Result := EmptyStr;
  DoGenerate(Result, lCriteria);
  EndGenerate(Result);
end;

function TCriteriaTextGenerator.GenerateTextArgument(pArgument: TCustomArgument; pParams: TGenerateTextParams): String;
begin
  Result := GetTextFromGenerator(pArgument, pParams);
end;

end.

