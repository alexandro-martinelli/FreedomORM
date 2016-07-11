unit AM.Freedom.TextGenerator.GroupCriteriaTextGenerator;

interface

uses
  AM.Freedom.GroupCriteria,
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TGroupCriteriaTextGenerator = class(TCustomTextGenerator)
  strict private
    procedure BeginUpdate(var pResult: String; pGroupCriteria: TGroupCriteria; aIsFirst: Boolean);
    procedure DoUpdate(var pResult: String; pGroupCriteria: TGroupCriteria);
    procedure EndUpdate(var pResult: String; pGroupCriteria: TGroupCriteria);
    function GenerateTextCriteria(pCriteria: TCriteria): String;
    function GenerateTextGroupCriteria(pGroupCriteria: TGroupCriteria; aIsFirst: Boolean): string;
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; override;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.Helper.Policy;

{ TGroupCriteriaTextGenerator }

function TGroupCriteriaTextGenerator.GenerateTextGroupCriteria(pGroupCriteria: TGroupCriteria; aIsFirst: Boolean): string;
begin
  Result := EmptyStr;
  if (not pGroupCriteria.IsEmpty) then
  begin
    BeginUpdate(Result, pGroupCriteria, aIsFirst);
    DoUpdate(Result, pGroupCriteria);
    EndUpdate(Result, pGroupCriteria);
  end;
  if (Result = '()') then
  begin
    Result := '';
  end;
end;

procedure TGroupCriteriaTextGenerator.BeginUpdate(var pResult: String; pGroupCriteria: TGroupCriteria; aIsFirst: Boolean);
begin
  if (pGroupCriteria.Count >= 1) then
    pResult := pResult + '(';
end;

procedure TGroupCriteriaTextGenerator.DoUpdate(var pResult: String; pGroupCriteria: TGroupCriteria);
var
  I: Integer;
  lTextCriteria: string;
  function InsertPolicyIfNot: String;
  begin
    Result := '';
    if ContainsText(pGroupCriteria.Policy.ToString, 'not') then
    begin
      Result := ' not ';
    end;
  end;
begin
  if pGroupCriteria.ListCriterias.Count > 1 then
  begin
    pResult := pResult + '(';
  end;
  for I := 0 to pGroupCriteria.ListCriterias.Count - 1 do
  begin
    lTextCriteria := GenerateTextCriteria(pGroupCriteria.ListCriterias.Items[I]);
    if (lTextCriteria <> '') then
    begin
      if I > 0 then
      begin
        pResult := pResult + pGroupCriteria.Policy.ToString + lTextCriteria;
      end else
      begin
        pResult := pResult + ifthen((pResult <> EmptyStr) and (pResult[Length(pResult)] <> '('), ' ') +
            InsertPolicyIfNot + lTextCriteria;
      end;
    end;
  end;
  if (pGroupCriteria.ListCriterias.Count > 1) then
  begin
    pResult := pResult + ')';
  end;
end;

procedure TGroupCriteriaTextGenerator.EndUpdate(var pResult: String; pGroupCriteria: TGroupCriteria);
var
  lSearch: TGroupCriteria;
  lTextGroupCriteria: string;
begin
  for lSearch in pGroupCriteria do
  begin
    lTextGroupCriteria := GenerateTextGroupCriteria(lSearch, False);
    if (lTextGroupCriteria <> '') then
    begin
      if (pResult <> EmptyStr) and (pResult <> '(') then
      begin
        pResult := pResult + pGroupCriteria.Policy.ToString;
      end;
      pResult := pResult + lTextGroupCriteria;
    end;
  end;
  if pGroupCriteria.Count >= 1 then
  begin
    pResult := pResult + ')';
  end;
end;

function TGroupCriteriaTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lGroupCriteria: TGroupCriteria;
begin
  Result := EmptyStr;
  lGroupCriteria := TGroupCriteria(pObject);
  if Assigned(lGroupCriteria) then
  begin
    Result := GenerateTextGroupCriteria(lGroupCriteria, True);
  end;
end;

function TGroupCriteriaTextGenerator.GenerateTextCriteria(pCriteria: TCriteria): String;
begin
  Result := GetTextFromGenerator(pCriteria, nil);
end;

end.
