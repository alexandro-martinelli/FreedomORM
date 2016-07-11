unit AM.Freedom.ReverseSentenceKeyExecutor;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.FreedomBindKeyBoard,
  AM.Freedom.dclFreedomORMConfig,
  ToolsAPI;

type
  TReverseSentenceKeyExecutor = class(TCustomKeyExecutor)
  private
    function ReverseSentence(const pSentence: String): String;
    function ReverseStrSentence(const pSentence: String): String;
  protected
    function GetShortCut: TShortCut; override;
  public
    class function Description: string; override;
    procedure Execute(const pContext: IOTAKeyContext); override;
  end;


implementation

uses
  System.StrUtils;

{ TInvertSentenceKeyExecutor }

class function TReverseSentenceKeyExecutor.Description: string;
begin
  Result := 'Reverse sentence'
end;

procedure TReverseSentenceKeyExecutor.Execute(const pContext: IOTAKeyContext);
var
  lSelectedText: String;
begin
  lSelectedText := GetSelectedBlockText(pContext);
  lSelectedText := ReverseSentence(lSelectedText);
  pContext.EditBuffer.EditPosition.InsertText(lSelectedText);
end;

function TReverseSentenceKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.ReverseSentence;
end;

function TReverseSentenceKeyExecutor.ReverseStrSentence(const pSentence: String): String;
var
  lLeftSentence, lRigthSentence: string;
  lInitialPos: Integer;
begin
  Result := pSentence;
  if (Pos(':=', pSentence) > 0) then
  begin
    lLeftSentence := Trim(Copy(pSentence, 1, Pos(':=', pSentence) - 1));
    lInitialPos := Pos(':=', pSentence) + 2;
    lRigthSentence := Trim(Copy(pSentence, lInitialPos,  length(pSentence) - (lInitialPos - 1)));
    Result := Format('%s := %s', [lRigthSentence, lLeftSentence]);
  end;
end;

function TReverseSentenceKeyExecutor.ReverseSentence(const pSentence: String): String;
var
  lSentence: TStrings;
  lStrSentence: String;
begin
  Result := '';
  lSentence := TStringList.Create;
  try
    ExtractStrings([';'], [' '], PWideChar(pSentence), lSentence);
    for lStrSentence in lSentence do
    begin
      Result := Result + ifthen(Result <> '', sLineBreak) + ReverseStrSentence(lStrSentence) + ';';
    end;
  finally
    lSentence.Free;
  end;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TReverseSentenceKeyExecutor);

end.