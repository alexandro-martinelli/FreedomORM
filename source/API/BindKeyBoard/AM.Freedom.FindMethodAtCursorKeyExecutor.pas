unit AM.Freedom.FindMethodAtCursorKeyExecutor;

interface

uses
  System.SysUtils,
  AM.Freedom.FreedomBindKeyBoard,
  AM.Freedom.dclFreedomORMConfig,
  System.Classes,
  AM.Freedom.CustomFindMethodKeyExecutor;

type
  TFindMethodAtCursorKeyExecutor = class(TCustomFindMethodKeyExecutor)
  protected
    function GetShortCut: TShortCut; override;
    function FindLineNumber: Integer; override;
  public
    class function Description: string; override;
  end;

implementation

{ TFindMethodAtCursorKeyExecutor }

class function TFindMethodAtCursorKeyExecutor.Description: string;
begin
  Result := 'Find Method at cursor';
end;

function TFindMethodAtCursorKeyExecutor.FindLineNumber: Integer;
var
  lSelectedText: String;
  lCanContinue: Boolean;
begin
  Result := 0;
  lSelectedText := GetSelectedBlockText(Context);
  if (lSelectedText = '') then
  begin
    lSelectedText := ExtractIdentifierAtCursor(Context, lCanContinue, False);
  end;
  if (lSelectedText <> '') then
  begin
    Result := UnitElement.BeginMethodImplementationWithMethodName(lSelectedText);
  end;
end;

function TFindMethodAtCursorKeyExecutor.GetShortCut: TShortCut;
begin
  Result := 0;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TFindMethodAtCursorKeyExecutor);

end.