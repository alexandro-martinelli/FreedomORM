unit AM.Freedom.ViewRefactoringsKeyExecutor;

interface

uses
  System.SysUtils,
  AM.Freedom.FreedomBindKeyBoard,
  AM.Freedom.dclFreedomORMConfig,
  ToolsAPI,
  System.Classes;

type
  TViewRefactoringsKeyExecutor = class(TCustomKeyExecutor)
  protected
    function GetShortCut: TShortCut; override;
  public
    class function Description: string; override;
    procedure Execute(const pContext: IOTAKeyContext); override;
  end;

implementation

{ TViewRefactoringsKeyExecutor }

uses
  AM.Freedom.frmViewRefactorings;

class function TViewRefactoringsKeyExecutor.Description: string;
begin
  Result := '';
end;

procedure TViewRefactoringsKeyExecutor.Execute(const pContext: IOTAKeyContext);
var
  lKeyExecutor: TCustomKeyExecutor;
begin
  lKeyExecutor := TfrmViewRefactorings.ViewRefactorings;
  if (Assigned(lKeyExecutor)) then
  begin
    lKeyExecutor.Execute(pContext);
  end;
end;

function TViewRefactoringsKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.ViewRefactorings;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TViewRefactoringsKeyExecutor);

end.