unit AM.Freedom.MoveBlockDownKeyExecutor;

interface

uses
  System.SysUtils,
  AM.Freedom.FreedomBindKeyBoard,
  AM.Freedom.dclFreedomORMConfig,
  ToolsAPI,
  System.Classes;

type
  TMoveBlockDownKeyExecutor = class(TCustomKeyExecutor)
  protected
    function GetShortCut: TShortCut; override;
  public
    class function Description: string; override;
    procedure Execute(const pContext: IOTAKeyContext); override;
  end;

implementation

{ TMoveBlockDownKeyExecutor }

class function TMoveBlockDownKeyExecutor.Description: string;
begin
  Result := 'Move block/line down';
end;

procedure TMoveBlockDownKeyExecutor.Execute(const pContext: IOTAKeyContext);
var
  lBlockText: string;
  lEditPos, lInitialEditPos: TOTAEditPos;
  lCharPos, lInitialCharPos: TOTACharPos;
begin
  pContext.EditBuffer.EditOptions.BufferOptions.AutoIndent := False;
  try
    FixSelectedBlock(pContext);
    lBlockText := GetSelectedBlockText(pContext);
    pContext.EditBuffer.EditBlock.Delete;
    pContext.EditBuffer.EditPosition.MoveBOL;
    pContext.EditBuffer.EditPosition.MoveRelative(1, 0);
    pContext.EditBuffer.EditPosition.BackspaceDelete(1);
    pContext.EditBuffer.EditPosition.MoveEOL;
    pContext.EditBuffer.EditPosition.InsertText(sLineBreak);
    pContext.EditBuffer.EditPosition.MoveBOL;
    lInitialEditPos := pContext.EditBuffer.TopView.CursorPos;
    pContext.EditBuffer.TopView.ConvertPos(True, lInitialEditPos, lInitialCharPos);
    pContext.EditBuffer.EditPosition.InsertText(lBlockText);
    pContext.EditBuffer.EditPosition.MoveEOL;
    lEditPos := pContext.EditBuffer.TopView.CursorPos;
    pContext.EditBuffer.BlockStart := lInitialCharPos;
    pContext.EditBuffer.TopView.ConvertPos(True, lEditPos, lCharPos);
    pContext.EditBuffer.BlockAfter := lCharPos;
    pContext.EditBuffer.BlockType := btNonInclusive;
    pContext.EditBuffer.BlockVisible := False;
    pContext.EditBuffer.BlockVisible := True;
  finally
    pContext.EditBuffer.EditOptions.BufferOptions.AutoIndent := True;
  end;
end;

function TMoveBlockDownKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.MoveBlockDown;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TMoveBlockDownKeyExecutor);

end.

