unit AM.Freedom.MoveBlockUpKeyExecutor;

interface

uses
  System.SysUtils,
  AM.Freedom.FreedomBindKeyBoard,
  AM.Freedom.dclFreedomORMConfig,
  ToolsAPI,
  System.Classes;

type
  TMoveBlockUpKeyExecutor = class(TCustomKeyExecutor)
  protected
    function GetShortCut: TShortCut; override;
  public
    class function Description: string; override;
    procedure Execute(const pContext: IOTAKeyContext); override;
  end;

implementation

uses
  System.StrUtils;

{ TMoveBlockKeyExecutor }

class function TMoveBlockUpKeyExecutor.Description: string;
begin
  Result := 'Move block/line up';
end;

procedure TMoveBlockUpKeyExecutor.Execute(const pContext: IOTAKeyContext);
var
  lBlockText: String;
  lEditPos, lInitialEditPos: TOTAEditPos;
  lCharPos, lInitialCharPos: TOTACharPos;
begin
  pContext.EditBuffer.EditOptions.BufferOptions.AutoIndent := False;
  try
    FixSelectedBlock(pContext);
    lBlockText := GetSelectedBlockText(pContext);
    pContext.EditBuffer.EditBlock.Delete;
    pContext.EditBuffer.EditPosition.MoveBOL;
    pContext.EditBuffer.EditPosition.BackspaceDelete(1);
    lInitialEditPos := pContext.EditBuffer.TopView.CursorPos;
    pContext.EditBuffer.TopView.ConvertPos(True, lInitialEditPos, lInitialCharPos);
    pContext.EditBuffer.EditPosition.MoveBOL;
    pContext.EditBuffer.EditPosition.InsertText(sLineBreak);
    pContext.EditBuffer.EditPosition.MoveRelative(-1, 0);
    pContext.EditBuffer.EditPosition.MoveBOL;
    lInitialEditPos := pContext.EditBuffer.TopView.CursorPos;
    pContext.EditBuffer.TopView.ConvertPos(True, lInitialEditPos, lInitialCharPos);
    pContext.EditBuffer.EditPosition.InsertText(lBlockText);
    lEditPos := pContext.EditBuffer.TopView.CursorPos;
    pContext.EditBuffer.TopView.ConvertPos(True, lEditPos, lCharPos);
    pContext.EditBuffer.BlockStart := lInitialCharPos;
    pContext.EditBuffer.BlockAfter := lCharPos;
    pContext.EditBuffer.BlockType := btNonInclusive;
    pContext.EditBuffer.BlockVisible := True;
  finally
    pContext.EditBuffer.EditOptions.BufferOptions.AutoIndent := True;
  end;
end;

function TMoveBlockUpKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.MoveBlockUp;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TMoveBlockUpKeyExecutor);

end.
