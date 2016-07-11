unit AM.Freedom.ExtractMethodKeyExecutor;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.FreedomBindKeyBoard,
  UnitReader.UnitElement,
  UnitReader.ClassElement,
  ToolsAPI;

type
  TExtractMethodKeyExecutor = class(TCustomKeyExecutor)
  strict private
    FClassElement: TClassElement;
    FStartPos: TOTAEditPos;
    FContext: IOTAKeyContext;
    FUnitElement: TUnitElement;
    FEditPosition: IOTAEditPosition;
  private
    FLine: Integer;
  protected
    function GetShortCut: TShortCut; override;
  public
    class function Description: string; override;
    procedure Execute(const pContext: IOTAKeyContext); override;
  end;


implementation

{ TExtractConstantKeyExecutor }

uses
  AM.Freedom.frmExtractMethod,
  AM.Freedom.dclFreedomORMConfig,
  AM.UnitReader.Helper.MemberVisibility;

class function TExtractMethodKeyExecutor.Description: string;
begin
  Result := 'Extract method';
end;

procedure TExtractMethodKeyExecutor.Execute(const pContext: IOTAKeyContext);
var
  lSelectedBlockText: String;
  lExtractMethodResult: TExtractMethodResult;
  lConstantDeclaration: TVisibilityScopeDeclaration;
  lEndMethod: Integer;
  lBlockStart: TOTACharPos;
  lEndBlockPos: TOTAEditPos;
  lLastSelectionIsLineDown: Boolean;
begin
  FContext := pContext;
  FEditPosition := FContext.EditBuffer.EditPosition;
  try
    FStartPos := FContext.EditBuffer.TopView.CursorPos;
    lSelectedBlockText := GetSelectedBlockText(FContext);
    if (lSelectedBlockText = '') then
    begin
      FixSelectedBlock(FContext);
      lSelectedBlockText := GetSelectedBlockText(FContext);
    end;
    lBlockStart := pContext.EditBuffer.BlockStart;
    pContext.EditBuffer.TopView.ConvertPos(False, FStartPos, lBlockStart);
    lBlockStart := pContext.EditBuffer.BlockAfter;
    pContext.EditBuffer.TopView.ConvertPos(False, lEndBlockPos, lBlockStart);
    lLastSelectionIsLineDown := lEndBlockPos.Col = 1;
    FUnitElement := GetUnitElement(FContext);
    FClassElement := FUnitElement.ClassDeclarationOnLineNumber(FStartPos.Line);
    lExtractMethodResult := TfrmExtractMethod.ExtractMethod(lSelectedBlockText, FClassElement, FUnitElement);
    if (Assigned(lExtractMethodResult)) then
    begin
      FContext.EditBuffer.EditBlock.Delete;
      FStartPos := FContext.EditBuffer.TopView.CursorPos;
      FLine := FStartPos.Line;
      if (lExtractMethodResult.DeclareOnLine = 0) then
      begin
        lConstantDeclaration := FClassElement.ExtractLineForVisibilityScope(lExtractMethodResult.VisibilityScope);
        try
          FEditPosition.GotoLine(lConstantDeclaration.LineNumber);
          FEditPosition.MoveEOL;
          if (not lConstantDeclaration.IsDeclaration) then
          begin
            FEditPosition.InsertText(sLineBreak);
            Inc(FLine);
            FEditPosition.MoveBOL;
            FEditPosition.InsertText('  ' + lExtractMethodResult.VisibilityScope.ToString);
            FEditPosition.MoveEOL;
          end;
        finally
          lConstantDeclaration.Free;
        end;
      end
      else
      begin
        FEditPosition.GotoLine(lExtractMethodResult.DeclareOnLine);
        FEditPosition.MoveEOL;
      end;
      FEditPosition.InsertText(sLineBreak);
      Inc(FLine);
      FEditPosition.InsertText(lExtractMethodResult.MethodDeclaration);
      FStartPos.Line := FLine;
      GoBackToPos(FContext, FStartPos);
      FEditPosition.MoveEOL;
      while FEditPosition.Column = 1 do
      begin
        FEditPosition.BackspaceDelete(1);
        FEditPosition.MoveEOL;
      end;
      if (lLastSelectionIsLineDown) then
      begin
        FEditPosition.MoveRelative(-1, 0);
        FEditPosition.MoveEOL;
      end;
      FEditPosition.InsertText(sLineBreak);
      FEditPosition.InsertText(lExtractMethodResult.MethodResultName + ';');
      FUnitElement.Free;
      FUnitElement := GetUnitElement(FContext);
      lEndMethod := FUnitElement.EndMethodImplementationAfterLineIndex(FStartPos.Line);
      FEditPosition.GotoLine(lEndMethod);
      FEditPosition.MoveEOL;
      FContext.EditBuffer.EditOptions.BufferOptions.AutoIndent := False;
      try
        FEditPosition.InsertText(sLineBreak);
        FEditPosition.MoveBOL;
        FEditPosition.InsertText(sLineBreak + lExtractMethodResult.MethodImplemetation);
        FEditPosition.MoveEOL;
        GoBackToPos(FContext, FStartPos);
      finally
        FContext.EditBuffer.EditOptions.BufferOptions.AutoIndent := True;
      end;
      FContext.EditBuffer.TopView.Paint;
    end;
  finally
    FreeAndnil(FUnitElement);
    FEditPosition := nil;
    FContext := nil;
  end;
end;

function TExtractMethodKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.ExtractMethod;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TExtractMethodKeyExecutor);

end.