unit AM.Freedom.DeclareMethodKeyExecutor;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.FreedomBindKeyBoard,
  UnitReader.UnitElement,
  UnitReader.ClassElement,
  ToolsAPI;

type
  TDeclareMethodKeyExecutor = class(TCustomKeyExecutor)
  strict private
    FClassElement: TClassElement;
    FStartPos: TOTAEditPos;
    FContext: IOTAKeyContext;
    FUnitElement: TUnitElement;
    FEditPosition: IOTAEditPosition;
    FLine: Integer;
  protected
    function GetShortCut: TShortCut; override;
  public
    class function Description: string; override;
    procedure Execute(const pContext: IOTAKeyContext); override;
  end;

implementation

{ TDeclareMethodKeyExecutor }

uses
  AM.Freedom.dclFreedomORMConfig,
  AM.Freedom.frmExtractMethod,
  AM.UnitReader.Helper.MemberVisibility;

class function TDeclareMethodKeyExecutor.Description: string;
begin
  Result := 'Declare method';
end;

procedure TDeclareMethodKeyExecutor.Execute(const pContext: IOTAKeyContext);
var
  lSelectedBlockText: String;
  lMethodResult: TExtractMethodResult;
  lDeclaration: TVisibilityScopeDeclaration;
  lEndMethod: Integer;
  lCanContinue: Boolean;
  lSpaces: String;
begin
  FContext := pContext;
  FEditPosition := FContext.EditBuffer.EditPosition;
  try
    FStartPos := FContext.EditBuffer.TopView.CursorPos;
    FLine := FStartPos.Line;
    lSelectedBlockText := GetSelectedBlockText(FContext);
    lCanContinue := True;
    if (lSelectedBlockText = '') then
    begin
      lSelectedBlockText := ExtractIdentifierAtCursor(FContext, lCanContinue, True);
    end;
    if (lSelectedBlockText <> '') and lCanContinue then
    begin
      FUnitElement := GetUnitElement(FContext);
      try
        FClassElement := FUnitElement.ClassDeclarationOnLineNumber(FStartPos.Line);
        lMethodResult := TfrmExtractMethod.DeclareMethod(lSelectedBlockText, FClassElement, FUnitElement);
        try
          if (Assigned(lMethodResult)) then
          begin
            lSpaces := '';
            if (lMethodResult.DeclareOnLine = 0) then
            begin
              lDeclaration := FClassElement.ExtractLineForVisibilityScope(lMethodResult.VisibilityScope);
              try
                FEditPosition.GotoLine(lDeclaration.LineNumber);
                FEditPosition.MoveEOL;
                if (not lDeclaration.IsDeclaration) then
                begin
                  FEditPosition.InsertText(sLineBreak);
                  Inc(FLine);
                  FEditPosition.MoveBOL;
                  FEditPosition.InsertText('  ' + lMethodResult.VisibilityScope.ToString);
                  FEditPosition.MoveEOL;
                  lSpaces := StringOfChar(' ', StrToInt(FContext.EditBuffer.EditOptions.BufferOptions.TabStops));
                end;
              finally
                lDeclaration.Free;
              end;
            end
            else
            begin
              FEditPosition.GotoLine(lMethodResult.DeclareOnLine);
              FEditPosition.MoveEOL;
            end;
            FEditPosition.InsertText(sLineBreak);
            Inc(FLine);
            FEditPosition.InsertText(lSpaces + lMethodResult.MethodDeclaration);
            FStartPos.Line := FLine;
            GoBackToPos(FContext, FStartPos);
            FUnitElement.Free;
            FUnitElement := GetUnitElement(FContext);
            lEndMethod := FUnitElement.EndMethodImplementationAfterLineIndex(FStartPos.Line);
            FEditPosition.GotoLine(lEndMethod);
            FEditPosition.MoveEOL;
            FContext.EditBuffer.EditOptions.BufferOptions.AutoIndent := False;
            try
              FEditPosition.MoveEOL;
              FEditPosition.InsertText(sLIneBreak + sLineBreak + lMethodResult.MethodImplemetation);
              FEditPosition.MoveRelative(-1, 0);
              FEditPosition.MoveBOL;
              FStartPos := FContext.EditBuffer.TopView.CursorPos;
              FContext.EditBuffer.TopView.Center(FStartPos.Line, FStartPos.Col);
            finally
              FContext.EditBuffer.EditOptions.BufferOptions.AutoIndent := True;
            end;
            FContext.EditBuffer.MarkModified;
          end;
        finally
          FreeAndNil(lMethodResult);
        end;
      finally
        FUnitElement.Free;
      end;
    end;
  finally
    FEditPosition := nil;
    FContext := nil;
  end;
end;

function TDeclareMethodKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.DeclareMethod;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TDeclareMethodKeyExecutor);

end.