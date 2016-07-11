unit AM.Freedom.CustomUnitKeyExecutor;

interface

uses
  System.SysUtils,
  AM.Freedom.FreedomBindKeyBoard,
  UnitReader.UnitElement,
  ToolsAPI,
  AM.Freedom.frmUnitNames,
  System.Classes,
  UnitReader.EnumerationTypes;

type
  TCustomUnitKeyExecutor = class(TCustomKeyExecutor)
  private
    FUnitElement: TUnitElement;
  protected
    function GetUnitDeclaration(pAlreadDeclaredUnits: TStrings; pCurrentSection: TTokenType): TUnitDeclaration; virtual;
  public
    procedure Execute(const pContext: IOTAKeyContext); override;
  end;


implementation

uses
  System.StrUtils,
  AM.Freedom.Exceptions;

{ TCustomUnitKeyExecutor }

procedure TCustomUnitKeyExecutor.Execute(const pContext: IOTAKeyContext);
var
  lUnitDeclaration: TUnitDeclaration;
  lUsesDeclaration: TUsesDeclarationResult;
  lStartPos: TOTAEditPos;
  lLine: Integer;
  lDeclaredUnits: TStrings;
  lUnit: String;
  lIndex: Integer;
  lSection: TTokenType;
begin
  lStartPos := pContext.EditBuffer.TopView.CursorPos;
  lLine := lStartPos.Line;
  FUnitElement := GetUnitElement(pContext);
  lUnitDeclaration := nil;
  try
    lDeclaredUnits := FUnitElement.UsesList;
    try
      lSection := FUnitElement.SectionByLineNumber(lLine);
      lUnitDeclaration := GetUnitDeclaration(lDeclaredUnits, lSection);
    finally
      lDeclaredUnits.Free;
    end;
    if Assigned(lUnitDeclaration) then
    begin
      lUsesDeclaration := FUnitElement.EndOfUsesDeclaration(lUnitDeclaration.DeclareOnInterface);
      pContext.EditBuffer.EditPosition.GotoLine(lUsesDeclaration.LineNumber);
      if (lUsesDeclaration.IsDeclaration) then
      begin
        for lUnit in lUnitDeclaration.DeclareUnitNames do
        begin
          pContext.EditBuffer.EditPosition.MoveEOL;
          pContext.EditBuffer.EditPosition.MoveRelative(0, -1);
          pContext.EditBuffer.EditPosition.InsertText(',' +
            ifthen(lUnitDeclaration.DeclareOnNewLine, sLineBreak, ' ') + lUnit);
          Inc(lLine);
        end;
      end
      else
      begin
        pContext.EditBuffer.EditPosition.MoveEOL;
        pContext.EditBuffer.EditOptions.BufferOptions.AutoIndent := False;
        try
          pContext.EditBuffer.EditPosition.InsertText(sLineBreak + sLineBreak);
          Inc(lLine, 2);
          pContext.EditBuffer.EditPosition.MoveBOL;
          pContext.EditBuffer.EditPosition.InsertText('uses' + sLineBreak);
          Inc(lLine);
          pContext.EditBuffer.EditPosition.MoveBOL;
          pContext.EditBuffer.EditPosition.InsertText('  ' + lUnitDeclaration.DeclareUnitNames.Strings[0] + ';');
        finally
          pContext.EditBuffer.EditOptions.BufferOptions.AutoIndent := True;
        end;
        for lIndex := 1 to lUnitDeclaration.DeclareUnitNames.Count - 1 do
        begin
          lUnit := lUnitDeclaration.DeclareUnitNames.Strings[lIndex];
          pContext.EditBuffer.EditPosition.MoveEOL;
          pContext.EditBuffer.EditPosition.MoveRelative(0, -1);
          pContext.EditBuffer.EditPosition.InsertText(',' +
            ifthen(lUnitDeclaration.DeclareOnNewLine, sLineBreak, ' ') + lUnit);
          Inc(lLine);
        end;
      end;
      lStartPos.Line := lLine;
      GoBackToPos(pContext, lStartPos);
      pContext.EditBuffer.TopView.Center(lStartPos.Line, lStartPos.Col);
    end;
  finally
    FUnitElement.Free;
    FreeAndNil(lUnitDeclaration);
  end;
end;

function TCustomUnitKeyExecutor.GetUnitDeclaration(pAlreadDeclaredUnits: TStrings;
  pCurrentSection: TTokenType): TUnitDeclaration;
begin
  raise EInvalidMethodCallOnClass.Create('GetUnitDeclaration', ClassName);
end;

end.
