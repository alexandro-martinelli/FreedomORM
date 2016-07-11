unit AM.Freedom.ExtractVariableKeyExecutor;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.FreedomBindKeyBoard,
  AM.Freedom.frmDeclareVariable,
  UnitReader.UnitElement,
  ToolsAPI;

type
  TExtractVariableKeyExecutor = class(TCustomKeyExecutor)
  private
    FLine: Integer;
    FStartPos: TOTAEditPos;
    FContext: IOTAKeyContext;
    FVariable: TDeclareVariable;
    FUnitElement: TUnitElement;
    FVarValue: string;
    FVariableDeclaration: TVariableDeclarationResult;
    FEditPosition: IOTAEditPosition;
    function ExtractVarType(pVarValue: string): string;
    procedure DoExtractVariable;
//    procedure ExtractTextOccurences;
  protected
    function GetShortCut: TShortCut; override;
  public
    class function Description: string; override;
    procedure Execute(const pContext: IOTAKeyContext); override;
  end;

implementation

uses
  System.StrUtils,
  AM.Freedom.dclFreedomORMConfig,
  Vcl.Forms,
  Winapi.Windows;
{ TExtractVariableKeyExecutor }

class function TExtractVariableKeyExecutor.Description: string;
begin
  Result := 'Extract variable';
end;

procedure TExtractVariableKeyExecutor.DoExtractVariable;
var
  lBeginLine: Integer;
begin
  FEditPosition.GotoLine(FVariableDeclaration.LineNumber);
  FEditPosition.MoveEOL;
  FEditPosition.InsertText(sLineBreak);
  Inc(FLine);
  if (not FVariableDeclaration.IsDeclaration) then
  begin
    FEditPosition.InsertText('var' + sLineBreak);
    Inc(FLine);
  end;
  FEditPosition.InsertText(Format('%s%s: %s;', [ifthen(not FVariableDeclaration.IsDeclaration, '  '),  FVariable.VarName, FVariable.VarType]));
  if (not FVariable.InitializeBeforeCurrentRow) then
  begin
    lBeginLine := FUnitElement.BeginMethodImplementationBeforeLineIndex(FStartPos.Line) + FLine;
    FEditPosition.GotoLine(lBeginLine);
    FEditPosition.MoveEOL;
    FEditPosition.InsertText(sLineBreak + Format('  %s := %s;', [FVariable.VarName, FVarValue]));
    Inc(FLine);
  end;
  FStartPos.Line := FStartPos.Line + FLine;
  GoBackToPos(FContext, FStartPos);
  FEditPosition.InsertText(FVariable.VarName);
  if (FVariable.InitializeBeforeCurrentRow) then
  begin
    FEditPosition.MoveRelative(-1, 0);
    FEditPosition.MoveEOL;
    FEditPosition.InsertText(sLineBreak + Format('%s := %s;', [FVariable.VarName, FVarValue]));
    FStartPos.Line := FStartPos.Line + 1;
    GoBackToPos(FContext, FStartPos);
  end;
//  ExtractTextOccurences;
  FContext.EditBuffer.TopView.Center(FStartPos.Line, FStartPos.Col);
end;

procedure TExtractVariableKeyExecutor.Execute(const pContext: IOTAKeyContext);
var
  lVarType: string;
begin
  FContext := pContext;
  FEditPosition := FContext.EditBuffer.EditPosition;
  FLine := 0;
  FVarValue := GetSelectedBlockText(FContext);
  if (FVarValue <> '') then
  begin
    lVarType := ExtractVarType(FVarValue);
    FVariable := TfrmDeclareVariable.ExtractVariable('', lVarType);
    try
      if (FVariable.VarName <> '') then
      begin
        FContext.EditBuffer.EditBlock.Delete;
        FStartPos := FContext.EditBuffer.TopView.CursorPos;
        FUnitElement := GetUnitElement(FContext);
        try
          if not FUnitElement.VariableDeclarationExistsBeforeLineIndex(FStartPos.Line, FVariable.VarName) then
          begin
            FVariableDeclaration := FUnitElement.VariableDeclarationBeforeLineIndex(FStartPos.Line);
            try
              if (FVariableDeclaration.LineNumber > 0) then
              begin
                DoExtractVariable;
              end;
            finally
              FVariableDeclaration.Free;
            end;
          end;
        finally
          FUnitElement.Free;
        end;
      end;
    finally
      FVariable.Free;
      FContext := nil;
      FEditPosition := nil;
    end;
  end;
end;

{procedure TExtractVariableKeyExecutor.ExtractTextOccurences;
var
  lBeginSelectionEdit, lEndSelectionEdit: TOTAEditPos;
  lBeginSeletion, lEndSeletion: TOTACharPos;
  lSelectedText: String;
  lEditBuffer: IOTAEditBuffer;
begin
  lEditBuffer := FContext.EditBuffer;
  if (Application.MessageBox(PWideChar('Do you want to overwrite all occurrences of this text?'), 'Extract Variable', MB_YESNO + MB_DEFBUTTON2) = IDYES) then
  begin
    FEditPosition.MoveRelative(1, 0);
    FEditPosition.MoveBOL;
    lEditBuffer.EditOptions.BufferOptions.AutoIndent := False;
    try
      lBeginSelectionEdit := lEditBuffer.TopView.CursorPos;
      lEditBuffer.TopView.ConvertPos(True, lBeginSelectionEdit, lBeginSeletion);
      FLine := FUnitElement.EndMethodImplementationAfterLineIndex(lEditBuffer.TopView.CursorPos.Line - FLine) + FLine;
      FEditPosition.GotoLine(FLine - 1);
      FEditPosition.MoveEOL;
      lEndSelectionEdit := lEditBuffer.TopView.CursorPos;
      lEditBuffer.TopView.ConvertPos(True, lEndSelectionEdit, lEndSeletion);
      lSelectedText := GetBlockText(FContext,  lBeginSeletion, lEndSeletion);
      lSelectedText := ReplaceText(lSelectedText, FVarValue, FVariable.VarName);
      lEditBuffer.EditBlock.Delete;
      FEditPosition.MoveBOL;
      lEditBuffer.EditPosition.InsertText(lSelectedText);
      GoBackToPos(FContext, FStartPos);
    finally
      lEditBuffer.EditOptions.BufferOptions.AutoIndent := True;
    end;
  end;
end;}

function TExtractVariableKeyExecutor.ExtractVarType(pVarValue: string): string;
var
  lBoolean: Boolean;
  lInt: Integer;
  lInt64: Int64;
  lFloat: Extended;
begin
  if (StartsText('''', pVarValue) and EndsText('''', pVarValue)) then
  begin
    if (Length(pVarValue) > 1) then
    begin
      Result := 'String';
    end
    else
    begin
      Result := 'Char';
    end;
  end
  else if (TryStrToBool(pVarValue, lBoolean)) then
  begin
    Result := 'Boolean';
  end
  else if (TryStrToInt64(pVarValue, lInt64)) then
  begin
    Result := 'Int64';
  end
  else if (TryStrToInt(pVarValue, lInt)) then
  begin
    Result := 'Integer';
  end
  else if (TryStrToFloat(pVarValue, lFloat)) then
  begin
    Result := 'Extended';
  end
  else
  begin
    Result := 'TObject';
  end;
end;

function TExtractVariableKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.ExtractVariable;
end;

initialization

TFreedomBindKeyBoard.RegisterKeyExecutor(TExtractVariableKeyExecutor);

end.
