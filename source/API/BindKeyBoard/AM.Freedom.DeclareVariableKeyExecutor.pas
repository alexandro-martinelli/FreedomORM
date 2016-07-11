unit AM.Freedom.DeclareVariableKeyExecutor;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.FreedomBindKeyBoard,
  ToolsAPI;

type
  TDeclareVariableKeyExecutor = class(TCustomKeyExecutor)
  private const
    cInvalidVarNames: Array [0..7] of String = ('Now', 'Date', 'Time', 'TClass', 'TObject', 'begin', 'end', 'class');
  private
    function VarNameIsCorrect(pVarName: String): Boolean;
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
  UnitReader.UnitElement,
  AM.Freedom.frmDeclareVariable,
  AM.Freedom.Consts;

{ TDeclareVariableKeyExecutor }

class function TDeclareVariableKeyExecutor.Description: string;
begin
  Result := 'Declare variable';
end;

procedure TDeclareVariableKeyExecutor.Execute(const pContext: IOTAKeyContext);
var
  LStartPos: TOTAEditPos;
  lVarName: String;
  lUnitElement: TUnitElement;
  lLine: Integer;
  lVariable: TVariableDeclarationResult;
  lResult: TDeclareVariable;
  lInitialValue: String;
  lCanContinue: Boolean;
begin
  try
    lResult := nil;
    lCanContinue := True;
    LStartPos := pContext.EditBuffer.TopView.CursorPos;
    lVarName := GetSelectedBlockText(pContext);
    lLine := 0;
    if (lVarName = '') then
    begin
      lVarName := ExtractIdentifierAtCursor(pContext, lCanContinue);
      if (lCanContinue) then
      begin
        if (lVarName = '') then
        begin
          lResult := TfrmDeclareVariable.DeclareVariable(lVarName);
          lVarName := lResult.VarName;
        end;
      end;
    end;
    if (lVarName <> '') and VarNameIsCorrect(lVarName) then
    begin
      lUnitElement := GetUnitElement(pContext);
      try
        if (not lUnitElement.VariableDeclarationExistsBeforeLineIndex(lStartPos.Line, lVarName)) then
        begin
          lVariable := lUnitElement.VariableDeclarationBeforeLineIndex(lStartPos.Line);
          if (not Assigned(lResult)) then
          begin
            lResult := TfrmDeclareVariable.DeclareVariable(lVarName);
            lVarName := lResult.VarName;
          end;
          try
            if (lVarName <> '') and VarNameIsCorrect(lVarName) and
               (not lUnitElement.VariableDeclarationExistsBeforeLineIndex(lStartPos.Line, lVarName)) and
               (lVariable.LineNumber > 0) and pContext.EditBuffer.EditPosition.GotoLine(lVariable.LineNumber) then
            begin
              pContext.EditBuffer.EditPosition.MoveEOL;
              pContext.EditBuffer.EditPosition.InsertText(sLineBreak);
              Inc(lLine);
              pContext.EditBuffer.EditPosition.MoveBOL;
              if (not lVariable.IsDeclaration) then
              begin
                pContext.EditBuffer.EditPosition.InsertText('var' + sLineBreak);
                Inc(lLine);
              end;
              pContext.EditBuffer.EditPosition.InsertText(Format('  %s: %s;', [lVarName, lResult.VarType]));
            end;
          finally
            lVariable.Free;
          end;
        end;
      finally
        lUnitElement.Free;
      end;
      lStartPos.Line := lStartPos.Line + lLine;
      GoBackToPos(pContext, lStartPos);
      if lCanContinue then
      begin
        if (Assigned(lResult) and (lResult.DeclareOnCursor)) then
        begin
          if (lResult.InitialValue <> '') then
          begin
            lInitialValue := lResult.InitialValue;
            if ContainsText('ShortString;OpenString;String;PWideChar;PAnsiChar;AnsiChar;Char;AnsiString;' +
              'WideString;PChar;WideChar;UnicodeString', lResult.VarType) then
            begin
              lInitialValue := QuotedStr(lResult.InitialValue);
            end;
            pContext.EditBuffer.EditPosition.InsertText(Format('%s := %s;', [lVarName, lInitialValue]));
          end
          else
          begin
            pContext.EditBuffer.EditPosition.InsertText(Format('%s', [lVarName]));
          end;
        end;
        pContext.EditBuffer.TopView.Center(lStartPos.Line, lStartPos.Col);
      end;
    end;
  finally
    FreeAndNil(lResult);
  end;
end;

function TDeclareVariableKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.DeclareVariable;
end;

function TDeclareVariableKeyExecutor.VarNameIsCorrect(pVarName: String): Boolean;
var
  lIndex: Integer;
  lInt: Int64;
  lFloat: Extended;
begin
  Result := not StartsText('''', pVarName) and (Trim(pVarName) <> '');
  if (Result) then
  begin
    for lIndex := Low(TConsts.cStringDelimiters) to High(TConsts.cStringDelimiters) do
    begin
      Result := not ContainsText(pVarName, TConsts.cStringDelimiters[lIndex]);
      if (not Result) then
      begin
        Break;
      end;
    end;
    if (Result) then
    begin
      Result := not TryStrToInt64(pVarName, lInt);
      if (Result) then
      begin
        Result := not TryStrToFloat(pVarName, lFloat);
        if (Result) then
        begin
          for lIndex := Low(cInvalidVarNames) to High(cInvalidVarNames) do
          begin
            Result := not SameText(pVarName, cInvalidVarNames[lIndex]);
            if (not Result) then
            begin
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TDeclareVariableKeyExecutor);

end.