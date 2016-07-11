unit AM.Freedom.ExtractConstantKeyExecutor;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.FreedomBindKeyBoard,
  AM.Freedom.frmDeclareVariable,
  UnitReader.UnitElement,
  ToolsAPI;

type
  TExtractConstantKeyExecutor = class(TCustomKeyExecutor)
  private
    FLine: Integer;
    FStartPos: TOTAEditPos;
    FContext: IOTAKeyContext;
    FUnitElement: TUnitElement;
    FEditPosition: IOTAEditPosition;
    function ValidateConstantValue(pConstValue: String): Boolean;
  protected
    function GetShortCut: TShortCut; override;
  public
    class function Description: string; override;
    procedure Execute(const pContext: IOTAKeyContext); override;
  end;


implementation

{ TExtractConstantKeyExecutor }

uses
  System.StrUtils,
  AM.Freedom.frmExtractConstant,
  AM.UnitReader.Helper.MemberVisibility,
  AM.Freedom.dclFreedomORMConfig;

class function TExtractConstantKeyExecutor.Description: string;
begin
  Result := 'Extract constant';
end;

procedure TExtractConstantKeyExecutor.Execute(const pContext: IOTAKeyContext);
var
  lConstValue: String;
  lConstantResult: TExtractConstantResult;
  lConstantDeclaration: TConstantDeclarationResult;
  lConstNameExists: Boolean;
begin
  FContext := pContext;
  FEditPosition := FContext.EditBuffer.EditPosition;
  FStartPos := FContext.EditBuffer.TopView.CursorPos;
  FLine := FStartPos.Line;
  lConstValue := GetSelectedBlockText(FContext);
  if (lConstValue <> '') then
  begin
    if (ValidateConstantValue(lConstValue)) then
    begin
      lConstantResult := TfrmExtractConstant.ExtractConstant(lConstValue);
      try
        if (lConstantResult.ConstantName <> '') then
        begin
          FUnitElement := GetUnitElement(FContext);
          try
            lConstNameExists := FUnitElement.ConstantDeclarationExistsBeforeLineIndex(FStartPos.Line, lConstantResult.ConstantName);
            if (not lConstantResult.DeclareOnClass) then
            begin
              lConstantDeclaration := FUnitElement.ConstantDeclarationBeforeLineIndex(FStartPos.Line);
            end
            else
            begin
              lConstantDeclaration := FUnitElement.ConstantClassDeclaration(FStartPos.Line, lConstantResult.VisibilityScope);
            end;
            try
              if lConstantDeclaration.LineNumber > 0 then
              begin
                FContext.EditBuffer.EditBlock.Delete;
                FEditPosition.InsertText(lConstantResult.ConstantName);
                if (not lConstNameExists) then
                begin
                  FEditPosition.GotoLine(lConstantDeclaration.LineNumber);
                  FEditPosition.MoveEOL;
                  FEditPosition.InsertText(sLineBreak);
                  Inc(FLine);
                  if (not lConstantDeclaration.IsDeclaration) then
                  begin
                    FEditPosition.MoveBOL;
                    if (lConstantResult.DeclareOnClass) then
                    begin
                      FEditPosition.InsertText('  ' + lConstantResult.VisibilityScope.ToString + sLineBreak);
                      Inc(FLine);
                    end;
                    FEditPosition.InsertText(ifthen(lConstantResult.DeclareOnClass, '  ') + 'const' + sLineBreak);
                    Inc(FLine);
                  end;
                  FEditPosition.InsertText(Format('%s%s = %s;', [ifthen(not lConstantDeclaration.IsDeclaration, '  '),
                    lConstantResult.ConstantName, lConstValue]));
                end;
                FStartPos.Line := FLine;
                GoBackToPos(FContext, FStartPos);
              end;
            finally
              FreeAndNil(lConstantDeclaration);
            end;
          finally
            FreeAndNil(FUnitElement);
          end;
        end;
      finally
        FreeAndNil(lConstantResult);
      end;
    end;
  end;
end;

function TExtractConstantKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.ExtractConstant;
end;

function TExtractConstantKeyExecutor.ValidateConstantValue(pConstValue: String): Boolean;
var
  lInt64: Int64;
  lFloat: Extended;
begin
  Result := StartsText('''', pConstValue) or (StartsText('[', pConstValue) and EndsText('''', pConstValue)) or
      SameText('True', pConstValue) or SameText('False', pConstValue);
  if (not Result) then
  begin
    Result := TryStrToInt64(pConstValue, lInt64);
    if (not Result) then
    begin
      Result := TryStrToFloat(pConstValue, lFloat);
    end;
  end;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TExtractConstantKeyExecutor);

end.