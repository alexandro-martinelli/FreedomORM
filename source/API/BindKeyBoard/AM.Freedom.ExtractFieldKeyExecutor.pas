unit AM.Freedom.ExtractFieldKeyExecutor;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.FreedomBindKeyBoard,
  AM.Freedom.frmDeclareVariable,
  UnitReader.UnitElement,
  ToolsAPI;

type
  TExtractFieldKeyExecutor = class(TCustomKeyExecutor)
  private
    function ExtractFieldType(pVarValue: string): string;
    procedure DoExtractField;
  protected
    function GetShortCut: TShortCut; override;
  public
    class function Description: string; override;
    procedure Execute(const pContext: IOTAKeyContext); override;
  end;


implementation

uses
  AM.Freedom.dclFreedomORMConfig;

{ TDeclareFieldKeyExecutor }

class function TExtractFieldKeyExecutor.Description: string;
begin
  Result := 'Declare field';
end;

procedure TExtractFieldKeyExecutor.DoExtractField;
begin

end;

procedure TExtractFieldKeyExecutor.Execute(const pContext: IOTAKeyContext);
var
  lFieldType: string;
begin
  FContext := pContext;
  FEditPosition := FContext.EditBuffer.EditPosition;
  FLine := 0;
  FFieldValue := GetSelectedBlockText(FContext);
  if (FFieldValue <> '') then
  begin
    lFieldType := ExtractFieldType(FFieldValue);
    FVariable := TfrmDeclareVariable.ExtractVariable('', lFieldType);
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

function TExtractFieldKeyExecutor.ExtractFieldType(pVarValue: string): string;
begin

end;

function TExtractFieldKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.ExtractVariable;
end;

end.
