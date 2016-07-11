unit AM.Freedom.CustomFindMethodKeyExecutor;

interface

uses
  System.SysUtils,
  AM.Freedom.FreedomBindKeyBoard,
  AM.Freedom.dclFreedomORMConfig,
  ToolsAPI,
  UnitReader.UnitElement,
  System.Classes;

type
  TCustomFindMethodKeyExecutor = class(TCustomKeyExecutor)
  private
    FUnitElement: TUnitElement;
    FContext: IOTAKeyContext;
  protected
    function FindLineNumber: Integer; virtual;
    property UnitElement: TUnitElement read FUnitElement;
    property Context: IOTAKeyContext read FContext;
  public
    procedure Execute(const pContext: IOTAKeyContext); override; final;
  end;

implementation

uses
  AM.Freedom.Exceptions;

{ TCustomFindMethodKeyExecutor }

procedure TCustomFindMethodKeyExecutor.Execute(const pContext: IOTAKeyContext);
var
  lLineNumber: Integer;
  lCursorPos: TOTAEditPos;
begin
  FContext := pContext;
  FUnitElement := GetUnitElement(pContext);
  try
    lLineNumber := FindLineNumber;
    if (lLineNumber > 0) then
    begin
      FContext.EditBuffer.EditPosition.GotoLine(lLineNumber + 1);
      lCursorPos := pContext.EditBuffer.TopView.CursorPos;
      FContext.EditBuffer.TopView.Center(lCursorPos.Line, lCursorPos.Col);
    end;
  finally
    FUnitElement.Free;
    FContext := nil;
  end;
end;

function TCustomFindMethodKeyExecutor.FindLineNumber: Integer;
begin
  raise EInvalidMethodCallOnClass.Create('FindLineNumber', ClassName);
end;

end.