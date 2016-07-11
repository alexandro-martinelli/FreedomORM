unit AM.Freedom.FindMethodKeyExecutor;

interface

uses
  System.SysUtils,
  AM.Freedom.FreedomBindKeyBoard,
  AM.Freedom.dclFreedomORMConfig,
  System.Classes,
  AM.Freedom.CustomFindMethodKeyExecutor;

type
  TFindMethodKeyExecutor = class(TCustomFindMethodKeyExecutor)
  protected
    function GetShortCut: TShortCut; override;
    function FindLineNumber: Integer; override;
  public
    class function Description: string; override;
  end;


implementation

uses
  AM.Freedom.frmFindMethod;

{ TFindMethodKeyExecutor }

class function TFindMethodKeyExecutor.Description: string;
begin
  Result := 'Find method';
end;

function TFindMethodKeyExecutor.FindLineNumber: Integer;
begin
  Result := TfrmFindMethod.FindMethod(UnitElement);
end;

function TFindMethodKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.FindMethod;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TFindMethodKeyExecutor);

end.