unit AM.Freedom.FreedomObjectKeyExecutor;

interface

uses
  ToolsAPI,
  AM.Freedom.FreedomBindKeyBoard,
  System.Classes,
  System.StrUtils;

type
  TFreedomObjectKeyExecutor = class(TCustomKeyExecutor)
  protected
    function GetShortCut: TShortCut; override;
  public
    class function Description: string; override;
    procedure Execute(const Context: IOTAKeyContext); override;
  end;

implementation

uses
  AM.Freedom.FreedomObjectUnitWizard,
  AM.Freedom.dclFreedomORMConfig;

{ TNewFreedomObjectBindKeyBoard }

class function TFreedomObjectKeyExecutor.Description: string;
begin
  Result := 'New Freedom Object';
end;

procedure TFreedomObjectKeyExecutor.Execute(const Context: IOTAKeyContext);
begin
  TFreedomObjectUnitWizard.ExecuteNewFreedomObject;
end;

function TFreedomObjectKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.NewFreedomObject;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TFreedomObjectKeyExecutor);

end.
