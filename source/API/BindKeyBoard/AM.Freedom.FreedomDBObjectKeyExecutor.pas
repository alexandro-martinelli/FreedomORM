unit AM.Freedom.FreedomDBObjectKeyExecutor;

interface

uses
  ToolsAPI,
  AM.Freedom.FreedomBindKeyBoard,
  System.Classes;

type
  TFreedomDBObjectKeyExecutor = class(TCustomKeyExecutor)
  protected
    function GetShortCut: TShortCut; override;
  public
    procedure Execute(const Context: IOTAKeyContext); override;
    class function Description: string; override;
  end;

implementation

uses
  AM.Freedom.FreedomDBObjectUnitWizard,
  AM.Freedom.dclFreedomORMConfig;

{ TNewFreedomDBObjectBindKeyBoard }

class function TFreedomDBObjectKeyExecutor.Description: string;
begin
  Result := 'New Freedom DB Object';
end;

procedure TFreedomDBObjectKeyExecutor.Execute(const Context: IOTAKeyContext);
begin
  TFreedomDBObjectUnitWizard.ExecuteFreedomDBObjectUnit;
end;

function TFreedomDBObjectKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.NewFreedomDBObject;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TFreedomDBObjectKeyExecutor);

end.
