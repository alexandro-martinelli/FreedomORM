unit AM.Freedom.NewUnitKeyExecutor;

interface

uses
  ToolsAPI,
  System.Classes,
  AM.Freedom.DelphiNewUnitCreator,
  AM.Freedom.FreedomBindKeyBoard;

type
  TNewUnitKeyExecutor = class(TCustomKeyExecutor)
  protected
    function GetShortCut: TShortCut; override;
  public
    class function Description: string; override;
    procedure Execute(const pContext: IOTAKeyContext); override;
  end;

implementation

uses
  AM.Freedom.dclFreedomORMConfig;

{ TNewFreedomDBObjectBindKeyBoard }

function TNewUnitKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.NewUnit;
end;

class function TNewUnitKeyExecutor.Description: string;
begin
  Result := 'New unit';
end;

procedure TNewUnitKeyExecutor.Execute(const pContext: IOTAKeyContext);
var
  lCurrentProject: IOTAProject;
  lIDE: IOTAModuleServices;
  lCreator: IOTAModuleCreator;
  lUnitIdent, lClassName, lFileName: string;
begin
  lIDE := (BorlandIDEServices as IOTAModuleServices);
  try
    lCurrentProject := lIDE.GetActiveProject;
    try
      if Assigned(lCurrentProject) then
      begin
        lIDE.GetNewModuleAndClassName('', lUnitIdent, lClassName, lFileName);
        lCreator := TDelphiNewUnitCreator.Create(lUnitIdent);
        lIde.CreateModule(lCreator);
        lCurrentProject.AddFile(lFileName, True);
      end;
    finally
      lCreator := nil;
      lCurrentProject := nil;
    end;
  finally
    lIDE := nil;
  end;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TNewUnitKeyExecutor);

end.

