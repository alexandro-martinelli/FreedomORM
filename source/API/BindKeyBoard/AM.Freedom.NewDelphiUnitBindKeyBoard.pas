unit AM.Freedom.NewDelphiUnitBindKeyBoard;

interface

uses
  ToolsAPI,
  System.Classes,
  Vcl.Menus,
  AM.Freedom.CustomFreedomBindKeyBoard,
  AM.Freedom.DelphiNewUnitCreator;

type
  TNewDelphiUnitBindKeyBoard = class(TCustomFreedomBindKeyBoard)
  protected
    procedure Execute(const Context: IOTAKeyContext; KeyCode: TShortCut); override;
    function GetShortCuts: TShortCutArray; override;
    function GetDisplayName: string; override;
    function GetName: string; override;
  end;

implementation

{ TNewFreedomDBObjectBindKeyBoard }

procedure TNewDelphiUnitBindKeyBoard.Execute(const Context: IOTAKeyContext; KeyCode: TShortCut);
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

function TNewDelphiUnitBindKeyBoard.GetDisplayName: string;
begin
  Result := 'Delphi New Unit';
end;

function TNewDelphiUnitBindKeyBoard.GetName: string;
begin
  Result := 'DelphiNewUnit';
end;

function TNewDelphiUnitBindKeyBoard.GetShortCuts: TShortCutArray;
begin
  SetLength(Result, 1);
  Result[0] := ShortCut(Ord('U'), [ssAlt, ssCtrl]);
end;

end.

