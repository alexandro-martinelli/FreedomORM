unit AM.Freedom.NewFreedomObjectBindKeyBoard;

interface

uses
  ToolsAPI,
  AM.Freedom.CustomFreedomBindKeyBoard,
  System.Classes;

type
  TNewFreedomObjectBindKeyBoard = class(TCustomFreedomBindKeyBoard)
  protected
    procedure Execute(const Context: IOTAKeyContext; KeyCode: TShortCut); override;
    function GetShortCuts: TShortCutArray; override;
    function GetName: string; override;
    function GetDisplayName: string; override;
  end;

implementation

uses
  AM.Freedom.FreedomObjectUnitWizard,
  Vcl.Menus;

{ TNewFreedomObjectBindKeyBoard }

procedure TNewFreedomObjectBindKeyBoard.Execute(const Context: IOTAKeyContext; KeyCode: TShortCut);
begin
  TFreedomObjectUnitWizard.ExecuteNewFreedomObject;
end;

function TNewFreedomObjectBindKeyBoard.GetDisplayName: string;
begin
  Result := 'Freedom Object';
end;

function TNewFreedomObjectBindKeyBoard.GetName: string;
begin
  Result := 'FreedomObject';
end;

function TNewFreedomObjectBindKeyBoard.GetShortCuts: TShortCutArray;
begin
  SetLength(Result, 1);
  Result[0] := ShortCut(Ord('O'), [ssCtrl, ssShift]);
end;

end.
