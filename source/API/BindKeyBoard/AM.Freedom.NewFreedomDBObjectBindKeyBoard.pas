unit AM.Freedom.NewFreedomDBObjectBindKeyBoard;

interface

uses
  ToolsAPI,
  AM.Freedom.CustomFreedomBindKeyBoard,
  System.Classes;

type
  TNewFreedomDBObjectBindKeyBoard = class(TCustomFreedomBindKeyBoard)
  protected
    procedure Execute(const Context: IOTAKeyContext; KeyCode: TShortCut); override;
    function GetShortCuts: TShortCutArray; override;
    function GetDisplayName: string; override;
    function GetName: string; override;
  end;

implementation

uses
  Vcl.Menus,
  AM.Freedom.FreedomDBObjectUnitWizard;

{ TNewFreedomDBObjectBindKeyBoard }

procedure TNewFreedomDBObjectBindKeyBoard.Execute(const Context: IOTAKeyContext; KeyCode: TShortCut);
begin
  TFreedomDBObjectUnitWizard.ExecuteFreedomDBObjectUnit;
end;

function TNewFreedomDBObjectBindKeyBoard.GetDisplayName: string;
begin
  Result := 'Freedom DB Object';
end;

function TNewFreedomDBObjectBindKeyBoard.GetName: string;
begin
  Result := 'FreedomDBObject';
end;

function TNewFreedomDBObjectBindKeyBoard.GetShortCuts: TShortCutArray;
begin
  SetLength(Result, 1);
  Result[0] := ShortCut(Ord('B'), [ssCtrl, ssShift]);
end;

end.
