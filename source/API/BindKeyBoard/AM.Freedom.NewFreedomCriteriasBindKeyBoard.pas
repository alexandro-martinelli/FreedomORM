unit AM.Freedom.NewFreedomCriteriasBindKeyBoard;

interface

uses
  ToolsAPI,
  AM.Freedom.CustomFreedomBindKeyBoard,
  System.Classes,
  AM.Freedom.frmNewListCriteria;

type
  TNewFreedomCriteriasBindKeyBoard = class(TCustomFreedomBindKeyBoard)
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

procedure TNewFreedomCriteriasBindKeyBoard.Execute(const Context: IOTAKeyContext; KeyCode: TShortCut);
var
  lNewCriterias: TStrings;
  lIndex: Integer;
  lText: string;
begin
  lNewCriterias := TfrmNewListCriteria.NewListCriteria(True);
  if (Assigned(lNewCriterias)) and (lNewCriterias.COunt >= 1) then
  begin
    try
      for lIndex := 0 to lNewCriterias.Count - 1 do
      begin
        lText := lNewCriterias.Strings[lIndex];
        if (lIndex <> 0) then
        begin
          Context.EditBuffer.TopView.Position.InsertText(sLineBreak);
        end;
        Context.EditBuffer.TopView.Position.InsertText(lText);
      end;
    finally
      lNewCriterias.Free;
    end;
  end;

end;

function TNewFreedomCriteriasBindKeyBoard.GetDisplayName: string;
begin
  Result := 'Freedom Criterias';
end;

function TNewFreedomCriteriasBindKeyBoard.GetName: string;
begin
  Result := 'FreedomCriterias';
end;

function TNewFreedomCriteriasBindKeyBoard.GetShortCuts: TShortCutArray;
begin
  SetLength(Result, 1);
  Result[0] := ShortCut(Ord('C'), [ssCtrl, ssAlt]);
end;

end.
