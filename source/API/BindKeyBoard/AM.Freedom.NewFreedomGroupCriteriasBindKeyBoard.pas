unit AM.Freedom.NewFreedomGroupCriteriasBindKeyBoard;

interface

uses
  ToolsAPI,
  AM.Freedom.CustomFreedomBindKeyBoard,
  System.Classes,
  AM.Freedom.frmNewListCriteria;

type
  TNewFreedomGroupCriteriasBindKeyBoard = class(TCustomFreedomBindKeyBoard)
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

procedure TNewFreedomGroupCriteriasBindKeyBoard.Execute(const Context: IOTAKeyContext; KeyCode: TShortCut);
var
  lNewCriterias: TStrings;
  lText: String;
  lIndex: Integer;
begin
  lNewCriterias := TfrmNewListCriteria.NewListCriteria;
  if (Assigned(lNewCriterias)) and (lNewCriterias.Count >= 1) then
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

function TNewFreedomGroupCriteriasBindKeyBoard.GetDisplayName: string;
begin
  Result := 'Freedom Group Criterias';
end;

function TNewFreedomGroupCriteriasBindKeyBoard.GetName: string;
begin
  Result := 'FreedomGroupCriterias';
end;

function TNewFreedomGroupCriteriasBindKeyBoard.GetShortCuts: TShortCutArray;
begin
  SetLength(Result, 1);
  Result[0] := ShortCut(Ord('G'), [ssCtrl, ssAlt]);
end;

end.
