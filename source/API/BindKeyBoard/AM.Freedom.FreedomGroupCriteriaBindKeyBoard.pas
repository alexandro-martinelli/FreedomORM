unit AM.Freedom.FreedomGroupCriteriaBindKeyBoard;

interface

uses
  ToolsAPI,
  AM.Freedom.CustomFreedomBindKeyBoard,
  System.Classes,
  AM.Freedom.frmNewListCriteria;

type
  TFreedomGroupCriteriaBindKeyBoard = class(TCustomFreedomBindKeyBoard)
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

procedure TFreedomGroupCriteriaBindKeyBoard.Execute(const Context: IOTAKeyContext; KeyCode: TShortCut);
var
  lNewCriterias: TStrings;
begin
  lNewCriterias := TfrmNewListCriteria.NewListCriteria;
  if (Assigned(lNewCriterias)) and (lNewCriterias.COunt >= 1) then
  begin
    try
      Context.EditBuffer.TopView.Position.InsertText(lNewCriterias.Text);
    finally
      lNewCriterias.Free;
    end;
  end;
end;

function TFreedomGroupCriteriaBindKeyBoard.GetDisplayName: string;
begin
  Result := 'Freedom Criterias';
end;

function TFreedomGroupCriteriaBindKeyBoard.GetName: string;
begin
  Result := 'FreedomCriterias';
end;

function TFreedomGroupCriteriaBindKeyBoard.GetShortCuts: TShortCutArray;
begin
  SetLength(Result, 2);
  Result[0] := ShortCut(Ord('G'), [ssCtrl]);
  Result[1] := ShortCut(Ord('G'), [ssCtrl])
end;

end.
