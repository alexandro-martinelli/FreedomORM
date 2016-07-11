unit AM.Freedom.GroupCriteriasKeyExecutor;

interface

uses
  ToolsAPI,
  AM.Freedom.FreedomBindKeyBoard,
  System.Classes,
  AM.Freedom.frmNewListCriteria;

type
  TGroupCriteriasKeyExecutor = class(TCustomKeyExecutor)
  protected
    function GetShortCut: TShortCut; override;
  public
    class function Description: string; override;
    procedure Execute(const Context: IOTAKeyContext); override;
  end;

implementation

uses
  AM.Freedom.FreedomDBObjectUnitWizard,
  AM.Freedom.dclFreedomORMConfig,
  System.SysUtils;

{ TNewFreedomDBObjectBindKeyBoard }

class function TGroupCriteriasKeyExecutor.Description: string;
begin
  Result := 'New Group criteria';
end;

procedure TGroupCriteriasKeyExecutor.Execute(const Context: IOTAKeyContext);
var
  lNewCriterias: TStrings;
  lText: String;
  lIndex: Integer;
begin
  lNewCriterias := TfrmNewListCriteria.NewListCriteria;
  try
    if (Assigned(lNewCriterias)) and (lNewCriterias.Count >= 1) then
    begin
      for lIndex := 0 to lNewCriterias.Count - 1 do
      begin
        lText := lNewCriterias.Strings[lIndex];
        if (lIndex <> 0) then
        begin
          Context.EditBuffer.TopView.Position.InsertText(sLineBreak);
        end;
        Context.EditBuffer.TopView.Position.InsertText(lText);
      end;
    end;
  finally
    FreeAndNil(lNewCriterias);
  end;
end;

function TGroupCriteriasKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.NewGroupCriteria;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TGroupCriteriasKeyExecutor);

end.
