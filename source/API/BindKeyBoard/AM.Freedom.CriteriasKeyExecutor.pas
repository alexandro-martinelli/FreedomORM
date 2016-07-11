unit AM.Freedom.CriteriasKeyExecutor;

interface

uses
  ToolsAPI,
  System.Classes,
  AM.Freedom.frmNewListCriteria,
  AM.Freedom.FreedomBindKeyBoard;

type
  TCriteriasKeyExecutor = class(TCustomKeyExecutor)
  protected
    function GetShortCut: TShortCut; override;
  public
    class function Description: string; override;
    procedure Execute(const pContext: IOTAKeyContext); override;
  end;

implementation

uses
  AM.Freedom.dclFreedomORMConfig,
  System.SysUtils;

{ TNewFreedomDBObjectBindKeyBoard }

function TCriteriasKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.NewCriteria;
end;

class function TCriteriasKeyExecutor.Description: string;
begin
  Result := 'New Criteria';
end;

procedure TCriteriasKeyExecutor.Execute(const pContext: IOTAKeyContext);
var
  lNewCriterias: TStrings;
  lIndex: Integer;
  lText: string;
begin
  lNewCriterias := TfrmNewListCriteria.NewListCriteria(True);
  try
    if (Assigned(lNewCriterias)) and (lNewCriterias.Count >= 1) then
    begin
      for lIndex := 0 to lNewCriterias.Count - 1 do
      begin
        lText := lNewCriterias.Strings[lIndex];
        if (lIndex <> 0) then
        begin
          pContext.EditBuffer.EditPosition.InsertText(sLineBreak);
        end;
        pContext.EditBuffer.EditPosition.InsertText(lText);
      end;
    end;
  finally
    FreeAndNil(lNewCriterias);
  end;
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TCriteriasKeyExecutor);

end.
