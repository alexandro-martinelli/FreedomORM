unit AM.Freedom.UseUnitKeyExecutor;

interface

uses
  System.SysUtils,
  AM.Freedom.FreedomBindKeyBoard,
  AM.Freedom.dclFreedomORMConfig,
  UnitReader.UnitElement,
  System.Classes,
  AM.Freedom.frmUnitNames,
  UnitReader.EnumerationTypes,
  AM.Freedom.CustomUnitKeyExecutor;

type
  TUseUnitKeyExecutor = class(TCustomUnitKeyExecutor)
  protected
    function GetShortCut: TShortCut; override;
    function GetUnitDeclaration(pAlreadDeclaredUnits: TStrings; pCurrentSection: TTokenType): TUnitDeclaration; override;
  public
    class function Description: string; override;
  end;


implementation

{ TUseUnitKeyExecutor }

class function TUseUnitKeyExecutor.Description: string;
begin
  Result := 'Use Unit';
end;

function TUseUnitKeyExecutor.GetShortCut: TShortCut;
begin
    Result := TdclFreedomORMConfig.GetInstance.UseUnit;
end;

function TUseUnitKeyExecutor.GetUnitDeclaration(pAlreadDeclaredUnits: TStrings; pCurrentSection: TTokenType): TUnitDeclaration;
begin
  Result := TfrmUnitNames.UseUnit(pAlreadDeclaredUnits, pCurrentSection);
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TUseUnitKeyExecutor);

end.