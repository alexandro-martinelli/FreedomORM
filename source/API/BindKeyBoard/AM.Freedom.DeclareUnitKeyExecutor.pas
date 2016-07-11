unit AM.Freedom.DeclareUnitKeyExecutor;

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
  TDeclareUnitKeyExecutor = class(TCustomUnitKeyExecutor)
  protected
    function GetShortCut: TShortCut; override;
    function GetUnitDeclaration(pAlreadDeclaredUnits: TStrings; pCurrentSection: TTokenType): TUnitDeclaration; override;
  public
    class function Description: string; override;
  end;


implementation

uses
  System.StrUtils;

{ TDeclareUnitKeyExecutor }

class function TDeclareUnitKeyExecutor.Description: string;
begin
  Result := 'Declare unit';
end;

function TDeclareUnitKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.DeclareUnit;
end;

function TDeclareUnitKeyExecutor.GetUnitDeclaration(pAlreadDeclaredUnits: TStrings; pCurrentSection: TTokenType): TUnitDeclaration;
begin
  Result := TfrmUnitNames.DeclareUnit(pAlreadDeclaredUnits, pCurrentSection);
end;

initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TDeclareUnitKeyExecutor);

end.