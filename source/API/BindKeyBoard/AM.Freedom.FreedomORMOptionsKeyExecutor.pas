unit AM.Freedom.FreedomORMOptionsKeyExecutor;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.FreedomBindKeyBoard,
  UnitReader.UnitElement,
  UnitReader.ClassElement,
  ToolsAPI;

type
  TFreedomORMOptionsKeyExecutor = class(TCustomKeyExecutor)
  protected
    function GetShortCut: TShortCut; override;
  public
    class function Description: string; override;
    procedure Execute(const pContext: IOTAKeyContext); override;
  end;

implementation

uses
  AM.Freedom.frmConfigFreedomORM,
  AM.Freedom.dclFreedomORMConfig;

{ TConfigFreedomORMKeyExecutor }

class function TFreedomORMOptionsKeyExecutor.Description: string;
begin
  Result := 'Freedom ORM options'
end;

procedure TFreedomORMOptionsKeyExecutor.Execute(const pContext: IOTAKeyContext);
begin
  TfrmConfigFreedomORM.Configure;
end;

function TFreedomORMOptionsKeyExecutor.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.FreedomORMOptions;
end;


initialization
  TFreedomBindKeyBoard.RegisterKeyExecutor(TFreedomORMOptionsKeyExecutor);

end.