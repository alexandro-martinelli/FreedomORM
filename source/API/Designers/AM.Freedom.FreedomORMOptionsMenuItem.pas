unit AM.Freedom.FreedomORMOptionsMenuItem;

interface

uses
  System.SysUtils,
  AM.Freedom.CustomMenuItem;

type
  TFreedomORMOptionsMenuItem = class(TCustomMenuItem)
  strict private
    class var FInstance: TFreedomORMOptionsMenuItem;
  strict protected
    procedure DoMenuClick(Sender: TObject); override;
    function GetCaption: string; override;
  public
    class function GetInstance: TFreedomORMOptionsMenuItem;
    class procedure DestroyInstance;
  end;

implementation

{ TFreedomORMOptionsMenuItem }

uses
  AM.Freedom.frmConfigFreedomORM;

class procedure TFreedomORMOptionsMenuItem.DestroyInstance;
begin
  FreeAndNil(FInstance)
end;

procedure TFreedomORMOptionsMenuItem.DoMenuClick(Sender: TObject);
begin
  TfrmConfigFreedomORM.Configure;
end;

function TFreedomORMOptionsMenuItem.GetCaption: string;
begin
  Result := 'Options';
end;

class function TFreedomORMOptionsMenuItem.GetInstance: TFreedomORMOptionsMenuItem;
begin
  if (not Assigned(FInstance)) then
  begin
    FInstance := TFreedomORMOptionsMenuItem.Create;
  end;
  Result := FInstance;
end;

end.