unit AM.Freedom.FreedomORMMenuItem;

interface

uses
  AM.Freedom.CustomMenuItem;

type
  TFreedomORMMenuItem = class(TCustomMenuItem)
  strict private
    class var FFreedomORMMenuItem: TFreedomORMMenuItem;
  strict protected
    function GetCaption: String; override;
    procedure DoMenuClick(Sender: TObject); override;
  public
    class function GetInstance: TFreedomORMMenuItem;
    class procedure DestroyInstance;
  end;

implementation

{ TFreedomORMMenuItem }

class procedure TFreedomORMMenuItem.DestroyInstance;
begin
  if Assigned(FFreedomORMMenuItem) then
  begin
    FFreedomORMMenuItem.Free;
  end;
end;

procedure TFreedomORMMenuItem.DoMenuClick(Sender: TObject);
begin

end;

function TFreedomORMMenuItem.GetCaption: String;
begin
  Result := 'FreedomORM';
end;

class function TFreedomORMMenuItem.GetInstance: TFreedomORMMenuItem;
begin
  if not Assigned(FFreedomORMMenuItem) then
  begin
    FFreedomORMMenuItem := TFreedomORMMenuItem.Create;
  end;
  Result := FFreedomORMMenuItem;
end;

initialization

finalization
  TFreedomORMMenuItem.DestroyInstance;

end.
