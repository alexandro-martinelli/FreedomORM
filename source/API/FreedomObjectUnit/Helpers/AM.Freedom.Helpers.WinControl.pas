unit AM.Freedom.Helpers.WinControl;

interface

uses
  Vcl.Controls;

type
  TWinControlHelper = class helper for TWincontrol
  public
    procedure EnabledInternalControls(pEnabled: Boolean);
  end;

implementation

{ TWinControlHelper }

procedure TWinControlHelper.EnabledInternalControls(pEnabled: Boolean);
var
  lIndex: Integer;
begin
  for lIndex := 0 to Self.ControlCount - 1 do
  begin
    if (Self.Controls[lIndex] is TWinControl) then
    begin
      TWinControl(Self.Controls[lIndex]).EnabledInternalControls(pEnabled);
    end
    else
    begin
      (Self.Controls[lIndex]).Enabled := pEnabled;
    end;
  end;
  Self.Enabled := pEnabled;
end;

end.
