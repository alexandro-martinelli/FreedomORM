unit AM.Freedom.RegFreedomBindKeyBoard;

interface

procedure Register;

implementation

uses
  ToolsAPI,
  System.SysUtils,
  AM.Freedom.FreedomBindKeyBoard;

procedure Register;
begin
  TFreedomBindKeyBoard.RegisterKeyBindings;
end;


end.
