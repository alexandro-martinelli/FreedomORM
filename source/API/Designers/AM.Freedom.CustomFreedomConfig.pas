unit AM.Freedom.CustomFreedomConfig;

interface

uses
  System.SysUtils;

type
  TCustomFreedomConfig = class
  strict private const
    cAllUsersProfileVariable = 'ALLUSERSPROFILE';
  protected
    class function GetIniFileDirectory: String;
  end;


implementation

uses
  System.IOUtils;

class function TCustomFreedomConfig.GetIniFileDirectory: String;
begin
  Result := GetEnvironmentVariable(cAllUsersProfileVariable) + '\FreedomORM';
  if (not TDirectory.Exists(Result)) then
  begin
    TDirectory.CreateDirectory(Result);
  end;
  Result := Result + '\';
end;

end.