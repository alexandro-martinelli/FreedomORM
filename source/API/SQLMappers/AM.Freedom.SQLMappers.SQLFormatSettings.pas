unit AM.Freedom.SQLMappers.SQLFormatSettings;

interface

type
  TOnGetDateFormat = function: String of object;

  TSQLFormatSettings = class sealed
  private
    FTimeFormat: string;
    FDateTimeFormat: string;
    FDateFormat: string;
    FOnGetDateFormat: TOnGetDateFormat;
    function GetDateFormat: string;
    function GetDateTimeFormat: string;
  public
    property DateFormat: string read GetDateFormat write FDateFormat;
    property TimeFormat: string read FTimeFormat write FTimeFormat;
    property DateTimeFormat: string read GetDateTimeFormat write FDateTimeFormat;
    property OnGetDateFormat: TOnGetDateFormat read FOnGetDateFormat write FOnGetDateFormat;
  end;

implementation

uses
  System.SysUtils;

{ TSQLFormatSettings }

function TSQLFormatSettings.GetDateFormat: string;
begin
  if (FDateFormat = '') then
  begin
    if Assigned(FOnGetDateFormat) then
    begin
      FDateFormat := FOnGetDateFormat;
    end;
  end;
  Result := FDateFormat;
end;


function TSQLFormatSettings.GetDateTimeFormat: string;
begin
  if (FDateTimeFormat = '') then
  begin
    FDateTimeFormat := Format('%s %s', [DateFormat, TimeFormat]);
  end;
  Result := FDateTimeFormat;
end;

end.
