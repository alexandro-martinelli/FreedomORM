unit AM.Freedom.TextGenerator.ValueArgumentTextGenerator;

interface

uses
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  TValueArgumentTextGenerator = class(TCustomTextGenerator)
  strict protected
    function DoGenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String; override;
  end;


implementation

uses
  System.SysUtils,
  AM.Freedom.EnumerationTypes,
  System.StrUtils,
  AM.Freedom.SQLMappers.SQLFormatSettings;

{ TValueArgumentTextGenerator }

function TValueArgumentTextGenerator.DoGenerateText(pObject: TObject; pParams: TGenerateTextParams): string;
var
  lArgument: TValueArgument;
  lSettings: TFormatSettings;
  lSQLFormat: TSQLFormatSettings;
  lDateFormat, lTimeFormat, lDateTimeFormat: String;
begin
  lSettings := TFormatSettings.Create;
  lSettings.DecimalSeparator := '.';
  lSettings.ThousandSeparator := ' ';
  lArgument := TValueArgument(pObject);
  if (lArgument.ValueType in [ctyDate, ctyTime, ctyDateTime]) then
  begin
    if GetSQLMapper <> nil then
    begin
      lSQLFormat := GetSQLMapper.GetSQLFormatSettings;
      lDateFormat := lSQLFormat.DateFormat;
      lTimeFormat := lSQLFormat.TimeFormat;
      lDateTimeFormat := lSQLFormat.DateTimeFormat;
    end
    else
    begin
      lDateFormat := lSettings.ShortDateFormat;
      lTimeFormat := lSettings.ShortTimeFormat;
      lDateTimeFormat := Format('%s %s', [lDateFormat, lTimeFormat]);
    end;
  end;
  case lArgument.ValueType of
   ctySmallint, ctyInteger, ctyInt64: Result := IntToStr(lArgument.AsInt64);
   ctyString, ctyChar: Result := lArgument.AsString;
   ctySingle, ctyDouble, ctyExtended: Result := FloatToStr(lArgument.AsExtended, lSettings);
   ctyCurrency: Result := CurrToStr(lArgument.AsCurrency, lSettings);
   ctyDate: Result := FormatDateTime(lDateFormat, lArgument.AsDate);
   ctyTime: Result := FormatDateTime(lTimeFormat, lArgument.AsTime);
   ctyDateTime: Result := FormatDateTime(lDateTimeFormat, lArgument.AsDateTime);
   ctyBoolean: Result := GetSQLMapper.BooleanToDefaultExpression(lArgument.AsBoolean);
  end;
  if Assigned(pParams) then
  begin
    if (pParams.ComparatorType in [cpLikeRigth, cpLikeMidle, cpLikeLeft]) and (lArgument.ValueType in [ctyChar, ctyString]) then
    begin
      Result := GetSQLMapper.FormatLikeExpression(Result);
    end;
    if pParams.ComparatorType in [cpLikeLeft, cpLikeMidle] then
    begin
      Result := '%' + Result;
    end;
    if pParams.ComparatorType in [cpLikeRigth, cpLikeMidle] then
    begin
      Result := Result + '%';
    end;
  end;

  if lArgument.ValueType in [ctyChar, ctyString, ctyDate, ctyTime, ctyDateTime] then
  begin
    Result := QuotedStr(Result);
  end;
end;

end.
