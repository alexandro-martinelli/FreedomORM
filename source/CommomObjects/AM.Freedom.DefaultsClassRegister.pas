unit AM.Freedom.DefaultsClassRegister;

interface

uses
  System.Generics.Collections,
  AM.Freedom.ObjectMapper.CustomAutoMappingValueGetter,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.SQLMappers.ISQLFormatter,
  AM.Freedom.IPersistent,
  AM.Freedom.DBPersistent.IDBConnector,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.CustomNullable,
  AM.Freedom.CustomRounder;

type
  TDefaultsClassRegister = class sealed
  strict private
    class var FDefaultAutoMappingValueGetter: TAutoMappingValueGetterClass;
    class var FDefaultSQLMapper: ISQLMapper;
    class var FDefaultPersistent: IPersistent;
    class var FDefaultNullableCompareClass: TNullableCompareClass;
    class function GetDefaultAutoMappingValueGetter: TAutoMappingValueGetterClass; static;
    class function GetDefaultSQLFormatter: ISQLFormatter; static;
    class function GetDefaultNullableCompareClass: TNullableCompareClass; static;
  private
    class var FDefaultSQLFormatter: ISQLFormatter;
    class var FDefaultRounderClass: TRounderClass;
    class function GetDefaultRounderClass: TRounderClass; static;
  public
    class property DefaultAutoMappingValueGetter: TAutoMappingValueGetterClass read GetDefaultAutoMappingValueGetter
        write FDefaultAutoMappingValueGetter;
    class property DefaultSQLMapper: ISQLMapper read FDefaultSQLMapper write FDefaultSQLMapper;
    class property DefaultSQLFormatter: ISQLFormatter read GetDefaultSQLFormatter write FDefaultSQLFormatter;
    class property DefaultPersistent: IPersistent read FDefaultPersistent write FDefaultPersistent;
    class property DefaultNullableCompareClass: TNullableCompareClass read GetDefaultNullableCompareClass write FDefaultNullableCompareClass;
    class property DefaultRounderClass: TRounderClass read GetDefaultRounderClass write FDefaultRounderClass;
  end;

implementation

uses
  AM.Freedom.SQLMappers.SQLFormatter,
  System.SysUtils;

{ TDefaultsClassRegister }

class function TDefaultsClassRegister.GetDefaultAutoMappingValueGetter: TAutoMappingValueGetterClass;
begin
  Result := FDefaultAutoMappingValueGetter;
  if not Assigned(Result) then
  begin
    Result := TCustomAutoMappingValueGetter;
  end;
end;

class function TDefaultsClassRegister.GetDefaultNullableCompareClass: TNullableCompareClass;
begin
  Result := FDefaultNullableCompareClass;
  if (not Assigned(Result)) then
  begin
    Result := TCustomNullableCompare;
  end;
end;

class function TDefaultsClassRegister.GetDefaultRounderClass: TRounderClass;
begin
  Result := FDefaultRounderClass;
  if (not Assigned(FDefaultRounderClass)) then
  begin
    Result := TCustomRounder;
  end;
end;

class function TDefaultsClassRegister.GetDefaultSQLFormatter: ISQLFormatter;
begin
  if not Assigned(FDefaultSQLFormatter) then
  begin
    FDefaultSQLFormatter := TSQLFormatter.Create;
  end;
  Result := FDefaultSQLFormatter;
end;

initialization

finalization
  TDefaultsClassRegister.DefaultSQLMapper := nil;
  TDefaultsClassRegister.FDefaultSQLFormatter := nil;
  TDefaultsClassRegister.DefaultPersistent := nil;

end.
