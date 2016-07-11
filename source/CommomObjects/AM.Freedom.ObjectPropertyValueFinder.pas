unit AM.Freedom.ObjectPropertyValueFinder;

interface

uses
  System.SysUtils,
  System.Classes,
  AM.Freedom.IFreedomObjectList,
  AM.Freedom.ObjectMapper;

type
  TObjectPropertyValueFinder = class
  private
    FCurrentObject: TObject;
    FPropertyName: String;
    FObjectMapper: TObjectMapper;
    function DoFindValue: Variant;
    function FindInObject(pPropertyName: String; pPropertys: TStrings): Variant;
    function GetObjectPropertyValue(pObject: TObject; pPropertys: TStrings): Variant;
    function FixPropertyName(pPropertys: TStrings): String;
  public
    property CurrentObject: TObject read FCurrentObject write FCurrentObject;
    property PropertyName: String read FPropertyName write FPropertyName;
    property ObjectMapper: TObjectMapper read FObjectMapper write FObjectMapper;
    function FindValue: Variant;
  end;


implementation

uses
  System.Variants,
  System.StrUtils,
  AM.Freedom.ObjectMapper.ObjectToMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.CustomColumnMapper;

{ TObjectPropertyValueFinder }

function TObjectPropertyValueFinder.DoFindValue: Variant;
var
  lAllPropertys: TStrings;
  lPropertyName: String;
begin
  Result := Null;
  lAllPropertys := TStringList.Create;
  ExtractStrings(['.'], [' '], PWideChar(FPropertyName), lAllPropertys);
  lPropertyName := lAllPropertys.Strings[0];
  try
    if not Supports(FCurrentObject, IFreedomObjectList) then
    begin
      Result := FindInObject(lPropertyName, lAllPropertys);
    end;
  finally
    lAllPropertys.Free;
  end;
end;

function TObjectPropertyValueFinder.FindInObject(pPropertyName: String; pPropertys: TStrings): Variant;
var
  lColumn: TCustomColumnMapper;
  lObject: TObject;
begin
  Result := Null;
  lColumn := FObjectMapper.Columns.FindColumn(pPropertyName);
  if Assigned(lColumn) then
  begin
    if lColumn.RttiOptions.RttiProperty.PropertyType.IsInstance then
    begin
      lObject := lColumn.RttiOptions.RttiProperty.GetValue(FCurrentObject).AsObject;
      if Assigned(lObject) then
      begin
        if not Supports(lObject, IFreedomObjectList) then
        begin
          pPropertys.Delete(0);
        end;
        Result := GetObjectPropertyValue(lObject, pPropertys);
      end;
    end
    else
    begin
      Result := lColumn.CurrentValue;
    end;
  end;
end;

function TObjectPropertyValueFinder.FindValue: Variant;
begin
  if Assigned(FCurrentObject) and (FPropertyName <> '') and Assigned(FObjectMapper) then
  begin
    Result := DoFindValue;
  end;
end;

function TObjectPropertyValueFinder.FixPropertyName(pPropertys: TStrings): String;
var
  lProperty: String;
begin
  Result := '';
  for lProperty in pPropertys do
  begin
    Result := Result + ifthen(Result <> '', '.') + lProperty;
  end;
end;

function TObjectPropertyValueFinder.GetObjectPropertyValue(pObject: TObject; pPropertys: TStrings): Variant;
var
  lObjectPropertyValueFinder: TObjectPropertyValueFinder;
  lParams: TObjectToMapperParams;
  lMapper: TObjectMapper;
begin
  lObjectPropertyValueFinder := TObjectPropertyValueFinder.Create;
  lParams := TObjectToMapperParams.Create;
  lParams.Options := [Properties];
  lParams.ObjectInstance := pObject;
  lMapper := TObjectToMapper.ObjectToMapper(lParams);
  try
    lObjectPropertyValueFinder.ObjectMapper := lMapper;
    lObjectPropertyValueFinder.CurrentObject := pObject;
    lObjectPropertyValueFinder.PropertyName := FixPropertyName(pPropertys);
    Result := lObjectPropertyValueFinder.FindValue;
  finally
    lObjectPropertyValueFinder.Free;
    TObjectToMapper.UnlockMapper(lMapper.GetHashCode);
  end;
end;

end.