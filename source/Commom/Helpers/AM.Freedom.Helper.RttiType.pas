unit AM.Freedom.Helper.RttiType;

interface

uses
  System.Classes,
  System.Rtti,
  AM.Freedom.EnumerationTypes;

type
  TRttiTypeHelper = class helper for TRttiType
  public
    function ToColumnType: TColumnType;
  end;

implementation

uses
  System.TypInfo,
  System.SysUtils,
  AM.Freedom.ILazy,
  AM.Freedom.Nullable,
  AM.Freedom.INullable, AM.Freedom.XML;

{ TRttiTypeHelper }

function TRttiTypeHelper.ToColumnType: TColumnType;
var
  lTypeData: PTypeData;
  lTypeName: string;
  lContext: TRttiContext;
  lRttiType: TRttiType;
  lProperty: TRttiProperty;
begin
  Result := ctyUnknow;
  case Self.TypeKind of
    tkInteger:
      case Self.AsOrdinal.OrdType of
        otSByte, otUByte:
          Result := ctyByte;
        otSWord, otUWord:
          Result := ctySmallint;
        otSLong, otULong:
          Result := ctyInteger;
      end;
    tkWChar, tkChar:
      Result := ctyChar;
    tkEnumeration:
      begin
        lTypeName := GetTypeName(Self.Handle);
        if SameText(lTypeName, 'Boolean') then
        begin
          Result := ctyBoolean;
        end
        else
        begin
          Result := ctyEnumerator;
        end;
      end;
    tkFloat:
      begin
        lTypeData := GetTypeData(Self.Handle);
        lTypeName := GetTypeName(Self.Handle);
        case lTypeData.FloatType of
          ftSingle:
            Result := ctySingle;
          ftDouble:
            begin
              Result := ctyDouble;
              if SameText(lTypeName, 'TDate') then
              begin
                Result := ctyDate;
              end
              else if SameText(lTypeName, 'TTime') then
              begin
                Result := ctyTime;
              end
              else if SameText(lTypeName, 'TDateTime') then
              begin
                Result := ctyDateTime;
              end;
            end;
          ftExtended:
            Result := ctyExtended;
          ftCurr:
            Result := ctyCurrency;
        end;
      end;
    tkUString, tkLString, tkWString, tkString:
      Result := ctyString;
    tkClass:
      begin
        if Self.AsInstance.MetaclassType.InheritsFrom(TStream) then
        begin
          Result := ctyBlob;
        end
        else if Self.AsInstance.MetaclassType.InheritsFrom(TStrings) then
        begin
          Result := ctyMemo;
        end
        else if Self.AsInstance.MetaclassType.InheritsFrom(TXML) then
        begin
          Result := ctyXML;
        end
        else if Supports(Self.AsInstance.MetaclassType, INullable) then
        begin
          lContext := TRttiContext.Create;
          try
            lRttiType := lContext.GetType(Self.AsInstance.MetaclassType.ClassInfo);
            lProperty := lRttiType.GetProperty('Value');
            Result := lProperty.PropertyType.ToColumnType;
          finally
            lContext.Free;
          end;
        end;
      end;
    tkInt64:
      Result := ctyInt64;
  end;
end;

end.
