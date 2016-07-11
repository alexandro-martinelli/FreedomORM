unit AM.Freedom.JSONValueFactory;

interface

uses
  AM.Freedom.JSONObjects,
  System.SysUtils;

type
  TJSONValueFactory = class sealed
  public
    class function CreateFromVariant(pVariant: Variant): TJSONValue;
  end;

implementation

uses
  System.Variants,
  System.Types,
  AM.Freedom.JSONConverter;

{ TJSONValueFactory }

class function TJSONValueFactory.CreateFromVariant(pVariant: Variant): TJSONValue;
var
  lObject: TObject;
  lClassConverter: TClassJSONConverter;
begin
  Result := nil;
  case VarType(pVariant) of
    varLongWord, varInt64, varUInt64, varWord, varByte, varShortInt, varSmallInt,
        varInteger: Result := TJSONInteger.Create(pVariant);
    varSingle, varDouble, varCurrency: begin
      Result := TJSONFloat.Create(pVariant);
    end;
    varDate: Result := TJSONDate.Create(pVariant);
    varUString, varString, varOleStr: begin
      Result := TJSONString.Create(VarToStr(pVariant));
    end;
    varObject: begin
      lObject := Pointer(Integer(pVariant));
      if (Assigned(lObject)) then
      begin
        lClassConverter := TClassJSONConverter.Create;
        try
          Result := lClassConverter.Convert(lObject);
        finally
          lClassConverter.Free;
        end;
      end;
    end;
    varBoolean: Result := TJSONBoolean.Create(pVariant);
    else
      Result := TJSONNull.Create;
   end;
end;

end.