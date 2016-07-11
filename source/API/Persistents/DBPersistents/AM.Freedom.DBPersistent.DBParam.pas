unit AM.Freedom.DBPersistent.DBParam;

interface

uses
  Data.DB,
  System.Classes,
  System.Generics.Collections;

type
  TDBParam = class sealed
  private
    FParamType: TFieldType;
    FParamName: string;
    FParamValue: Variant;
    FStreamParamValue: TStream;
  public
    constructor Create(pParamName: string; pParamType: TFieldType; pParamValue: Variant); overload;
    constructor Create(pParamName: string; pParamType: TFieldType; pParamValue: TStream); overload;
    function CreateNew: TDBParam;
    property ParamName: string read FParamName;
    property ParamType: TFieldType read FParamType;
    property ParamValue: Variant read FParamValue;
    property StreamParamValue: TStream read FStreamParamValue;
  end;

  TDBParams = class(TObjectList<TDBParam>)
  public
    function ParamByName(pParamName: String): TDBParam;
  end;

implementation

uses
  System.Variants, System.SysUtils;
{ TDBParam }

function TDBParam.CreateNew: TDBParam;
begin
  if FStreamParamValue <> nil then
  begin
    Result := TDBParam.Create(FParamName, FParamType, FStreamParamValue);
  end
  else
  begin
    Result := TDBParam.Create(FParamName, FParamType, FParamValue);
  end;
end;

constructor TDBParam.Create(pParamName: string; pParamType: TFieldType; pParamValue: Variant);
begin
  FParamName := pParamName;
  FParamType := pParamType;
  FParamValue := pParamValue;
  FStreamParamValue := nil;
end;

constructor TDBParam.Create(pParamName: string; pParamType: TFieldType; pParamValue: TStream);
begin
  FParamName := pParamName;
  FParamType := pParamType;
  FParamValue := Null;
  FStreamParamValue := pParamValue;
end;

{ TDBParams }

function TDBParams.ParamByName(pParamName: String): TDBParam;
var
  lParam: TDBParam;
begin
  Result := nil;
  for lParam in Self do
  begin
    if SameText(lParam.ParamName, pParamName) then
    begin
      Result := lParam;
      Break;
    end;
  end;
end;

end.
