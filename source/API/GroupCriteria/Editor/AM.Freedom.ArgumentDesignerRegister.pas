unit AM.Freedom.ArgumentDesignerRegister;

interface

uses
  System.Generics.Collections,
  System.SysUtils, System.StrUtils;

type
  TDesignerParamType = (dptString, dptVariant, dptArgument);

  TParameterDesigner = class sealed
  private
    FParamName: String;
    FParamType: TDesignerParamType;
    FRequired: Boolean;
  public
    constructor Create(pParamName: String; pParamType: TDesignerParamType; pRequired: Boolean);
    property ParamName: String read FParamName write FParamName;
    property ParamType: TDesignerParamType read FParamType write FParamType;
    property Required: Boolean read FRequired write FRequired;
    function Description(pParameterValue: String): String;
  end;

  TParameterDesignerList = class sealed(TObjectList<TParameterDesigner>)
  public
    function FindParameterByName(pParameterName: String): TParameterDesigner;
  end;

  TArgumentDesigner = class sealed
  private
    FArgumentClassName: String;
    FArgumentCreateName: String;
    FParameterDesignerList: TParameterDesignerList;
  public
    constructor Create;
    destructor Destroy; override;
    property ArgumentClassName: String read FArgumentClassName write FArgumentClassName;
    property ArgumentCreateName: String read FArgumentCreateName write FArgumentCreateName;
    property ParameterDesignerList: TParameterDesignerList read FParameterDesignerList;
  end;

  TArgumentDesignerList = class sealed(TObjectList<TArgumentDesigner>)
  public
    function FindArgumentClassName(pArgumentClassName: String): TArgumentDesigner;

  end;

  TArgumentDesignerRegister = class sealed
  protected
    class var FArgumentDesignerList: TArgumentDesignerList;
    class procedure CreateArgumentDesignerList;
    class procedure DestroyArgumentDesignerList;
  public
    class procedure RegisterArgumentDesigner(pArgumentDesigner: TArgumentDesigner);
    class function ArgumentDesignerList: TArgumentDesignerList;
  end;


implementation

{ TArgumentDesignerRegister }

constructor TArgumentDesigner.Create;
begin
  FParameterDesignerList := TParameterDesignerList.Create;
end;

destructor TArgumentDesigner.Destroy;
begin
  FParameterDesignerList.Free;
  inherited;
end;

{ TArgumentDesignerRegister }

class function TArgumentDesignerRegister.ArgumentDesignerList: TArgumentDesignerList;
begin
  CreateArgumentDesignerList;
  Result := FArgumentDesignerList;
end;

class procedure TArgumentDesignerRegister.CreateArgumentDesignerList;
begin
  if not (Assigned(FArgumentDesignerList)) then
  begin
    FArgumentDesignerList := TArgumentDesignerList.Create;
  end;
end;

class procedure TArgumentDesignerRegister.DestroyArgumentDesignerList;
begin
  FArgumentDesignerList.Free;
end;

class procedure TArgumentDesignerRegister.RegisterArgumentDesigner(pArgumentDesigner: TArgumentDesigner);
begin
  CreateArgumentDesignerList;
  if FArgumentDesignerList.FindArgumentClassName(pArgumentDesigner.ArgumentClassName) = nil then
  begin
    FArgumentDesignerList.Add(pArgumentDesigner);
  end
  else
  begin
    pArgumentDesigner.Free;
  end;
end;


{ TArgumentDesignerList }

function TArgumentDesignerList.FindArgumentClassName(pArgumentClassName: String): TArgumentDesigner;
var
  lArgument: TArgumentDesigner;
begin
  Result := nil;
  for lArgument in Self do
  begin
    if SameText(lArgument.ArgumentClassName, pArgumentClassName) then
    begin
      Result := lArgument;
      Break;
    end;
  end;
end;

{ TParameterDesigner }

constructor TParameterDesigner.Create(pParamName: String; pParamType: TDesignerParamType; pRequired: Boolean);
begin
  FParamName := pParamName;
  FParamType := pParamType;
  FRequired := pRequired;
end;

function TParameterDesigner.Description(pParameterValue: String): String;
var
  lDate: TDateTime;
  lInt64: Int64;
  lFloat: Extended;
  lCurr: Currency;
  lBool: Boolean;
begin
  Result := '';
  if (pParameterValue <> '') then
  begin
    case FParamType of
      dptString: Result := QuotedStr(pParameterValue);
      dptVariant:
        begin
          if ContainsText('Now;Date;Time', pParameterValue) or TryStrToInt64(pParameterValue, lInt64) or TryStrToFloat(pParameterValue, lFloat) or
             TryStrToCurr(pParameterValue, lCurr) or TryStrToBool(pParameterValue, lBool) then
          begin
            Result := pParameterValue;
          end
          else if (TryStrToDate(pParameterValue, lDate)) then
          begin
            Result := 'StrToDate(' + QuotedStr(pParameterValue) + ')';
          end
          else if (TryStrToTime(pParameterValue, lDate)) then
          begin
            Result := 'StrToTime(' + QuotedStr(pParameterValue) + ')';
          end
          else if (TryStrToDateTime(pParameterValue, lDate)) then
          begin
            Result := 'StrToDateTime(' + QuotedStr(pParameterValue) + ')';
          end
          else
          begin
            Result := pParameterValue;
          end;
        end;
      dptArgument: Result := pParameterValue;
    end;
  end;
end;

{ TParameterDesignerList }

function TParameterDesignerList.FindParameterByName(pParameterName: String): TParameterDesigner;
var
  lParameter: TParameterDesigner;
begin
  Result := nil;
  for lParameter in Self do
  begin
    if (SameText(lParameter.ParamName, pParameterName)) then
    begin
      Result := lParameter;
      Break;
    end;
  end;
end;

initialization
  TArgumentDesignerRegister.CreateArgumentDesignerList;

finalization
  TArgumentDesignerRegister.DestroyArgumentDesignerList;

end.
