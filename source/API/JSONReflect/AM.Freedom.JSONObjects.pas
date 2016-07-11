unit AM.Freedom.JSONObjects;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Types,
  System.Generics.Collections;

type
  TJSONValue = class abstract
  public
    function Value: String; virtual;
  end;

  TJSONNull = class(TJSONValue)
  public
    function Value: String; override;
  end;

  TJSONString = class(TJSONValue)
  private
    FString: String;
  public
    constructor Create(pString: String);
    function Value: String; override;
  end;

  TJSONDate = class(TJSONString)
  public
    constructor Create(pDate: TDate); reintroduce;
  end;

  TJSONTime = class(TJSONString)
  public
    constructor Create(pTime: TTime); reintroduce;
  end;

  TJSONDateTime = class(TJSONString)
  public
    constructor Create(pDateTime: TDateTime); reintroduce;
  end;

  TJSONFloat = class(TJSONValue)
  private
    FString: String;
  public
    constructor Create(pSingle: Single); overload;
    constructor Create(pDouble: Double); overload;
    constructor Create(pReal: Real); overload;
    constructor Create(pExtended: Extended); overload;
    constructor Create(pCurrency: Currency); overload;
    function Value: string; override;
  end;

  TJSONInteger = class(TJSONValue)
  private
    FInteger: Int64;
  public
    constructor Create(pInteger: Int64);
    function Value: string; override;
  end;

  TJSONBoolean = class(TJSONValue)
  private
    FBoolean: Boolean;
  public
    constructor Create(pBoolean: Boolean);
    function Value: string; override;
  end;

  TJSONEnumerator = class(TJSONString)
  public
    constructor Create(pEnumTypeInfo: PTypeInfo; pOrdinalValue: Int64); reintroduce;
  end;

  TJSONPair = class(TJSONValue)
    FName: String;
    FValue: TJSONValue;
  public
    constructor Create(pName: String; pValue: TJSONValue);
    destructor Destroy; override;
    function Value: string; override;
  end;

  TCustomJSONArray = class(TJSONValue)
  private
    FElementList: TObjectList<TJSONValue>;
  protected
    function GetInitializer: String; virtual;
    function GetFinalizer: String; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddElement(pJSONValue: TJSONValue); overload;
    function Value: string; override;
    property Elements: TObjectList<TJSONValue> read FElementList;
  end;

  TJSONArray = class(TCustomJSONArray)
  public
    procedure AddElement(pString: String); overload;
    procedure AddElement(pExtended: Extended); overload;
    procedure AddElement(pCurrency: Currency); overload;
    procedure AddElement(pDate: TDate); overload;
    procedure AddElement(pTime: TTime); overload;
    procedure AddElement(pDateTime: TDateTime); overload;
    procedure AddElement(pBoolean: Boolean); overload;
    procedure AddElement(pByteArray: TArray<Byte>); overload;
  end;

  TJSONObject = class(TCustomJSONArray)
  protected
    function GetInitializer: String; override;
    function GetFinalizer: String; override;
  end;

implementation

uses
  System.StrUtils,
  AM.Freedom.Exceptions,
  AM.Freedom.JSONUtils,
  AM.Freedom.JSONConsts;

{ TJSONString }

constructor TJSONString.Create(pString: String);
begin
  FString := pString;
end;

function TJSONString.Value: String;
begin
  Result := TJSONUtils.QuoteString(FString);
end;

{ TJSONValue }

function TJSONValue.Value: String;
begin
  raise EInvalidMethodCallOnClass.Create('Value', ClassName);
end;

{ TJSONFloat }

constructor TJSONFloat.Create(pCurrency: Currency);
begin
  FString := CurrToStr(pCurrency);
end;

constructor TJSONFloat.Create(pExtended: Extended);
begin
  FString := FloatToStr(pExtended);
end;

constructor TJSONFloat.Create(pReal: Real);
begin
  FString := FloatToStr(pReal);
end;

constructor TJSONFloat.Create(pDouble: Double);
begin
  FString := FloatToStr(pDouble);
end;

constructor TJSONFloat.Create(pSingle: Single);
begin
  FString := FloatToStr(pSingle);
end;

function TJSONFloat.Value: string;
begin
  Result := TJSONUtils.RemoveFloatingCharacteres(FString);
end;

{ TJSONPair }

constructor TJSONPair.Create(pName: String; pValue: TJSONValue);
begin
  FName := pName;
  FValue := pValue;
end;

function TJSONPair.Value: string;
begin
  Result := TJSONUtils.QuoteString(FName) + TJSONConsts.cNameValueSeparator + FValue.Value;
end;

destructor TJSONPair.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

{ TCustomJSONArray }

constructor TCustomJSONArray.Create;
begin
  FElementList := TObjectList<TJSONValue>.Create;
end;

function TCustomJSONArray.Value: string;
var
  lElement: TJSONValue;
begin
  Result := '';
  for lElement in FElementList do
  begin
    Result := Result + Ifthen(Result <> '', TJSONConsts.cArrayValueSeparator) + lElement.Value;
  end;
  if (Result <> '') then
  begin
    Result := GetInitializer + Result + GetFinalizer;
  end;
end;

destructor TCustomJSONArray.Destroy;
begin
  FElementList.Free;
  inherited;
end;

function TCustomJSONArray.GetInitializer: String;
begin
  Result := TJSONConsts.cArrayInitialization;
end;

function TCustomJSONArray.GetFinalizer: String;
begin
  Result := TJSONConsts.cArrayFinalization;
end;

procedure TCustomJSONArray.AddElement(pJSONValue: TJSONValue);
begin
  Elements.Add(pJSONValue);
end;

{ TJSONArray }

procedure TJSONArray.AddElement(pCurrency: Currency);
begin
  Elements.Add(TJSONFloat.Create(pCurrency));
end;

procedure TJSONArray.AddElement(pExtended: Extended);
begin
  Elements.Add(TJSONFloat.Create(pExtended));
end;

procedure TJSONArray.AddElement(pString: String);
begin
  Elements.Add(TJSONString.Create(pString));
end;

procedure TJSONArray.AddElement(pBoolean: Boolean);
begin
  Elements.Add(TJSONBoolean.Create(pBoolean));
end;

procedure TJSONArray.AddElement(pDateTime: TDateTime);
begin
  Elements.Add(TJSONDateTime.Create(pDateTime));
end;

procedure TJSONArray.AddElement(pTime: TTime);
begin
  Elements.Add(TJSONTime.Create(pTime));
end;

procedure TJSONArray.AddElement(pDate: TDate);
begin
  Elements.Add(TJSONDate.Create(pDate));
end;

procedure TJSONArray.AddElement(pByteArray: TArray<Byte>);
var
  lByte: Byte;
begin
  for lByte in pByteArray do
  begin
    Elements.Add(TJSONInteger.Create(lByte))
  end;
end;

{ TJSONDate }

constructor TJSONDate.Create(pDate: TDate);
begin
  inherited Create(DateToStr(pDate));
end;

{ TJSONTime }

constructor TJSONTime.Create(pTime: TTime);
begin
  inherited Create(TimeToStr(pTime));
end;

{ TJSONDateTime }

constructor TJSONDateTime.Create(pDateTime: TDateTime);
begin
  inherited Create(DateTimeToStr(pDateTime));
end;

{ TJSONBoolean }

constructor TJSONBoolean.Create(pBoolean: Boolean);
begin
  FBoolean := pBoolean;
end;

function TJSONBoolean.Value: string;
begin
  Result := BoolToStr(FBoolean, True).ToLower;
end;

{ TJSONEnumerator }

constructor TJSONEnumerator.Create(pEnumTypeInfo: PTypeInfo; pOrdinalValue: Int64);
begin
  inherited Create(GetEnumName(pEnumTypeInfo, pOrdinalValue));
end;

{ TJSONObject }

function TJSONObject.GetInitializer: String;
begin
  Result := TJSONConsts.cClassInitialization;
end;

function TJSONObject.GetFinalizer: String;
begin
  Result := TJSONConsts.cClassFinalization;
end;

{ TJSONNull }

function TJSONNull.Value: String;
begin
  Result := 'null';
end;

{ TJSONInteger }

constructor TJSONInteger.Create(pInteger: Int64);
begin
  FInteger := pInteger;
end;

function TJSONInteger.Value: string;
begin
  Result := FInteger.ToString;
end;

end.