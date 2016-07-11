unit AM.Freedom.SQLMappers.Arguments;

interface

uses
  System.Classes,
  System.Rtti,
  System.Generics.Collections,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria.ValueCriteria,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.ArgumentDesignerRegister;

type
  TTableArgument = class(TSimpleArgument)
  private
    FSchema: String;
  protected
    class function AgumentDesigner: TArgumentDesigner; override;
  public
    constructor Create(const pName: String; pAlias: String = ''; pSchemaName: string = ''); overload;
    property Name;
    property Alias;
    property Schema: String read FSchema write FSchema;
  end;

  TFieldArgument = class(TSimpleArgument)
  private
    FTableAlias: String;
  protected
    class function AgumentDesigner: TArgumentDesigner; override;
  public
    constructor Create(const pName: string; const pTableAlias: String = ''; const pAlias: String = ''); overload;
    function HasTableAlias: Boolean;
    property Alias;
    property Name;
    property TableAlias: String read FTableAlias write FTableAlias;
  end;

  TCustomLiteralArgument = class abstract(TSimpleArgument)
  strict private
    FLiteralValue: String;
  strict protected
    function GetLiteralValue: String; virtual;
    property LiteralValue: String read FLiteralValue write FLiteralValue;
  public
    constructor Create(const pLiteralValue: string); overload; virtual;
  end;

  TLiteralArgument = class(TCustomLiteralArgument)
  protected
    class function AgumentDesigner: TArgumentDesigner; override;
  public
    property LiteralValue;
  end;

  TCustomInternalLiteralValue = class abstract(TCustomLiteralArgument)
  strict protected
    function GetInternalLiteralValue: String; virtual; abstract;
  public
    constructor Create; reintroduce;
    property LiteralValue: string read GetLiteralValue;
  end;

  TNullArgument = class(TCustomInternalLiteralValue)
  strict protected
    function GetInternalLiteralValue: string; override;
  protected
    class function AgumentDesigner: TArgumentDesigner; override;
  end;

  TValueArgument = class(TSimpleArgument)
  strict private
    FValue: Variant;
    FColumnType: TColumnType;
    FStream: TStream;
    function GetAsCurrency: Currency;
    function GetAsDate: TDate;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsExtended: Extended;
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    function GetAsSmallint: Smallint;
    function GetAsString: String;
    function GetAsTime: TTime;
    function GetAsBoolean: Boolean;
    function GetAsStream: TStream;
    function GetAsSingle: Single;
    function GetAsByte: Byte;
    constructor Create(pValue: Variant; pColumnType: TColumnType); overload; virtual;
    constructor Create(pStream: TStream; pColumnType: TColumnType); overload; virtual;
  protected
    class function AgumentDesigner: TArgumentDesigner; override;
  public
    destructor Destroy; override;
    class function CreateAs<T>(pValue: T; pBlobType: TColumnType = ctyMemo): TValueArgument;
    class function CreateAsString(pValue: String): TValueArgument;
    class function CreateAsSmallint(pValue: Smallint): TValueArgument;
    class function CreateAsInteger(pValue: Integer): TValueArgument;
    class function CreateAsInt64(pValue: Int64): TValueArgument;
    class function CreateAsDouble(pValue: Double): TValueArgument;
    class function CreateAsExtended(pValue: Extended): TValueArgument;
    class function CreateAsCurrency(pValue: Currency): TValueArgument;
    class function CreateAsDate(pValue: TDate): TValueArgument;
    class function CreateAsTime(pValue: TTime): TValueArgument;
    class function CreateAsDateTime(pValue: TDateTime): TValueArgument;
    class function CreateAsByte(pValue: Byte): TValueArgument;
    class function CreateAsBoolean(pValue: Boolean): TValueArgument;
    class function CreateAsSingle(pValue: Single): TValueArgument;
    class function CreateAsStream(pValue: TStream; pBlobType: TColumnType): TValueArgument;
    class function CreateAsVariant(pValue: Variant): TValueArgument;
    class function CreateAsZero: TValueArgument;
    class function CreateAsBlank: TValueArgument;
    class function CreateAsNow: TValueArgument;
    property ValueType: TColumnType read FColumnType;
    property AsString: String read GetAsString;
    property AsSmallint: Smallint read GetAsSmallint;
    property AsInteger: Integer read GetAsInteger;
    property AsInt64: Int64 read GetAsInt64;
    property AsDouble: Double read GetAsDouble;
    property AsExtended: Extended read GetAsExtended;
    property AsCurrency: Currency read GetAsCurrency;
    property AsDate: TDate read GetAsDate;
    property AsTime: TTime read GetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime;
    property AsStream: TStream read GetAsStream;
    property AsBoolean: Boolean read GetAsBoolean;
    property AsSingle: Single read GetAsSingle;
    property AsByte: Byte read GetAsByte;

    property Value: Variant read FValue;
  end;

  TBetweenArgument = class(TSimpleArgument)
  strict private
    FBetweenArgument: TCustomArgument;
    FAndArgument: TCustomArgument;
    procedure ValidArgument(pArgument: TCustomArgument);
  strict protected
    procedure SetBetweenArgument(const pArgument: TCustomArgument);
    procedure SetAndArgument(const pArgument: TCustomArgument);
  public
    constructor Create(const pBetweenArgument, pAndArgument: TCustomArgument); virtual;
    destructor Destroy; override;
    property BetweenArgument: TCustomArgument read FBetweenArgument write SetBetweenArgument;
    property AndArgument: TCustomArgument read FAndArgument write SetAndArgument;
  end;

  TInArgument = class(TSimpleArgument)
  strict private
    FInArguments: TObjectList<TCustomArgument>;
  public
    constructor Create(pArguments: Array of TCustomArgument); virtual;
    destructor Destroy; override;
    property InArguments: TObjectList<TCustomArgument> read FInArguments;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.Exceptions,
  System.TypInfo,
  System.Variants,
  AM.Freedom.Helper.Variant;

{ TFieldArgument }

class function TFieldArgument.AgumentDesigner: TArgumentDesigner;
begin
  Result := TArgumentDesigner.Create;
  Result.ArgumentClassName := ClassName;
  Result.ArgumentCreateName := 'Create';
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pName', dptString, True));
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pTableAlias', dptString, False));
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pAlias', dptString, False));
end;

constructor TFieldArgument.Create(const pName, pTableAlias, pAlias: String);
begin
  Name := pName;
  Alias := pAlias;
  FTableAlias := pTableAlias;
end;

function TFieldArgument.HasTableAlias: Boolean;
begin
  Result := (FTableAlias = '') and (Pos('.', Name) <= 0);
end;

{ TLiteralField }

constructor TValueArgument.Create(pValue: Variant; pColumnType: TColumnType);
begin
  inherited Create;
  FValue := pValue;
  FColumnType := pColumnType;
end;

class function TValueArgument.AgumentDesigner: TArgumentDesigner;
begin
  Result := TArgumentDesigner.Create;
  Result.ArgumentClassName := ClassName;
  Result.ArgumentCreateName := 'CreateAs%s';
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pValue', dptVariant, True));
end;

constructor TValueArgument.Create(pStream: TStream; pColumnType: TColumnType);
begin
  inherited Create;
  FStream := pStream;
  FColumnType := pColumnType;
  FValue := Null;
end;

class function TValueArgument.CreateAs<T>(pValue: T; pBlobType: TColumnType): TValueArgument;
var
  lValue: TValue;
  lIsInvalidArgument: Boolean;
begin
  lValue := TValue.From<T>(pValue);
  lIsInvalidArgument := False;
  if TTypeInfo(TypeInfo(T)^).Kind <> tkClass then
  begin
    if TypeInfo(T) = TypeInfo(String) then
    begin
      Result := TValueArgument.Create(lValue.AsString, ctyString);
    end
    else if TypeInfo(T) = TypeInfo(Char) then
    begin
      Result := TValueArgument.Create(lValue.AsType<Char>, ctyChar);
    end
    else if TypeInfo(T) = TypeInfo(Byte) then
    begin
      Result := TValueArgument.Create(lValue.AsType<Byte>, ctyByte);
    end
    else if TypeInfo(T) = TypeInfo(Smallint) then
    begin
      Result := TValueArgument.Create(lValue.AsType<Smallint>, ctySmallint);
    end
    else if TypeInfo(T) = TypeInfo(Integer) then
    begin
      Result := TValueArgument.Create(lValue.AsInteger, ctyInteger);
    end
    else if TypeInfo(T) = TypeInfo(Int64) then
    begin
      Result := TValueArgument.Create(lValue.AsInt64, ctyInt64);
    end
    else if TypeInfo(T) = TypeInfo(TDate) then
    begin
      Result := TValueArgument.Create(lValue.AsType<TDate>, ctyDate);
    end
    else if TypeInfo(T) = TypeInfo(TTime) then
    begin
      Result := TValueArgument.Create(lValue.AsType<TTime>, ctyTime);
    end
    else if TypeInfo(T) = TypeInfo(TDateTime) then
    begin
      Result := TValueArgument.Create(lValue.AsType<TDateTime>, ctyDateTime);
    end
    else if TypeInfo(T) = TypeInfo(Single) then
    begin
      Result := TValueArgument.Create(lValue.AsType<Single>, ctySingle);
    end
    else if TypeInfo(T) = TypeInfo(Double) then
    begin
      Result := TValueArgument.Create(lValue.AsType<Double>, ctyDouble);
    end
    else if TypeInfo(T) = TypeInfo(Extended) then
    begin
      Result := TValueArgument.Create(lValue.AsExtended, ctyExtended);
    end
    else if TypeInfo(T) = TypeInfo(Currency) then
    begin
      Result := TValueArgument.Create(lValue.AsCurrency, ctyCurrency);
    end
    else if TypeInfo(T) = TypeInfo(Boolean) then
    begin
      Result := TValueArgument.Create(lValue.AsType<Boolean>, ctyBoolean);
    end
    else
    begin
      lIsInvalidArgument := True;
    end;
  end
  else
  begin
    if lValue.AsObject.ClassType.InheritsFrom(TStream) then
    begin
      Result := TValueArgument.CreateAsStream(lValue.AsType<TStream>, pBlobType);
    end
    else
    begin
      lIsInvalidArgument := True;
    end;
  end;
  if lIsInvalidArgument then
  begin
    raise EInvalidArgumentValue.Create(String(TTypeInfo(TypeInfo(T)^).Name));
  end;
end;

class function TValueArgument.CreateAsBoolean(pValue: Boolean): TValueArgument;
begin
  Result := TValueArgument.Create(pValue, ctyBoolean);
end;

class function TValueArgument.CreateAsByte(pValue: Byte): TValueArgument;
begin
  Result := TValueArgument.Create(pValue, ctyByte);
end;

class function TValueArgument.CreateAsCurrency(pValue: Currency): TValueArgument;
begin
  Result := Self.Create(pValue, ctyCurrency);
end;

class function TValueArgument.CreateAsDate(pValue: TDate): TValueArgument;
begin
  Result := Self.Create(pValue, ctyDate);
end;

class function TValueArgument.CreateAsDateTime(pValue: TDateTime): TValueArgument;
begin
  Result := Self.Create(pValue, ctyDateTime);
end;

class function TValueArgument.CreateAsDouble(pValue: Double): TValueArgument;
begin
  Result := Self.Create(pValue, ctyDouble);
end;

class function TValueArgument.CreateAsExtended(pValue: Extended): TValueArgument;
begin
  Result := Self.Create(pValue, ctyExtended);
end;

class function TValueArgument.CreateAsInt64(pValue: Int64): TValueArgument;
begin
  Result := Self.Create(pValue, ctyInt64);
end;

class function TValueArgument.CreateAsInteger(pValue: Integer): TValueArgument;
begin
  Result := Self.Create(pValue, ctyInteger);
end;

class function TValueArgument.CreateAsSingle(pValue: Single): TValueArgument;
begin
  Result := Self.Create(pValue, ctySingle);
end;

class function TValueArgument.CreateAsSmallint(pValue: Smallint): TValueArgument;
begin
  Result := Self.Create(pValue, ctySmallint);
end;

class function TValueArgument.CreateAsStream(pValue: TStream; pBlobType: TColumnType): TValueArgument;
begin
  Result := Self.Create(pValue, pBlobType);
end;

class function TValueArgument.CreateAsString(pValue: String): TValueArgument;
begin
  Result := Self.Create(pValue, ctyString);
end;

class function TValueArgument.CreateAsTime(pValue: TTime): TValueArgument;
begin
  Result := Self.Create(pValue, ctyTime);
end;

class function TValueArgument.CreateAsVariant(pValue: Variant): TValueArgument;
begin
  Result := nil;
  case pValue.VariantType of
    ctyByte, ctySmallint, ctyInteger: Result := TValueArgument.CreateAsInteger(pValue);
    ctyInt64, ctyEnumerator: Result := TValueArgument.CreateAsInt64(pValue);
    ctyChar, ctyString: Result := TValueArgument.CreateAsString(pValue);
    ctySingle, ctyDouble, ctyCurrency, ctyExtended: Result := TValueArgument.CreateAsExtended(pValue);
    ctyDate: Result := TValueArgument.CreateAsDate(pValue);
    ctyTime: Result := TValueArgument.CreateAsTime(pValue);
    ctyDateTime: Result := TValueArgument.CreateAsDateTime(pValue);
    ctyBoolean: Result := TValueArgument.CreateAsBoolean(pValue);
  end;
end;

destructor TValueArgument.Destroy;
begin
  if Assigned(FStream) then
  begin
    FStream.Free;
  end;
  inherited;
end;

function TValueArgument.GetAsBoolean: Boolean;
begin
  Result := FValue.TryToBoolean;
end;

function TValueArgument.GetAsByte: Byte;
begin
  Result := FValue.TryToByte;
end;

function TValueArgument.GetAsCurrency: Currency;
begin
  Result := FValue.TryToCurrency;
end;

function TValueArgument.GetAsDate: TDate;
begin
  Result := FValue.TryToDate;
end;

function TValueArgument.GetAsDateTime: TDateTime;
begin
  Result := FValue.TryToDateTime;
end;

function TValueArgument.GetAsDouble: Double;
begin
  Result := FValue.TryToDouble;
end;

function TValueArgument.GetAsExtended: Extended;
begin
  Result := FValue.TryToExtended;
end;

function TValueArgument.GetAsInt64: Int64;
begin
  Result := FValue.TryToInt64;
end;

function TValueArgument.GetAsInteger: Integer;
begin
  Result := FValue.TryToInt;
end;

function TValueArgument.GetAsSingle: Single;
begin
  Result := FValue.TryToSingle;
end;

function TValueArgument.GetAsSmallint: Smallint;
begin
  Result := FValue.TryToInt;
end;

function TValueArgument.GetAsStream: TStream;
begin
  Result := FStream;
end;

function TValueArgument.GetAsString: String;
begin
  Result := FValue.ToString;
end;

function TValueArgument.GetAsTime: TTime;
begin
  Result := FValue.TryToTime;
end;
{ TCustomLiteralArgument }

constructor TCustomLiteralArgument.Create(const pLiteralValue: string);
begin
  inherited Create;
  FLiteralValue := pLiteralValue;
end;

function TCustomLiteralArgument.GetLiteralValue: String;
begin
  Result := FLiteralValue;
end;
{ TCustomInternalLiteralValue }

constructor TCustomInternalLiteralValue.Create;
begin
  inherited Create(GetInternalLiteralValue);
end;
{ TBetweenArgument }

constructor TBetweenArgument.Create(const pBetweenArgument, pAndArgument: TCustomArgument);
begin
  inherited Create;
  SetBetweenArgument(pBetweenArgument);
  SetAndArgument(pAndArgument);
end;

destructor TBetweenArgument.Destroy;
begin
  FreeAndnil(FBetweenArgument);
  FreeAndnil(FAndArgument);
  inherited;
end;

procedure TBetweenArgument.ValidArgument(pArgument: TCustomArgument);
begin
  if (pArgument <> nil) and ((pArgument.ClassType = TTableArgument) or (pArgument.ClassType = TBetweenArgument) or (pArgument.ClassType = TInArgument)) then
  begin
    raise EInvalidArgument.Create(pArgument.ClassName, Self.ClassName);
  end;
end;

procedure TBetweenArgument.SetAndArgument(const pArgument: TCustomArgument);
begin
  ValidArgument(pArgument);
  if FAndArgument <> pArgument then
  begin
    FreeAndnil(FAndArgument);
  end;
  FAndArgument := pArgument
end;

procedure TBetweenArgument.SetBetweenArgument(const pArgument: TCustomArgument);
begin
  ValidArgument(pArgument);
  if FBetweenArgument <> pArgument then
  begin
    FreeAndnil(FBetweenArgument);
  end;
  FBetweenArgument := pArgument
end;
{ TInArgument }

constructor TInArgument.Create(pArguments: Array of TCustomArgument);
var
  I: Integer;
begin
  FInArguments := TObjectList<TCustomArgument>.Create;
  for I := Low(pArguments) to High(pArguments) do
  begin
    FInArguments.Add(pArguments[I]);
  end;
end;

destructor TInArgument.Destroy;
begin
  FreeAndnil(FInArguments);
  inherited;
end;
{ TTableArgument }

class function TTableArgument.AgumentDesigner: TArgumentDesigner;
begin
  Result := TArgumentDesigner.Create;
  Result.ArgumentClassName := ClassName;
  Result.ArgumentCreateName := 'Create';
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pName', dptString, False));
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pAlias', dptString, True));
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pSchemaName', dptString, True));
end;

constructor TTableArgument.Create(const pName: String; pAlias, pSchemaName: string);
begin
  Name := pName;
  Alias := pAlias;
  FSchema := pSchemaName;
end;
{ TNullArgument }

class function TNullArgument.AgumentDesigner: TArgumentDesigner;
begin
  Result := TArgumentDesigner.Create;
  Result.ArgumentClassName := ClassName;
  Result.ArgumentCreateName := 'Create';
end;

function TNullArgument.GetInternalLiteralValue: string;
begin
  Result := 'null';
end;

{ TLiteralArgument }

class function TLiteralArgument.AgumentDesigner: TArgumentDesigner;
begin
  Result := TArgumentDesigner.Create;
  Result.ArgumentClassName := ClassName;
  Result.ArgumentCreateName := 'Create';
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pLiteralValue', dptString, True));
end;

class function TValueArgument.CreateAsZero: TValueArgument;
begin
  Result := TValueArgument.CreateAsInteger(0);
end;

class function TValueArgument.CreateAsBlank: TValueArgument;
begin
  Result := TValueArgument.CreateAsString('');
end;

class function TValueArgument.CreateAsNow: TValueArgument;
begin
  Result := TValueArgument.CreateAsDateTime(Now);
end;

initialization
  TArgumentDesignerRegister.RegisterArgumentDesigner(TFieldArgument.AgumentDesigner);
  TArgumentDesignerRegister.RegisterArgumentDesigner(TValueArgument.AgumentDesigner);
  TArgumentDesignerRegister.RegisterArgumentDesigner(TTableArgument.AgumentDesigner);
  TArgumentDesignerRegister.RegisterArgumentDesigner(TLiteralArgument.AgumentDesigner);
  TArgumentDesignerRegister.RegisterArgumentDesigner(TNullArgument.AgumentDesigner);

end.
