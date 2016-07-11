unit AM.Freedom.ObjectMapper.ColumnOptions;

interface

uses
  System.Rtti,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.Schemas, AM.Freedom.Helper.RttiField, AM.Freedom.Helper.RttiProperty;

type
  TIdOptions = class sealed
  private
    FSequenceName: string;
    FIdOption: TIdOption;
    FIsId: Boolean;
    FSchemas: TSchemas;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(pSource: TIdOptions);
    function IsValidSequence(pSchemaName: String): Boolean;
    property SequenceName: string read FSequenceName write FSequenceName;
    property IdOption: TIdOption read FIdOption write FIdOption;
    property IsId: Boolean read FIsId write FIsId default False;
    property Schemas: TSchemas read FSchemas;
  end;

  TOrderOptions = class sealed
  private
    FIndex: Integer;
    FOrderType: TOrderType;
    FDirective: String;
  public
    property Index: Integer read FIndex write FIndex;
    property OrderType: TOrderType read FOrderType write FOrderType;
    property Directive: String read FDirective write FDirective;
  end;

  TDefaultValueOptions = class sealed
  private
    FIsNow: Boolean;
    FValue: Variant;
  public
    constructor Create;
    procedure Assign(pSource: TDefaultValueOptions);
    function HaveDefaultValue: Boolean;
    property IsNow: Boolean read FIsNow write FIsNow;
    property Value: Variant read FValue write FValue;
  end;

  TLazyOptions = class
  private
    FIsLazyLoaded: Boolean;
    FIsLazy: Boolean;
    FLazyType: TLazyType;
    FLazyClassType: TClass;
  public
    constructor Create;
    procedure Assign(pSource: TLazyOptions);
    property IsLazy: Boolean read FIsLazy write FIsLazy;
    property IsLazyLoaded: Boolean read FIsLazyLoaded write FIsLazyLoaded;
    property LazyType: TLazyType read FLazyType write FLazyType;
    property LazyClassType: TClass read FLazyClassType write FLazyClassType;
  end;

  TRttiColumnOptions = class sealed
  private
    FRttiField: TRttiField;
    FRttiFieldHelper: TRttiFieldHelper;
    FRttiProperty: TRttiProperty;
    FRttiType: TRttiType;
    FRttiObject: TObject;

    function GetIsField: Boolean;
    function GetIsProperty: Boolean;
    function GetRttiName: String;
    procedure SetRttiField(const Value: TRttiField);
  public
    destructor Destroy; override;
    procedure Assign(pSource: TRttiColumnOptions);
    procedure Clear;
    property RttiField: TRttiField read FRttiField write SetRttiField;
    property RttiFieldHelper: TRttiFieldHelper read FRttiFieldHelper;
    property RttiProperty: TRttiProperty read FRttiProperty write FRttiProperty;
    property RttiType: TRttiType read FRttiType write FRttiType;
    property RttiObject: TObject read FRttiObject write FRttiObject;
    property IsField: Boolean read GetIsField;
    property IsProperty: Boolean read GetIsProperty;
    property RttiName: String read GetRttiName;
  end;

  TBindOptions = class sealed
  private
    FDisplayFormat: String;
    FDisplayLabel: String;
  public
    property DisplayFormat: String read FDisplayFormat write FDisplayFormat;
    property DisplayLabel: String read FDisplayLabel write FDisplayLabel;
  end;

  TRoundOptions = class sealed
  private
    FRoundDecimalsMode: TRoundDecimalsMode;
    FRoundDecimals: Byte;
    FIsRounded: Boolean;
    FCanBeModified: Boolean;
  public
    constructor Create;
    property IsRounded: Boolean read FIsRounded write FIsRounded;
    property RoundDecimals: Byte read FRoundDecimals write FRoundDecimals;
    property RoundDecimalsMode: TRoundDecimalsMode read FRoundDecimalsMode write FRoundDecimalsMode default rdmApproach;
    property CanBeModified: Boolean read FCanBeModified write FCanBeModified;
  end;

implementation

uses
  System.SysUtils,
  System.Variants,
  AM.Freedom.Helper.Variant;

{ TLazyOptions }

procedure TLazyOptions.Assign(pSource: TLazyOptions);
begin
  FIsLazyLoaded := pSource.IsLazyLoaded;
  FIsLazy := pSource.IsLazy;
  FLazyType := pSource.LazyType;
  FLazyClassType := pSource.LazyClassType;
end;

constructor TLazyOptions.Create;
begin
  FIsLazyLoaded := False;
  FIsLazy := False;
  FLazyType := TLazyType.Simple;
end;

{ TRttiOptions }

procedure TRttiColumnOptions.Assign(pSource: TRttiColumnOptions);
begin
  FRttiField := pSource.RttiField;
  FRttiType := pSource.RttiType;
  FRttiObject := pSource.RttiObject;
end;

procedure TRttiColumnOptions.Clear;
begin
  FRttiField := nil;
  FRttiType := nil;
  FRttiObject := nil;
end;

destructor TRttiColumnOptions.Destroy;
begin
  FRttiField := nil;
  FRttiType := nil;
  FRttiObject := nil;
  FreeAndNil(FRttiFieldHelper);
  inherited;
end;

function TRttiColumnOptions.GetIsField: Boolean;
begin
  Result := Assigned(FRttiField);
end;

function TRttiColumnOptions.GetIsProperty: Boolean;
begin
  Result := Assigned(FRttiProperty);
end;

function TRttiColumnOptions.GetRttiName: String;
begin
  if (IsField) then
  begin
    Result := FRttiField.Name;
  end
  else if (IsProperty) then
  begin
    Result := FRttiProperty.Name;
  end;
end;

procedure TRttiColumnOptions.SetRttiField(const Value: TRttiField);
begin
  FreeAndNil(FRttiFieldHelper);
  FRttiField := Value;
  if (Assigned(Value)) then
  begin
    FRttiFieldHelper := TRttiFieldHelper.Create(FRttiField);
  end;
end;

{ TIdOptions }

procedure TIdOptions.Assign(pSource: TIdOptions);
var
  lSchema: TSchemaItem;
begin
  FSequenceName := pSource.SequenceName;
  FIdOption := pSource.IdOption;
  FIsId := pSource.IsId;
  FSchemas.Clear;
  for lSchema in pSource.Schemas do
  begin
    FSchemas.AddSchema(lSchema.Name, lSchema.Default);
  end;
end;

constructor TIdOptions.Create;
begin
  FSchemas := TSchemas.Create;
end;

destructor TIdOptions.Destroy;
begin
  FSchemas.Free;
  inherited;
end;

function TIdOptions.IsValidSequence(pSchemaName: String): Boolean;
begin
  Result := (Self.Schemas.FindSchema(pSchemaName) <> nil) or ((pSchemaName = '') and (Self.Schemas.Count = 0));
  if Result then
  begin
    Result := (FSequenceName <> '') and (FIdOption = Sequence);
  end;
end;

{ TDefaultValueOptions }

procedure TDefaultValueOptions.Assign(pSource: TDefaultValueOptions);
begin
  FIsNow := pSource.IsNow;
  FValue := pSource.Value;
end;

constructor TDefaultValueOptions.Create;
begin
  FIsNow := False;
  FValue := Null;
end;

function TDefaultValueOptions.HaveDefaultValue: Boolean;
begin
  Result := FIsNow or not FValue.IsNull;
end;

{ TRoundOptions }

constructor TRoundOptions.Create;
begin
  FRoundDecimalsMode := rdmApproach;
  FCanBeModified := True;
end;

end.
