unit AM.Freedom.SQLCommands.FieldOptions;

interface

uses
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.EnumerationTypes;

type
  TFieldOptionsClass = class of TCustomFieldOptions;

  TCustomFieldOptions = class abstract
  strict private
    FNullable: TNullableChange;
    FDefault: TCustomArgument;
  strict protected
    procedure SetDefault(const Value: TCustomArgument); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Nullable: TNullableChange read FNullable write FNullable default nDontChange;
    property Default: TCustomArgument read FDefault write SetDefault;
  end;

  TFieldOptions = class(TCustomFieldOptions)
  private
    FIdentity: Boolean;
  public
    property Identity: Boolean read FIdentity write FIdentity;
  end;

  TDomainOptions = class(TCustomFieldOptions);

implementation

uses
  System.SysUtils;


{ TCustomFieldOptions }

constructor TCustomFieldOptions.Create;
begin
  FNullable := nDontChange;
end;

destructor TCustomFieldOptions.Destroy;
begin
  FreeAndNil(FDefault);
  inherited;
end;

procedure TCustomFieldOptions.SetDefault(const Value: TCustomArgument);
begin
  if Assigned(FDefault) then
  begin
    FreeAndNil(FDefault);
  end;
  FDefault := Value;
end;

end.
