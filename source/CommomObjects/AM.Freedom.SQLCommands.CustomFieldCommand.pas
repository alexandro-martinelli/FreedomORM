unit AM.Freedom.SQLCommands.CustomFieldCommand;

interface

uses
  AM.Freedom.SQLMappers.NamedObject,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.SQLCommands.FieldOptions;

type
  TFieldCommandClass = class of TCustomFieldCommand;

  TCustomFieldCommand = class(TNamedObject)
  strict private
    FFieldOptions: TFieldOptions;
  strict private
    FDomain: String;
  protected
    procedure SetFieldOptions(const pFieldOptions: TFieldOptions); virtual;
  public
    constructor Create; overload;
    constructor Create(pName: String; pDomainName: String = ''; pFieldOptions: TFieldOptions = nil); overload;
    constructor Create(pName: String; pNullable: TNullableChange; pDomainName: String = ''; pDefault: TCustomArgument = nil;
        pDescription: String = ''; pIdentity: Boolean = False); overload;
    destructor Destroy; override;
    property Name;
    property FieldOptions: TFieldOptions read FFieldOptions write SetFieldOptions;
    property Domain: String read FDomain write FDomain;
  end;

implementation

uses
  System.SysUtils;
{ TCustomFieldCommand }

constructor TCustomFieldCommand.Create;
begin
  SetFieldOptions(TFieldOptions.Create);
end;

constructor TCustomFieldCommand.Create(pName: String; pDomainName: String; pFieldOptions: TFieldOptions);
begin
  Create;
  Name := pName;
  FDomain := pDomainName;
  if Assigned(pFieldOptions) then
  begin
    SetFieldOptions(pFieldOptions);
  end
  else
  begin
    SetFieldOptions(TFieldOptions.Create);
  end;
end;

constructor TCustomFieldCommand.Create(pName: String; pNullable: TNullableChange; pDomainName: String; pDefault: TCustomArgument; pDescription: String; pIdentity: Boolean);
var
  lFieldOptions: TFieldOptions;
begin
  lFieldOptions := TFieldOptions.Create;
  lFieldOptions.Nullable := pNullable;
  lFieldOptions.Default := pDefault;
  lFieldOptions.Identity := pIdentity;
  Create(pName, pDomainName, lFieldOptions);
end;

destructor TCustomFieldCommand.Destroy;
begin
  FreeAndNil(FFieldOptions);
  inherited;
end;

procedure TCustomFieldCommand.SetFieldOptions(const pFieldOptions: TFieldOptions);
begin
  if Assigned(FFieldOptions) then
  begin
    FreeAndNil(FFieldOptions);
  end;
  FFieldOptions := pFieldOptions;
end;

end.
