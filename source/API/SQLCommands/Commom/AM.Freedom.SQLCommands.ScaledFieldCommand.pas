unit AM.Freedom.SQLCommands.ScaledFieldCommand;

interface

uses
  AM.Freedom.SQLCommands.SizedFieldCommand,
  AM.Freedom.SQLMapper.CustomArgument, AM.Freedom.EnumerationTypes;

type
  TScaledFieldCommand = class abstract(TSizedFieldCommand)
  strict private
    FScale: Byte;
  public
    constructor Create(pName: String; pSize: UInt16; pScale: Byte; pNullable: TNullableChange = nDontChange;
        pDomainName: String = ''; pDefault: TCustomArgument = nil; pDescription: String = '';
        pIdentity: Boolean = False); overload;
    property Scale: Byte read FScale write FScale;
  end;

implementation

{ TScaledFieldCommand }

constructor TScaledFieldCommand.Create(pName: String; pSize: UInt16; pScale: Byte; pNullable: TNullableChange; pDomainName: String;
    pDefault: TCustomArgument; pDescription: String; pIdentity: Boolean);
begin
  inherited Create(pName, pSize, pNullable, pDomainName, pDefault, pDescription, pIdentity);
  FScale := pScale;
end;

end.
