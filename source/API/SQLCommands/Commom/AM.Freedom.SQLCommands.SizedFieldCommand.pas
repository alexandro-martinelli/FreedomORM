unit AM.Freedom.SQLCommands.SizedFieldCommand;

interface

uses
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLMapper.CustomArgument, AM.Freedom.EnumerationTypes;

type
  TSizedFieldCommand = class abstract(TCustomFieldCommand)
  strict private
    FSize: UInt16;
  public
   constructor Create(pName: String; pSize: UInt16; pNullable: TNullableChange = nDontChange; pDomainName: String = '';
       pDefault: TCustomArgument = nil; pDescription: String = ''; pIdentity: Boolean = False); overload;
    property Size: UInt16 read FSize write FSize;
  end;

implementation

{ TStringFieldCommand }

constructor TSizedFieldCommand.Create(pName: String; pSize: UInt16; pNullable: TNullableChange; pDomainName: String;
    pDefault: TCustomArgument; pDescription: String; pIdentity: Boolean);
begin
  inherited Create(pName, pNullable, pDomainName, pDefault, pDescription, pIdentity);
  FSize := pSize;
end;

end.
