unit AM.Freedom.SQLCommands.Fields;

interface

uses
  AM.Freedom.SQLCommands.SizedFieldCommand,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.SQLCommands.ScaledFieldCommand;

type
  TVarcharFieldCommand = class(TSizedFieldCommand);
  TCharFieldCommand = class(TSizedFieldCommand);
  TByteFieldCommand = class(TCustomFieldCommand);
  TSmallintFieldCommand = class(TCustomFieldCommand);
  TIntegerFieldCommand = class(TCustomFieldCommand);
  TInt64FieldCommand = class(TCustomFieldCommand);
  TDateFieldCommand = class(TCustomFieldCommand);
  TTimeFieldCommand = class(TCustomFieldCommand);
  TDateTimeFieldCommand = class(TCustomFieldCommand);
  TFloatFieldCommand = class(TCustomFieldCommand);
  TNumericFieldCommand = class(TScaledFieldCommand);
  TBlobFieldCommand = class(TSizedFieldCommand);
  TMemoFieldCommand = class(TCustomFieldCommand);
  TXMLFieldCommand = class(TCustomFieldCommand);
  TBooleanFieldCommand = class(TCustomFieldCommand);
  TDoubleFieldCommand = class(TCustomFieldCommand);

implementation

end.
