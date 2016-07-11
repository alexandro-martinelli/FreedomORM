unit AM.Freedom.ObjectMapper.TriggerMapper;

interface

uses
  System.Generics.Collections,
  AM.Freedom.ObjectMapper.CustomTrigger,
  AM.Freedom.EnumerationTypes;

type
  TTriggerMapper = class sealed
  private
    FMetaClass: TTriggerClass;
    FTriggerOptions: TTriggerOptions;
  public
    constructor Create(pMetaClass: TTriggerClass; pTriggerOptions: TTriggerOptions);
    property TriggerClass: TTriggerClass read FMetaClass;
    property TriggerOptions: TTriggerOptions read FTriggerOptions;
  end;

  TTriggersMapper = class(TObjectList<TTriggerMapper>);

implementation

{ TTriggerMapper }

constructor TTriggerMapper.Create(pMetaClass: TTriggerClass; pTriggerOptions: TTriggerOptions);
begin
  FMetaClass      := pMetaClass;
  FTriggerOptions := pTriggerOptions;
end;
end.
