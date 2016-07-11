unit AM.Freedom.ObjectMapper.CustomTrigger;

interface

uses
  AM.Freedom.ITrigger;

type
  TTriggerClass = class of TCustomTrigger;

  TCustomTrigger = class abstract(TInterfacedObject, ITrigger)
  public
    procedure BeforeInsert(AObject: TObject); virtual; abstract;
    procedure BeforeUpdate(AObject: TObject); virtual; abstract;
    procedure BeforeDelete(AObject: TObject); virtual; abstract;
    procedure AfterInsert(AObject: TObject); virtual; abstract;
    procedure AfterUpdate(AObject: TObject); virtual; abstract;
    procedure AfterDelete(AObject: TObject); virtual; abstract;
  end;

implementation

end.
