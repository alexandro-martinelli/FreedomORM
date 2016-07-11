unit AM.Freedom.ITrigger;

interface

type
  ITrigger = interface
    ['{7CBCD1B6-16CB-4C39-8E50-280E2A3E38B1}']
    procedure BeforeInsert(AObject: TObject);
    procedure BeforeUpdate(AObject: TObject);
    procedure BeforeDelete(AObject: TObject);
    procedure AfterInsert(AObject: TObject);
    procedure AfterUpdate(AObject: TObject);
    procedure AfterDelete(AObject: TObject);
  end;

implementation

end.
