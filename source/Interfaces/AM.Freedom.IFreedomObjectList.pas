unit AM.Freedom.IFreedomObjectList;

interface

uses
  AM.Freedom.IPersistent,
  AM.Freedom.GroupCriteria,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.ObjectMapper,
  AM.Freedom.SQLMappers.CustomSelect,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ICursor;

type
  IFreedomObjectList = interface
    ['{F35FD812-8E63-4815-8B5A-EE4CF6608872}']
    function FindObjectByProperty(pPropertyNames: String; pPropertyValue: Variant; pFindStrOptions: TFindStrOptions = []): TObject;
    procedure SetRefColumnValue(pRefColumnName: String; pColumnValue: Variant);
    function GetPersistent: IPersistent;
    procedure DoSearch(pGroupCriteria: TGroupCriteria);
    procedure DoSearchWithSelect(pSelect: TCustomSelect);
    procedure Assign(pCursor: TCursorList; pObjectMapper: TObjectMapper);
    procedure SetPersistState(pObjectState: TObjectState);
    procedure PersistObjects;
    procedure UnpersistObjects;
    procedure Clear;
    function IsEmpty: Boolean;
  end;

implementation

end.
