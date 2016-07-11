unit AM.Freedom.IFreedomObject;

interface

uses
  AM.Freedom.ObjectMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria,
  AM.Freedom.IPersistent,
  AM.Freedom.ObjectMapper.ColumnValueItem;

type
  IFreedomObject = interface
    ['{16D0DBAA-EDDA-4791-B580-3D4DE1F8B98C}']
    procedure DoFindWithID(pID: Variant);
    function GetColumnValueList: TColumnValueList;
    procedure SetColumnValueList(pColumnValueList: TColumnValueList);
    function GetObjectState: TObjectState;
    function GetOldObjectState: TObjectState;
    procedure SetOldObjectState(const pState: TObjectState);
    procedure SetObjectState(const pState: TObjectState);
    procedure Persist(pObjectState: TObjectState; pPersistent: IPersistent = nil);
    procedure DoSearch(pGroupCriteria: TGroupCriteria);
    procedure RevertState;
    procedure AssignInitialValues;
    procedure Initialize;

    property ObjectState: TObjectState read GetObjectState;
    property OldObjectState: TObjectState read GetOldObjectState;
  end;

implementation

end.
