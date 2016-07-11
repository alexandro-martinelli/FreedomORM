unit AM.Freedom.IPersistent;

interface

uses
  System.Generics.Collections,
  AM.Freedom.GroupCriteria,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.Persistent.SetCursorResult,
  AM.Freedom.SQLMappers.CustomSelect,
  AM.Freedom.ICursor,
  AM.Freedom.RoundObject,
  AM.Freedom.ObjectMapper;

type
  IPersistent = interface
    ['{F98575AB-35F5-42BC-B0F6-2EAC6563C937}']
    function CompareDDLColumnTypes(pSourceDDLColumnType, pDestinyDDLColumnType: TColumnType): Boolean;
    function FixIntColumnTypeForDDLColumn(pColumnType: TColumnType): TColumnType;
    function GetCursor(pClass: TClass; pGroupCriteria: TGroupCriteria; pMapper: TObjectMapper): ICursor;
    function GetCursorList(pClass: TClass; pGroupCriteria: TGroupCriteria; pMapper: TObjectMapper): TCursorList;
    function GetCursorWithSelect(pSelect: TCustomSelect): ICursor;
    function GetCursorListWithSelect(pSelect: TCustomSelect): TCursorList;
    function SetCursor(pObject: TObject; pObjectState: TObjectState): TSetCursorResult;
    procedure UpdatePersistent(pClass: TClass; pUpdateObjectOptions: TUpdateObjectOptions);
    procedure DropPersistent(pClass: TClass);
    function AdjustNameLength(pName: String): String;
    procedure BeginUpdate(pOwnsUpdate: TObject);
    procedure EndUpdate(pOwnsUpdate: TObject);
    procedure EndUpdateOnError(pOwnsUpdate: TObject);
    procedure ModifyRoundObject(pRoundObject: TRoundObject);
  end;

implementation

end.
