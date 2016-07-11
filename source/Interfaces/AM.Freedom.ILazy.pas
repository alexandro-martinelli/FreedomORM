unit AM.Freedom.ILazy;

interface

uses
  AM.Freedom.GroupCriteria,
  AM.Freedom.ObjectMapper.LiveRefresh,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.LiveRefreshObject;

type
  TOnLazyLiveRefresh = function(pLazy: TObject): TGroupCriteria of object;

  TOnGetLiveRefreshColumnValue = function(pLazyObject: TObject): TLiveRefreshObject of object;

  ILazy = interface
    ['{339A4F30-FA70-4950-9C64-6D446F3C9B6B}']
    procedure SetLazySearch(const Value: TGroupCriteria);
    function GetLoaded: Boolean;
    function GetObject: TObject;
    function GetLazyID: Variant;
    procedure SetLazyID(pLazyID: Variant);
    procedure Load;
    procedure SetFreeInternalValue;
    procedure SetOnGetLiveRefreshColumnValue(pOnGetLiveRefreshColumnValue: TOnGetLiveRefreshColumnValue);
    function GetLazyType: TLazyType;
    function GetClassType: TClass;
    property Loaded: Boolean read GetLoaded;
  end;

implementation

end.

