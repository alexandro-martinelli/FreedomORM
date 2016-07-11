unit AM.Freedom.LiveRefreshObject;

interface

uses
  System.SysUtils,
  AM.Freedom.EnumerationTypes;

type
  TLiveRefreshObject = class
  private
    FValue: Variant;
    FRefColumnName: String;
    FColumnType: TColumnType;
    FColumnName: String;
  public
    property Value: Variant read FValue write FValue;
    property ColumnName: String read FColumnName write FColumnName;
    property RefColumnName: String read FRefColumnName write FRefColumnName;
    property ColumnType: TColumnType read FColumnType write FColumnType;
  end;

implementation

end.
