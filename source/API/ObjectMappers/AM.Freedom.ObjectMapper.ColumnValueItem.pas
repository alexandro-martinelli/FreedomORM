unit AM.Freedom.ObjectMapper.ColumnValueItem;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  AM.Freedom.JSONFreedomObject,
  AM.Freedom.EnumerationTypes;

type
  TColumnValueItem = class(TJSONFreedomObject)
  private
    FColumnName: String;
    FColumnValue: Variant;
    FStreamColumnValue: TStream;
    FColumnType: TColumnType;
  public
    destructor Destroy; override;
    property ColumnName: String read FColumnName write FColumnName;
    property ColumnValue: Variant read FColumnValue write FColumnValue;
    property StreamColumnValue: TStream read FStreamColumnValue write FStreamColumnValue;
    property ColumnType: TColumnType read FColumnType write FColumnType;
  end;

  TColumnValueList = class(TObjectList<TColumnValueItem>)
  public
    function FindColumnValue(pColumnName: String; pColumnType: TColumnType = ctyUnknow): TColumnValueItem;
    function AddValue(pColumnName: String; pColumnValue: Variant; pColumnType: TColumnType; pStreamColumnValue: TStream = nil): TColumnValueItem;
  end;



implementation

uses
  System.StrUtils;

{ TColumnValueList }

function TColumnValueList.AddValue(pColumnName: String; pColumnValue: Variant; pColumnType: TColumnType; pStreamColumnValue: TStream): TColumnValueItem;
begin
  Result := FindColumnValue(pColumnName, pColumnType);
  if (not Assigned(Result)) then
  begin
    Result := TColumnValueItem.Create;
    Result.ColumnName := pColumnName;
    Add(Result);
  end;
  Result.ColumnValue := pColumnValue;
  Result.StreamColumnValue := pStreamColumnValue;
  Result.ColumnType := pColumnType;
end;

function TColumnValueList.FindColumnValue(pColumnName: String; pColumnType: TColumnType): TColumnValueItem;
var
  lItem: TColumnValueItem;
begin
  Result := nil;
  for lItem in Self do
  begin
    if SameText(lItem.ColumnName, pColumnName) and (pColumnType in [lItem.ColumnType, ctyUnknow]) then
    begin
      Result := lItem;
      Break;
    end;
  end;
end;

{ TColumnValueItem }

destructor TColumnValueItem.Destroy;
begin
  FreeAndNil(FStreamColumnValue);
  inherited;
end;

end.