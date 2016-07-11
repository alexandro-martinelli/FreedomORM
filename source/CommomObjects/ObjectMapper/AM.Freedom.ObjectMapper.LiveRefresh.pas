unit AM.Freedom.ObjectMapper.LiveRefresh;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.EnumerationTypes;

type
  TLiveRefresh = class
  private
    FLazyObject: TObject;
    FColumn: String;
    FColumnType: TColumnType;
    FRefColumnName: String;
  public
    property LazyObject: TObject read FLazyObject write FLazyObject;
    property Column: String read FColumn write FColumn;
    property ColumnType: TColumnType read FColumnType write FColumnType;
    property RefColumnName: String read FRefColumnName write FRefColumnName;
  end;

  TLiveRefreshs = class(TObjectList<TLiveRefresh>)
  public
    procedure AddLiveRefresh(pLazyObject: TObject; pColumn: String; pColumnType: TColumnType; pRefColumnName: String);
    function FindLiveRefresh(pLazyObject: TObject): TLiveRefresh;
    procedure FreeLazyObject(pLazyObject: TObject);
  end;

implementation

{ TLiveRefreshs }

uses AM.Freedom.ILazy;

procedure TLiveRefreshs.AddLiveRefresh(pLazyObject: TObject; pColumn: String; pColumnType: TColumnType; pRefColumnName: String);
var
  lLiveRefresh: TLiveRefresh;
begin
  lLiveRefresh := TLiveRefresh.Create;
  lLiveRefresh.LazyObject := pLazyObject;
  lLiveRefresh.Column := pColumn;
  lLiveRefresh.ColumnType := pColumnType;
  lLiveRefresh.RefColumnName := pRefColumnName;
  Add(lLiveRefresh);
end;

function TLiveRefreshs.FindLiveRefresh(pLazyObject: TObject): TLiveRefresh;
var
  lLiveRefresh: TLiveRefresh;
begin
  Result := nil;
  for lLiveRefresh in Self do
  begin
    if lLiveRefresh.LazyObject = pLazyObject then
    begin
      Result := lLiveRefresh;
      Break;
    end;
  end;
end;

procedure TLiveRefreshs.FreeLazyObject(pLazyObject: TObject);
var
  lLiveRefresh: TLiveRefresh;
begin
  lLiveRefresh := FindLiveRefresh(pLazyObject);
  while Assigned(lLiveRefresh) do
  begin
    Extract(lLiveRefresh);
    lLiveRefresh.Free;
    lLiveRefresh := FindLiveRefresh(pLazyObject);
  end;
end;

end.

