unit AM.Freedom.IndexColumn;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  AM.Freedom.SQLMappers.NamedObject,
  AM.Freedom.EnumerationTypes;

type
  TIndexColumn = class(TNamedObject)
  private
    FNullOption: TNullOption;
    FSortType: TSortType;
  public
    constructor Create(pName: String; pSortType: TSortType = Asc; pNullOption: TNullOption = noNone);
    property Name;
    property SortType: TSortType read FSortType write FSortType default Asc;
    property NullOption: TNullOption read FNullOption write FNullOption default noNone;
  end;

  TIndexColumns = class(TObjectList<TIndexColumn>)
  public
    function FindColumn(pColumnName: String): TIndexColumn;
    function ColumnExists(pColumnName: String): Boolean;
    function AddColumn(pColumnName: String; pSortType: TSortType = Asc; pNullOption: TNullOption = noNone): TIndexColumn;
  end;


implementation

uses
  System.StrUtils;

{ TIndexColumn }

constructor TIndexColumn.Create(pName: String; pSortType: TSortType; pNullOption: TNullOption);
begin
  Name := pName;
  FSortType := pSortType;
  FNullOption := pNullOption;
end;

{ TIndexColumns }

function TIndexColumns.AddColumn(pColumnName: String; pSortType: TSortType; pNullOption: TNullOption): TIndexColumn;
begin
  Result := TIndexColumn.Create(pColumnName, pSortType, pNullOption);
  Add(Result);
end;

function TIndexColumns.ColumnExists(pColumnName: String): Boolean;
begin
  Result := FindColumn(pColumnName) <> nil;
end;

function TIndexColumns.FindColumn(pColumnName: String): TIndexColumn;
var
  lColumn: TIndexColumn;
begin
  Result := nil;
  for lColumn in Self do
  begin
    if SameText(lColumn.Name, pColumnName) then
    begin
      Result := lColumn;
      Break;
    end;
  end;
end;

end.