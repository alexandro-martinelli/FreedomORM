unit AM.Freedom.FilteredItem;

interface

uses
  System.Generics.Collections,
  System.SysUtils;

type
  TFilteredItem<T: class, constructor> = class
  private
    FIndex: Integer;
    FCurrentObject: T;
  public
    constructor Create(pIndex: Integer; pCurrentObject: T);
    property Index: Integer read FIndex write FIndex;
    property CurrentObject: T read FCurrentObject write FCurrentObject;
  end;

  TFilteredList<T: class, constructor> = class(TObjectList<TFilteredItem<T>>)
  public
    procedure AddItem(pIndex: Integer; pCurrentObject: T);
  end;


implementation

{ TFilteredItem<T> }

constructor TFilteredItem<T>.Create(pIndex: Integer; pCurrentObject: T);
begin
  FIndex := pIndex;
  FCurrentObject := pCurrentObject;
end;

{ TFilteredList<T> }

procedure TFilteredList<T>.AddItem(pIndex: Integer; pCurrentObject: T);
var
  lFilteredItem: TFilteredItem<T>;
begin
  lFilteredItem := TFilteredItem<T>.Create(pIndex, pCurrentObject);
  Add(lFilteredItem);
end;

end.