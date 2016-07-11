unit AM.Freedom.ColumnMappersList;

interface

uses
  System.Generics.Collections,
  AM.Freedom.Attributes,
  AM.Freedom.ObjectMapper.CustomColumnReader,
  AM.Freedom.FreedomAttributes;

type
  TColumnMappers = class sealed
  private
    FColumnClass: TAttributeClass;
    FColumnReaderClass: TColumnReaderClass;
  public
    constructor Create(const pColumnClass: TAttributeClass; const pColumnReaderClass: TColumnReaderClass);
    property ColumnsClass: TAttributeClass read FColumnClass write FColumnClass;
    property ColumnReaderClass: TColumnReaderClass read FColumnReaderClass write FColumnReaderClass;

  end;

  TColumnMappersList = class sealed(TObjectList<TColumnMappers>)
  public
    function FindColumnReaderClass(pColumnClass: TAttributeClass): TColumnReaderClass;
  end;

implementation

{ TColumnMappers }

constructor TColumnMappers.Create(const pColumnClass: TAttributeClass; const pColumnReaderClass: TColumnReaderClass);
begin
  FColumnClass := pColumnClass;
  FColumnReaderClass := pColumnReaderClass;
end;

{ TColumnMappersList }

function TColumnMappersList.FindColumnReaderClass(pColumnClass: TAttributeClass): TColumnReaderClass;
var
  lItem: TColumnMappers;
begin
  Result := nil;
  for lItem in Self do
  begin
    if (lItem.ColumnsClass = pColumnClass) then
    begin
      Result := lItem.ColumnReaderClass;
    end;
  end;
end;

end.
