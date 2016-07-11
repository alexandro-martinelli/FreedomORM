unit AM.Freedom.ColumnReaderRegister;

interface

uses
  AM.Freedom.ColumnMappersList,
  AM.Freedom.Attributes,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.ObjectMapper.CustomColumnReader,
  AM.Freedom.FreedomAttributes;

type
  TColumnReaderRegister = class sealed
  strict private
    class var FMappersList: TColumnMappersList;
  private
    class procedure CreateList;
    class procedure DestroyList;
  public
    class function FindColumnReader(const pColumnClass: TAttributeClass): TColumnReaderClass;
    class procedure RegisterColumnReader(const pColumnClass: TAttributeClass; const pColumnReaderClass: TColumnReaderClass);
  end;

implementation

uses
  System.SysUtils, AM.Freedom.ObjectMapper.ColumnReaders;

{ TColumnMapperRegister }

class procedure TColumnReaderRegister.CreateList;
begin
  FMappersList := TColumnMappersList.Create;
end;

class procedure TColumnReaderRegister.DestroyList;
begin
  FreeAndNil(FMappersList);
end;

class function TColumnReaderRegister.FindColumnReader(const pColumnClass: TAttributeClass): TColumnReaderClass;
var
  lClass: TClass;
begin
  Result := FMappersList.FindColumnReaderClass(pColumnClass);
  if not Assigned(Result) then
  begin
    lClass := pColumnClass.ClassParent;
    while (lClass <> TCustomAttribute) and (Result = nil) do
    begin
      Result := FMappersList.FindColumnReaderClass(ColumnClass(lClass));
      lClass := lClass.ClassParent;
    end;
  end;
end;

class procedure TColumnReaderRegister.RegisterColumnReader(const pColumnClass: TAttributeClass; const pColumnReaderClass: TColumnReaderClass);
begin
  if FMappersList.FindColumnReaderClass(pColumnClass) = nil then
  begin
    FMappersList.Add(TColumnMappers.Create(pColumnClass, pColumnReaderClass))
  end;
end;

initialization
  TColumnReaderRegister.CreateList;
  TColumnReaderRegister.RegisterColumnReader(CustomColumn, TCustomColumnReader);
  TColumnReaderRegister.RegisterColumnReader(CustomNamedColumn, TCustomNamedColumnReader);
  TColumnReaderRegister.RegisterColumnReader(Column, TColumnReader);
  TColumnReaderRegister.RegisterColumnReader(BlobColumn, TBlobColumnReader);

  TColumnReaderRegister.RegisterColumnReader(BooleanColumn, TBooleanColumnReader);
  TColumnReaderRegister.RegisterColumnReader(EnumerationColumn, TEnumerationColumnReader);
  TColumnReaderRegister.RegisterColumnReader(JoinedColumn, TJoinedColumnReader);
  TColumnReaderRegister.RegisterColumnReader(DetailColumn, TDetailColumnReader);
  TColumnReaderRegister.RegisterColumnReader(AutoMapping, TAutoColumnReader);
  TColumnReaderRegister.RegisterColumnReader(Extension, TExtensionColumnReader);

finalization
  TColumnReaderRegister.DestroyList;

end.
