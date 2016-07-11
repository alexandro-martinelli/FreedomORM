unit AM.Freedom.ColumnTypeDBConverter;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.Exceptions,
  AM.Freedom.CustomDBPersistent;

type
  TColumnTypeDBConverter = class abstract
  private
    FColumnType: TColumnType;
    FFieldCommandClass: TFieldCommandClass;
    FDBPersistentClass: TDBPersistentClass;
  public
    property ColumnType: TColumnType read FColumnType write FColumnType;
    property FieldCommandClass: TFieldCommandClass read FFieldCommandClass write FFieldCommandClass;
    property DBPersistentClass: TDBPersistentClass read FDBPersistentClass write FDBPersistentClass;
    function IsEquals(pColumnType: TColumnType; pFieldCommandClass: TFieldCommandClass; pDBPersistentClass: TDBPersistentClass): Boolean;
  end;

  TRegisterColumnTypeDBConverter = class sealed
  strict private
    class var FRegisterList: TObjectList<TColumnTypeDBConverter>;
  private
    class procedure CreateList;
    class procedure DestroyList;
  public
    class procedure RegisterColumnTypeDBConverter(pColumnType: TColumnType; pFieldCommandClass: TFieldCommandClass; pDBPersistentClass: TDBPersistentClass);
    class function FindColumnTypeDBConverter(pColumnType: TColumnType; pFieldCommandClass: TFieldCommandClass; pDBPersistentClass: TDBPersistentClass): TColumnTypeDBConverter;
    class procedure UnregisterColumnTypeDBConverter(pColumnType: TColumnType; pFieldCommandClass: TFieldCommandClass; pDBPersistentClass: TDBPersistentClass);
    class function ColumnTypeToFieldCommandClass(pColumnType: TColumnType; pDBPersistentClass: TDBPersistentClass): TFieldCommandClass;
  end;


implementation


{ TCustomColumnTypeDBConverter }

function TColumnTypeDBConverter.IsEquals(pColumnType: TColumnType;
  pFieldCommandClass: TFieldCommandClass; pDBPersistentClass: TDBPersistentClass): Boolean;
begin
  Result := (pColumnType = FColumnType) and (pFieldCommandClass = FFieldCommandClass) and (pDBPersistentClass = FDBPersistentClass);
end;

{ TRegisterColumnTypeDBConverter }

class function TRegisterColumnTypeDBConverter.ColumnTypeToFieldCommandClass(pColumnType: TColumnType; pDBPersistentClass: TDBPersistentClass): TFieldCommandClass;
var
  lConverter: TColumnTypeDBConverter;
begin
  Result := nil;
  for lConverter in FRegisterList do
  begin
    if (lConverter.ColumnType = pColumnType) and (lConverter.DBPersistentClass = pDBPersistentClass) then
    begin
      Result := lConverter.FieldCommandClass;
      Break;
    end;
  end;
  if (not Assigned(Result)) and (pDBPersistentClass <> nil) then
  begin
    for lConverter in FRegisterList do
    begin
      if (lConverter.ColumnType = pColumnType) and (lConverter.DBPersistentClass = nil) then
      begin
        Result := lConverter.FieldCommandClass;
        Break;
      end;
    end;
  end;
end;

class procedure TRegisterColumnTypeDBConverter.CreateList;
begin
  FRegisterList := TObjectList<TColumnTypeDBConverter>.Create;
end;

class procedure TRegisterColumnTypeDBConverter.DestroyList;
begin
  FRegisterList.Free;
end;

class function TRegisterColumnTypeDBConverter.FindColumnTypeDBConverter(pColumnType: TColumnType;
  pFieldCommandClass: TFieldCommandClass; pDBPersistentClass: TDBPersistentClass): TColumnTypeDBConverter;
var
  lConverter: TColumnTypeDBConverter;
begin
  Result := nil;
  for lConverter in FRegisterList do
  begin
    if (lConverter.IsEquals(pColumnType, pFieldCommandClass, pDBPersistentClass)) then
    begin
      Result := lConverter;
      Break;
    end;
  end;
end;

class procedure TRegisterColumnTypeDBConverter.RegisterColumnTypeDBConverter(pColumnType: TColumnType;
  pFieldCommandClass: TFieldCommandClass; pDBPersistentClass: TDBPersistentClass);
var
  lConverter: TColumnTypeDBConverter;
begin
  lConverter := FindColumnTypeDBConverter(pColumnType, pFieldCommandClass, pDBPersistentClass);
  if (not Assigned(lConverter)) then
  begin
    lConverter := TColumnTypeDBConverter.Create;
    lConverter.ColumnType := pColumnType;
    lConverter.FieldCommandClass := pFieldCommandClass;
    lConverter.DBPersistentClass := pDBPersistentClass;
    FRegisterList.Add(lConverter);
  end;
end;

class procedure TRegisterColumnTypeDBConverter.UnregisterColumnTypeDBConverter(pColumnType: TColumnType;
  pFieldCommandClass: TFieldCommandClass; pDBPersistentClass: TDBPersistentClass);
var
  lConverter: TColumnTypeDBConverter;
begin
  lConverter := FindColumnTypeDBConverter(pColumnType, pFieldCommandClass, pDBPersistentClass);
  if (Assigned(lConverter)) then
  begin
    FRegisterList.Remove(lConverter);
  end;
end;

initialization
  TRegisterColumnTypeDBConverter.CreateList;

finalization
  TRegisterColumnTypeDBConverter.DestroyList;

end.