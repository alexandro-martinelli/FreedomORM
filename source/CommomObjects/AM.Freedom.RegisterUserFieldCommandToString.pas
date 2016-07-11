unit AM.Freedom.RegisterUserFieldCommandToString;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  AM.Freedom.CustomDBPersistent,
  AM.Freedom.SQLCommands.CustomFieldCommand;

type
  TUserFieldCommandToString = class sealed
  private
    FFieldCommandClass: TFieldCommandClass;
    FAsString: String;
    FDBPersistentClass: TDBPersistentClass;
  public
    property FieldCommandClass: TFieldCommandClass read FFieldCommandClass write FFieldCommandClass;
    property AsString: String read FAsString write FAsString;
    property DBPersistentClass: TDBPersistentClass read FDBPersistentClass write FDBPersistentClass;
    function IsEquals(pFieldCommandClass: TFieldCommandClass; pAsString: String; pDBPersistentClass: TDBPersistentClass): Boolean; overload;
    function IsEquals(pFieldCommandClass: TFieldCommandClass; pAsString: String): Boolean; overload;
  end;

  TRegisterUserFieldCommandToString = class sealed
  strict private
    class var FRegisterList: TObjectList<TUserFieldCommandToString>;
  private
    class procedure CreateList;
    class procedure DestroyList;
  public
    class procedure RegisterCommandClassToString(pFieldCommandClass: TFieldCommandClass; pAsString: String;
        pDBPersistentClass: TDBPersistentClass);
    class procedure UnregisterCommandClassToString(pFieldCommandClass: TFieldCommandClass; pAsString: String;
        pDBPersistentClass: TDBPersistentClass);

    class function FindCommandClassToString(pFieldCommandClass: TFieldCommandClass; pAsString: String;
        pDBPersistentClass: TDBPersistentClass): TUserFieldCommandToString;
    class function ListCommandClassToString(pFieldCommandClass: TFieldCommandClass; pAsString: String): TList<TUserFieldCommandToString>;
    class function FieldCommandClassToString(pFieldCommandClass: TFieldCommandClass; pDBPersistentClass: TDBPersistentClass): String;

  end;


implementation

{ TFieldCommandToStringRegister }

class procedure TRegisterUserFieldCommandToString.CreateList;
begin
  FRegisterList := TObjectList<TUserFieldCommandToString>.Create;
end;

class procedure TRegisterUserFieldCommandToString.DestroyList;
begin
  FRegisterList.Free
end;

class function TRegisterUserFieldCommandToString.FieldCommandClassToString(pFieldCommandClass: TFieldCommandClass;
  pDBPersistentClass: TDBPersistentClass): String;
var
  lFieldCommand: TUserFieldCommandToString;
begin
  Result := '';
  for lFieldCommand in FRegisterList do
  begin
    if (lFieldCommand.FieldCommandClass = pFieldCommandClass) and (lFieldCommand.DBPersistentClass = pDBPersistentClass) then
    begin
      Result := lFieldCommand.AsString;
      Break;
    end;
  end;
  if (Result = '') and (pDBPersistentClass <> nil) then
  begin
    for lFieldCommand in FRegisterList do
    begin
      if (lFieldCommand.FieldCommandClass = pFieldCommandClass) and  (lFieldCommand.DBPersistentClass = nil) then
      begin
        Result := lFieldCommand.AsString;
        Break;
      end;
    end;
  end;
end;

class function TRegisterUserFieldCommandToString.FindCommandClassToString(pFieldCommandClass: TFieldCommandClass;
  pAsString: String; pDBPersistentClass: TDBPersistentClass): TUserFieldCommandToString;
var
  lFieldCommand: TUserFieldCommandToString;
begin
  Result := nil;
  for lFieldCommand in FRegisterList do
  begin
    if (lFieldCommand.IsEquals(pFieldCommandClass, pAsString, pDBPersistentClass)) then
    begin
      Result := lFieldCommand;
      Break;
    end;
  end;
end;

class function TRegisterUserFieldCommandToString.ListCommandClassToString(pFieldCommandClass: TFieldCommandClass;
  pAsString: String): TList<TUserFieldCommandToString>;
var
  lFieldCommand: TUserFieldCommandToString;
begin
  Result := TList<TUserFieldCommandToString>.Create;
  for lFieldCommand in FRegisterList do
  begin
    if (lFieldCommand.IsEquals(pFieldCommandClass, pAsString)) then
    begin
      Result.Add(lFieldCommand);
    end;
  end;
end;

class procedure TRegisterUserFieldCommandToString.RegisterCommandClassToString(pFieldCommandClass: TFieldCommandClass;
  pAsString: String; pDBPersistentClass: TDBPersistentClass);
var
  lFieldCommand: TUserFieldCommandToString;
  lFieldFound: Boolean;
begin
  lFieldFound := False;
  for lFieldCommand in FRegisterList do
  begin
    lFieldFound := lFieldCommand.IsEquals(pFieldCommandClass, pAsString, pDBPersistentClass);
    if lFieldFound then
    begin
      Break;
    end;
  end;
  if (not lFieldFound) then
  begin
    lFieldCommand := TUserFieldCommandToString.Create;
    lFieldCommand.FieldCommandClass := pFieldCommandClass;
    lFieldCommand.AsString := pAsString;
    lFieldCommand.DBPersistentClass := pDBPersistentClass;
    FRegisterList.Add(lFieldCommand);
  end;
end;

class procedure TRegisterUserFieldCommandToString.UnregisterCommandClassToString(pFieldCommandClass: TFieldCommandClass;
  pAsString: String; pDBPersistentClass: TDBPersistentClass);
var
  lIndex: Integer;
begin
  for lIndex := FRegisterList.Count - 1 downto 0 do
  begin
    if FRegisterList.Items[lIndex].IsEquals(pFieldCommandClass, pAsString, pDBPersistentClass) then
    begin
      FRegisterList.Delete(lIndex);
      Break;
    end;
  end;
end;

{ TFieldCommandToString }

function TUserFieldCommandToString.IsEquals(pFieldCommandClass: TFieldCommandClass; pAsString: String;
    pDBPersistentClass: TDBPersistentClass): Boolean;
begin
  Result := (FFieldCommandClass = pFieldCommandClass) and (pAsString = FAsString) and
      (FDBPersistentClass = pDBPersistentClass);
end;

function TUserFieldCommandToString.IsEquals(pFieldCommandClass: TFieldCommandClass; pAsString: String): Boolean;
begin
  Result := (FFieldCommandClass = pFieldCommandClass) and (pAsString = FAsString);
end;

initialization
   TRegisterUserFieldCommandToString.CreateList;

finalization
  TRegisterUserFieldCommandToString.DestroyList;

end.