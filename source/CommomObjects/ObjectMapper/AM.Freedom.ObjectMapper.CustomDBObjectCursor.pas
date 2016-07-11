unit AM.Freedom.ObjectMapper.CustomDBObjectCursor;

interface

uses
  System.SysUtils,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.ObjectMapper.CustomObjectCursor,
  AM.Freedom.IPersistent,
  AM.Freedom.Exceptions,
  AM.Freedom.IDBPersistent,
  AM.Freedom.DBPersistent.IDBConnector,
  AM.Freedom.ICursor;

type
  TCustomDBObjectCursor = class abstract(TCustomObjectCursor)
  strict private
    FSingleCursor: Boolean;
    function ExtractStatement(pPersistent: IPersistent): IDBStatement;
  strict protected
    function GetSQLCommand: TCustomCommand; virtual;
    property SingleCursor: Boolean read FSingleCursor;
  public
    function ExtractCursor(pPersistent: IPersistent): ICursor; override;
    function ExtractCursorList(pPersistent: IPersistent): TCursorList; override;
  end;


implementation

{ TCustomDBObjectCursor }

function TCustomDBObjectCursor.ExtractCursor(pPersistent: IPersistent): ICursor;
var
  lStatement: IDBStatement;
begin
  Result := nil;
  FSingleCursor := True;
  lStatement := ExtractStatement(pPersistent);
  if (Assigned(lStatement)) then
  begin
    Result := lStatement.ExecuteQuery
  end;
end;

function TCustomDBObjectCursor.ExtractCursorList(pPersistent: IPersistent): TCursorList;
var
  lStatement: IDBStatement;
begin
  Result := nil;
  FSingleCursor := False;
  lStatement := ExtractStatement(pPersistent);
  try
    if (Assigned(lStatement)) then
    begin
      Result := lStatement.ExtractQueryList;
    end;
  finally
    lStatement := nil;
  end;
end;

function TCustomDBObjectCursor.ExtractStatement(pPersistent: IPersistent): IDBStatement;
var
  lDBPersistent: IDBPersistent;
  lCommand: TCustomCommand;
begin
  Result := nil;
  if Supports(pPersistent, IDBPersistent, lDBpersistent) then
  begin
    Result := lDBpersistent.NewDBStatement;
    lCommand := GetSQLCommand;
    try
      Result.SetCommand(lCommand, lDBPersistent.GetSQLMapper);
    finally
      lCommand.Free;
    end;
  end;
end;

function TCustomDBObjectCursor.GetSQLCommand: TCustomCommand;
begin
  raise EInvalidMethodCallOnClass.Create('GetSQLCommand', ClassName);
end;

end.