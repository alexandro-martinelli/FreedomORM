unit AM.Freedom.Persistent.DataSnapPersistent;

interface

uses
  AM.Freedom.CustomPersistent,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria,
  AM.Freedom.Persistent.SetCursorResult,
  AM.Freedom.Persistent.Cursor,
  AM.Freedom.ICursor,
  AM.Freedom.ObjectMapper, AM.Freedom.CustomDBPersistent;

type
  TOnSetCursor = function(pObject: TObject; pObjectState: TObjectState): TSetCursorResult of object;
  TOnGetCursor = function(pClass: TClass; pGroupCriteria: TGroupCriteria): ICursor of object;

  TDataSnapPersistent = class(TCustomPersistent)
  strict private
    FOnGetCursor: TOnGetCursor;
    FOnSetCursor: TOnSetCursor;
  strict protected
    function SetCursor(pObject: TObject; pObjectState: TObjectState): TSetCursorResult; override;
    function GetCursor(pClass: TClass; pGroupCriteria: TGroupCriteria; pMapper: TObjectMapper): ICursor; override;
    procedure DropPersistent(pClass: TClass); override;
    procedure UpdatePersistent(pClass: TClass; pUpdateObjectOptions: TUpdateObjectOptions); override;
  public
    property OnSetCursor: TOnSetCursor read FOnSetCursor write FOnSetCursor;
    property OnGetCursor: TOnGetCursor read FOnGetCursor write FOnGetCursor;
  end;

implementation

{ TDataSnapPersistent }

uses AM.Freedom.Exceptions;

procedure TDataSnapPersistent.DropPersistent(pClass: TClass);
begin
  raise EInvalidMethodCallOnClass.Create('DropPersistent', ClassName);
end;

function TDataSnapPersistent.GetCursor(pClass: TClass; pGroupCriteria: TGroupCriteria; pMapper: TObjectMapper): ICursor;
begin
  Result := nil;
  if Assigned(FOnGetCursor) then
  begin
    Result := FOnGetCursor(pClass, pGroupCriteria);
  end;
end;

function TDataSnapPersistent.SetCursor(pObject: TObject; pObjectState: TObjectState): TSetCursorResult;
begin
  Result := nil;
  if Assigned(FOnSetCursor) then
  begin
    Result := FOnSetCursor(pObject, pObjectState);
  end;
end;

procedure TDataSnapPersistent.UpdatePersistent(pClass: TClass; pUpdateObjectOptions: TUpdateObjectOptions);
begin
  raise EInvalidMethodCallOnClass.Create('UpdatePersistent', ClassName);
end;

end.
