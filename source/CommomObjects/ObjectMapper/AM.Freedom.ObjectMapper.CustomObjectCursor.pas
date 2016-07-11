unit AM.Freedom.ObjectMapper.CustomObjectCursor;

interface

uses
  System.SysUtils,
  AM.Freedom.ICursor,
  AM.Freedom.IPersistent,
  AM.Freedom.GroupCriteria;

type
  TObjectCursorClass = class of TCustomObjectCursor;

  TCustomObjectCursor = class abstract
  private
    FGroupCriteria: TGroupCriteria;
  public
    function ExtractCursor(pPersistent: IPersistent): ICursor; virtual;
    function ExtractCursorList(pPersistent: IPersistent): TCursorList; virtual;
    property GroupCriteria: TGroupCriteria read FGroupCriteria write FGroupCriteria;
  end;

implementation

uses
  AM.Freedom.Exceptions;

{ TCustomObjectCursor }

function TCustomObjectCursor.ExtractCursor(pPersistent: IPersistent): ICursor;
begin
  raise EInvalidMethodCallOnClass.Create('ExtractCursor', ClassName);
end;

function TCustomObjectCursor.ExtractCursorList(pPersistent: IPersistent): TCursorList;
begin
  raise EInvalidMethodCallOnClass.Create('ExtractCursorList', ClassName);
end;

end.
