unit AM.Freedom.Persistent.SetCursorResult;

interface

uses
  AM.Freedom.ObjectMapper;

type
  TSetCursorResult = class sealed
  private
    FResult: Boolean;
    FObjectMapper: TObjectMapper;
  public
    property Result: Boolean read FResult write FResult;
    property ObjectMapper: TObjectMapper read FObjectMapper write FObjectMapper;
  end;

implementation

end.
