unit AM.Freedom.RenameParams;

interface

uses
  System.SysUtils;

type
  TRenameParams = class sealed
  private
    FName: String;
    FRenameClassName: String;
    FCaption: String;
    FHasCaption: Boolean;
    FPrefix: String;
  public
    constructor Create;
    property RenameClassName: String read FRenameClassName write FRenameClassName;
    property Name: String read FName write FName;
    property Prefix: String read FPrefix write FPrefix;
    property HasCaption: Boolean read FHasCaption write FHasCaption default False;
    property Caption: String read FCaption write FCaption;
  end;

implementation

{ TRenameParams }

constructor TRenameParams.Create;
begin
  FHasCaption := False;
end;

end.