unit AM.Freedom.CustomMenuItem;

interface

uses
  Vcl.Menus,
  AM.Freedom.Exceptions,
  System.Classes;

type
  TCustomMenuItem = class
  strict private
    FMenuItem: TMenuItem;
  strict protected
    function GetCaption: String; virtual;
    procedure DoMenuClick(Sender: TObject); virtual;
    function GetShortCut: TShortCut; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property MenuItem: TMenuItem read FMenuItem;
  end;

implementation

{ TCustomMenuItem }

constructor TCustomMenuItem.Create;
begin
  FMenuItem := TMenuItem.Create(nil);
  FMenuItem.Caption := GetCaption;
  FMenuItem.OnClick := DoMenuClick;
  FMenuItem.ShortCut := GetShortCut;
end;

destructor TCustomMenuItem.Destroy;
begin
  FMenuItem.Free;
  inherited;
end;

procedure TCustomMenuItem.DoMenuClick(Sender: TObject);
begin
  raise EInvalidMethodCallOnClass.Create('DoMenuClick', ClassName);
end;

function TCustomMenuItem.GetCaption: String;
begin
  raise EInvalidMethodCallOnClass.Create('GetCaption', ClassName);
end;

function TCustomMenuItem.GetShortCut: TShortCut;
begin
  Result := 0;
end;

end.
