unit AM.Freedom.frmBaseObjectUnit;

interface

uses
  {$IFNDEF TEST}
  ToolsAPI,
  {$IFEND}
  System.IniFiles,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs;

type
  TfrmBaseObjectUnit = class(TForm)
    BalloonHint: TBalloonHint;
  private
    FIniFile: TIniFile;
    procedure InitializeIniFile;
  protected
    property IniFile: TIniFIle read FIniFile;
    procedure ShowBalloHintForControl(pControl: TControl; pDescription: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

constructor TfrmBaseObjectUnit.Create(AOwner: TComponent);
begin
  inherited;
  InitializeIniFile;
end;

destructor TfrmBaseObjectUnit.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

procedure TfrmBaseObjectUnit.InitializeIniFile;
{$IFNDEF TEST}
var
  lCurrentProject: IOTAProject;
{$IFEND}
begin
  {$IFDEF TEST}
  FIniFile := TIniFile.Create('C:\Alexandro\FreedomORM.cfg');
  {$ELSE}
  lCurrentProject := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
  FIniFile := TIniFile.Create(ExtractFilePath(lCurrentProject.FileName) + 'FreedomORM.cfg');
  {$IFEND}
end;


procedure TfrmBaseObjectUnit.ShowBalloHintForControl(pControl: TControl; pDescription: String);
begin
  if Assigned(pControl) then
  begin
    BalloonHint.Title := 'Error';
    BalloonHint.Description := pDescription;
    BalloonHint.ShowHint(pControl);
    TWinControl(pControl).SetFocus;
  end;
end;

end.
