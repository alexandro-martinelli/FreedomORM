unit AM.Freedom.frmBaseCriteria;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  AM.Freedom.frmBase,
  Vcl.ImgList,
  Vcl.ExtCtrls,
  AM.Freedom.Exceptions;

type
  TfrmBaseCriteria = class(TfrmBase)
    pnlName: TPanel;
  protected
    function GetAlignToParent: TAlign; virtual;
    function GetDescription: String; virtual;
  public
    procedure JumpTo(pControl: TWinControl);
    procedure Validate; virtual;
    destructor Destroy; override;
    property Description: String read GetDescription;
    procedure AdjustHeigth;
  end;

implementation

uses
  Vcl.StdCtrls;

{$R *.dfm}
{ TfrmBaseCriteria }

procedure TfrmBaseCriteria.AdjustHeigth;
var
  lIndex: Integer;
  lMaxBottom: Integer;
begin
  inherited;
  lMaxBottom := 0;
  for lIndex := 0 to ComponentCount - 1 do
  begin
    if Components[lIndex].InheritsFrom(TControl) then
    begin
      if TControl(Components[lIndex]).Top + TControl(Components[lIndex]).Height > lMaxBottom then
      begin
        lMaxBottom := TControl(Components[lIndex]).Top + TControl(Components[lIndex]).Height;
      end;
    end;
  end;
  Height := lMaxBottom + 2;
end;

destructor TfrmBaseCriteria.Destroy;
begin
  ReloadIniFile;
  FinalizeHistory;
  inherited;
end;

function TfrmBaseCriteria.GetAlignToParent: TAlign;
begin
  Result := TAlign.alTop;
end;

function TfrmBaseCriteria.GetDescription: String;
begin
  Result := '';
end;

procedure TfrmBaseCriteria.JumpTo(pControl: TWinControl);
begin
  Parent := pControl;
  BorderStyle := bsNone;
  Align := GetAlignToParent;
  Visible := True;
end;

procedure TfrmBaseCriteria.Validate;
begin
  raise EInvalidMethodCallOnClass.Create('Validate', ClassName);
end;

end.
