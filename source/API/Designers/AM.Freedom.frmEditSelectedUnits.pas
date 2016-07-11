unit AM.Freedom.frmEditSelectedUnits;

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
  Vcl.StdCtrls,
  Vcl.ImgList,
  System.DateUtils,
  System.IOUtils,
  System.Generics.Defaults;

type
  TfrmEditSelectedUnits = class(TfrmBase)
    lbxUnits: TListBox;
    btnOK: TButton;
    btnCancel: TButton;
    lblHint: TLabel;
    procedure lbxUnitsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    class procedure EditSelectedUnits(pSelectedUnits: TStrings);
  end;

implementation

{$R *.dfm}

class procedure TfrmEditSelectedUnits.EditSelectedUnits(pSelectedUnits: TStrings);
var
  lfrmEditSelectedUnits: TfrmEditSelectedUnits;
begin
  lfrmEditSelectedUnits := TfrmEditSelectedUnits.Create(nil);
  try
    lfrmEditSelectedUnits.lbxUnits.Items.Assign(pSelectedUnits);
    if (lfrmEditSelectedUnits.ShowModal = mrOk) then
    begin
      pSelectedUnits.Assign(lfrmEditSelectedUnits.lbxUnits.Items);
    end;
  finally
    lfrmEditSelectedUnits.Free;
  end;
end;

procedure TfrmEditSelectedUnits.lbxUnitsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  lIndex: Integer;
begin
  inherited;
  if (Key = VK_DELETE) then
  begin
    for lIndex := lbxUnits.Count - 1 downto 0 do
    begin
      if (lbxUnits.Selected[lIndex]) then
      begin
        lbxUnits.Items.Delete(lIndex);
      end;
    end;
  end;
end;

end.
