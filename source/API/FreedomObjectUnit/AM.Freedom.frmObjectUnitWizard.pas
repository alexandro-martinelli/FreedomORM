unit AM.Freedom.frmObjectUnitWizard;

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
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls;

type
  TfrmObjectUnitWizard = class(TForm)
    lblText: TLabel;
    imgLeft: TImage;
    PageControl1: TPageControl;
    tsEditor: TTabSheet;
    btnOptions: TButton;
    btnNewObjectUnit: TButton;
    btnNewDBObjectUnit: TButton;
  end;

implementation

{$R *.dfm}

end.
