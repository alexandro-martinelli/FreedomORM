unit AM.Freedom.frmNewCriteria;

interface

uses
  System.Generics.Collections,
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
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.ImgList,
  AM.Freedom.GroupCriteriaDescriptor,
  AM.Freedom.frmBaseCriteria,
  AM.Freedom.frmNewArgument;

type
  TfrmNewCriteria = class(TfrmBase)
    flpnlButtons: TFlowPanel;
    btnCancel: TButton;
    btnFinish: TButton;
    btnNext: TButton;
    btnBack: TButton;
    pgcSteps: TPageControl;
    tsCreateType: TTabSheet;
    lblCreateAs: TLabel;
    lblCreateExplain: TLabel;
    bvlSeparator: TBevel;
    cbxCreateAs: TComboBox;
    mmoCreateParameters: TMemo;
    procedure cbxCreateAsChange(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnFinishClick(Sender: TObject);
  strict private
  const
    cCreateType = 0;
    cCreateArgumentCount: array [0 .. 13] of Integer = (2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 3);
    cCreateArgumentCountName: array [0 .. 2] of string = ('pLeftArgument', 'pRigthArgument', 'pAndArgument');
  private
    FListArguments: TList<TfrmNewArgument>;
    FCriteriaDescriptor: TCriteriaDescriptor;
    function GetCriteriaDescription(pCreateCriteriaIndex: Byte): string;
    procedure RefreshButtons;
    procedure ValidateCreateType;
    procedure MakeArguments;
    procedure ValidateCurrentArgument;
    function GetNextPageIndex: Integer;
    procedure AdjustTabSheets;
    procedure MakeCriteriaDescriptor;
    function GetCreateAsText: String;
  protected
    function CanAskForClose: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function NewCriteria: TCriteriaDescriptor;
    property CriteriaDescriptor: TCriteriaDescriptor read FCriteriaDescriptor;
  end;

implementation

{$R *.dfm}

uses
  System.StrUtils;

{ TfrmNewCriteria }

procedure TfrmNewCriteria.AdjustTabSheets;
var
  lIndex: Integer;
begin
  for lIndex := 0 to pgcSteps.PageCount - 1 do
  begin
    pgcSteps.Pages[lIndex].TabVisible := False;
  end;
  tsCreateType.Show;
end;

procedure TfrmNewCriteria.btnBackClick(Sender: TObject);
begin
  pgcSteps.ActivePageIndex := pgcSteps.ActivePageIndex - 1;
  RefreshButtons;
end;

procedure TfrmNewCriteria.btnFinishClick(Sender: TObject);
begin
  inherited;
  ValidateCurrentArgument;
  ModalResult := mrOK;
end;

procedure TfrmNewCriteria.btnNextClick(Sender: TObject);
begin
  case pgcSteps.ActivePageIndex of
    cCreateType:
      ValidateCreateType;
  else
    ValidateCurrentArgument;
  end;
  pgcSteps.ActivePageIndex := GetNextPageIndex;
  RefreshButtons;
end;

function TfrmNewCriteria.CanAskForClose: Boolean;
begin
  Result := pgcSteps.ActivePageIndex > 0;
end;

procedure TfrmNewCriteria.cbxCreateAsChange(Sender: TObject);
begin
  inherited;
  lblCreateExplain.Caption := GetCriteriaDescription(cbxCreateAs.ItemIndex);
end;

constructor TfrmNewCriteria.Create(AOwner: TComponent);
begin
  inherited;
  cbxCreateAs.ItemIndex := 0;
  FListArguments := TList<TfrmNewArgument>.Create;
  AdjustTabSheets;
  FCriteriaDescriptor := TCriteriaDescriptor.Create;
end;

destructor TfrmNewCriteria.Destroy;
begin
  FListArguments.Free;
  inherited;
end;

procedure TfrmNewCriteria.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  if (CanClose) then
  begin
    if (ModalResult = mrCancel) then
    begin
      FreeAndNil(FCriteriaDescriptor);
    end
    else
    begin
      MakeCriteriaDescriptor;
    end;
  end;
end;

procedure TfrmNewCriteria.FormShow(Sender: TObject);
begin
  inherited;
  cbxCreateAs.SetFocus;
end;

function TfrmNewCriteria.GetCreateAsText: String;
begin
  Result := cbxCreateAs.Text;
  Result := ReplaceText(Result, ' ', '');
  Result := ReplaceText(Result, ' ', '');
end;

function TfrmNewCriteria.GetCriteriaDescription(pCreateCriteriaIndex: Byte): string;
begin
  case pCreateCriteriaIndex of
    0 .. 6, 9 .. 12:
      Result := 'This create requires two arguments(pLeftArgument(TCustomArgument) and pRigthArgument(TCustomArgument))';
    7, 8:
      Result := 'This create requires one argument(pLeftArgument(TCustomArgument))';
    13:
      Result := 'This create requires tree arguments(pLeftArgument(TCustomArgument) and after pBetweenArgument(TCustomArgument) and pAndArgument(TCustomArgument))';
  end;
end;

function TfrmNewCriteria.GetNextPageIndex: Integer;
begin
  Result := pgcSteps.ActivePageIndex + 1;
end;

procedure TfrmNewCriteria.MakeArguments;
var
  lTabSheet: TTabSheet;
  lIndex: Integer;
  lFrmNewArgument: TfrmNewArgument;
  lActivePageIndex: Integer;
begin
  FListArguments.Clear;
  lActivePageIndex := pgcSteps.ActivePageIndex;
  for lIndex := 0 to cCreateArgumentCount[cbxCreateAs.ItemIndex] - 1 do
  begin
    lTabSheet := TTabSheet.Create(pgcSteps);
    lTabSheet.PageControl := pgcSteps;
    lTabSheet.TabVisible := False;
    lFrmNewArgument := TfrmNewArgument.NewArgument(lTabSheet, cCreateArgumentCountName[lIndex]);
    lFrmNewArgument.JumpTo(lTabSheet);
    FListArguments.Add(lFrmNewArgument);
  end;
  pgcSteps.ActivePageIndex := lActivePageIndex;
end;

procedure TfrmNewCriteria.MakeCriteriaDescriptor;
var
  lArgument: TfrmNewArgument;
  lDescription: string;
begin
  lDescription := '';
  for lArgument in FListArguments do
  begin
    lDescription := lDescription + ifthen(lDescription <> '', ', ') +
      lArgument.Description;
  end;
  FCriteriaDescriptor.Description := 'TCriteria.CreateAs' + GetCreateAsText + '(' +
    lDescription + ')';
end;

class function TfrmNewCriteria.NewCriteria: TCriteriaDescriptor;
var
  lfrmNewCriteria: TfrmNewCriteria;
begin
  lfrmNewCriteria := TfrmNewCriteria.Create(nil);
  try
    lfrmNewCriteria.ShowModal;
    Result := lfrmNewCriteria.CriteriaDescriptor;
  finally
    lfrmNewCriteria.Free;
  end;
end;

procedure TfrmNewCriteria.RefreshButtons;
begin
  btnBack.Enabled := pgcSteps.ActivePageIndex > 0;
  btnFinish.Enabled := (pgcSteps.ActivePageIndex = pgcSteps.PageCount - 1) and (pgcSteps.PageCount > 1);
  btnFinish.Default := btnFinish.Enabled;
  btnNext.Enabled := not btnFinish.Enabled;
  btnNext.Default := btnNext.Enabled;
end;

procedure TfrmNewCriteria.ValidateCreateType;
begin
  if (cbxCreateAs.ItemIndex < 0) then
  begin
    ShowBalloHintForControl(cbxCreateAs, 'The create type is required!');
    Abort;
  end;
  MakeArguments;
end;

procedure TfrmNewCriteria.ValidateCurrentArgument;
begin
  FListArguments.Items[pgcSteps.ActivePageIndex - 1].Validate;
end;

end.
