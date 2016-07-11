unit AM.Freedom.frmViewRefactorings;

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
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  System.Generics.Collections,
  AM.Freedom.FreedomBindKeyBoard;

type
  TfrmViewRefactorings = class(TfrmBase)
    btnedtSearch: TButtonedEdit;
    lvRefactorings: TListView;
    btnOK: TButton;
    btnCancel: TButton;
    procedure lvRefactoringsDblClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnedtSearchChange(Sender: TObject);
    procedure btnedtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    function KeyExecutor: TCustomKeyExecutor;
    procedure PrepareRefactorings(pSearchText: String = '');
    procedure DoOK;
  public
    class function ViewRefactorings: TCustomKeyExecutor;
  end;

implementation

{$R *.dfm}

uses
  AM.Freedom.dclFreedomORMConfig,
  System.StrUtils,
  Vcl.Menus;

{ TfrmViewRefactorings }

function TfrmViewRefactorings.KeyExecutor: TCustomKeyExecutor;
begin
  Result := TCustomKeyExecutor(lvRefactorings.Selected.Data);
end;

procedure TfrmViewRefactorings.lvRefactoringsDblClick(Sender: TObject);
begin
  inherited;
  DoOK;
end;

procedure TfrmViewRefactorings.btnedtSearchChange(Sender: TObject);
begin
  inherited;
  PrepareRefactorings(btnedtSearch.Text);
end;

procedure TfrmViewRefactorings.btnedtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_DOWN) then
  begin
    if (lvRefactorings.Items.Count > 0) then
    begin
      lvRefactorings.SetFocus;
      lvRefactorings.Items.Item[0].Focused := True;
      lvRefactorings.Items.Item[0].Selected := True;
    end;
  end;
end;

procedure TfrmViewRefactorings.btnOKClick(Sender: TObject);
begin
  inherited;
  DoOK;
end;

procedure TfrmViewRefactorings.DoOK;
begin
  if (lvRefactorings.Selected <> nil) then
  begin
    ModalResult := mrOK;
  end;
end;

procedure TfrmViewRefactorings.PrepareRefactorings(pSearchText: String);
var
  lRefactoringList: TObjectList<TCustomKeyExecutor>;
  lKeyExecutor: TCustomKeyExecutor;
  lItem: TListItem;
begin
  lRefactoringList := TFreedomBindKeyBoard.KeyExecutorList;
  lvRefactorings.Items.Clear;
  for lKeyExecutor in lRefactoringList do
  begin
    if (lKeyExecutor.Description <> '') then
    begin
      if (pSearchText = '') or ContainsText(lKeyExecutor.Description, pSearchText) then
      begin
        lItem := lvRefactorings.Items.Add;
        lItem.Caption := lKeyExecutor.Description;
        lItem.Data := lKeyExecutor;
        if (lKeyExecutor.ShortCut <> 0) then
        begin
          lItem.Caption := Format('%s ( %s )', [lItem.Caption, ShortCutToText(lKeyExecutor.ShortCut)]);
        end;
      end;
    end;
  end;
end;

class function TfrmViewRefactorings.ViewRefactorings: TCustomKeyExecutor;
var
  lfrmViewRefactorings: TfrmViewRefactorings;
begin
  Result := nil;
  lfrmViewRefactorings := TfrmViewRefactorings.Create(nil);
  try
    lfrmViewRefactorings.PrepareRefactorings;
    if (lfrmViewRefactorings.ShowModal = mrOk) then
    begin
      Result := lfrmViewRefactorings.KeyExecutor;
    end;
  finally
    lfrmViewRefactorings.Free;
  end;
end;

end.
