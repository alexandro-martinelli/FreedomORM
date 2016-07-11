unit AM.Freedom.frmFindMethod;

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
  Vcl.ExtCtrls,
  Vcl.ImgList,
  UnitReader.Elements,
  UnitReader.UnitElement,
  UnitReader.ClassElement,
  Vcl.ComCtrls;

type
  TfrmFindMethod = class(TfrmBase)
    btnedtSearch: TButtonedEdit;
    btnOK: TButton;
    btnCancel: TButton;
    lvMethods: TListView;
    procedure lvMethodsDblClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnedtSearchChange(Sender: TObject);
    procedure btnedtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FUnitElement: TUnitElement;
    procedure FixMethodList(pSearchText: String);
    function ExtractMethodNameFromLine(pMethod: TLineElement): String;
    function GetMethodLineNumber: Integer;
    procedure SetUnitElement(pUnitElement: TUnitElement);
    procedure FixWidthColumnsOfListMethods;
    procedure DoOK;
    function GetHasMethods: Boolean;
  public
    class function FindMethod(pUnitElement: TUnitElement): Integer;
    property MethodLineNumber: Integer read GetMethodLineNumber;
    property HasMethods: Boolean read GetHasMethods;
  end;

implementation

uses
  System.StrUtils,
  UnitReader.EnumerationTypes,
  UnitReader.TokenTypeHelper;

{$R *.dfm}

{ TfrmFindMethod }

procedure TfrmFindMethod.btnedtSearchChange(Sender: TObject);
begin
  FixMethodList(btnedtSearch.Text);
end;

procedure TfrmFindMethod.btnedtSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = VK_DOWN) and (Shift = []) then
  begin
    if (lvMethods.Items.Count > 0) then
    begin
      lvMethods.SetFocus;
      lvMethods.Items.Item[0].Selected := True;
      lvMethods.Items.Item[0].Focused := True;
    end;
  end;
end;

procedure TfrmFindMethod.btnOKClick(Sender: TObject);
begin
  inherited;
  DoOk;
end;

procedure TfrmFindMethod.DoOK;
begin
  if (lvMethods.Selected <> nil) then
  begin
    ModalResult := mrOK;
  end;
end;

function TfrmFindMethod.ExtractMethodNameFromLine(pMethod: TLineElement): String;
begin
  Result := FUnitElement.ExtractDisplayMethodName(pMethod);
end;

class function TfrmFindMethod.FindMethod(pUnitElement: TUnitElement): Integer;
var
  lfrmFindMethod: TfrmFindMethod;
begin
  Result := 0;
  lfrmFindMethod := TfrmFindMethod.Create(nil);
  try
    lfrmFindMethod.SetUnitElement(pUnitElement);
    if (lfrmFindMethod.HasMethods) then
    begin
      if lfrmFindMethod.ShowModal = mrOK then
      begin
        Result := lfrmFindMethod.MethodLineNumber;
      end;
    end;
  finally
    lfrmFindMethod.Free;
  end;
end;

function TfrmFindMethod.GetHasMethods: Boolean;
begin
  Result := lvMethods.Items.Count > 0;
end;

function TfrmFindMethod.GetMethodLineNumber: Integer;
var
  lLine: TLineElement;
  lInternalLine: TLineElement;
begin
  Result := 0;
  if (lvMethods.Selected <> nil) then
  begin
    lLine := TBlockElement(lvMethods.Selected.Data);
    for lInternalLine in TBlockElement(lLine).Lines do
    begin
      if (lInternalLine.TokenType.IsMethodImplementation) then
      begin
        Result := TBlockElement(lInternalLine).Lines.Last.LineNumber;
        Result := FUnitElement.BeginMethodImplementationBeforeLineIndex(Result);
        Break;
      end;
    end;
  end;
end;

procedure TfrmFindMethod.lvMethodsDblClick(Sender: TObject);
begin
  inherited;
  DoOk;
end;

procedure TfrmFindMethod.FixMethodList(pSearchText: String);
var
  lMethod: TLineElement;
  lItem: TListItem;
  lClassElement: TClassElement;
  lMethodName: String;
begin
  lvMethods.Clear;
  for lClassElement in FUnitElement.ClassDeclarationList do
  begin
    for lMethod in lClassElement.MethodDeclarationList do
    begin
      lMethodName := ExtractMethodNameFromLine(lMethod);
      if (pSearchText = '') or (ContainsText(lMethodName, pSearchText)) then
      begin
        lItem := lvMethods.Items.Add;
        lItem.Caption := lMethodName;
        lItem.SubItems.Add(lClassElement.ClassElementName);
        lItem.Data := lMethod;
        if lMethod.TokenType in [tkProcedureDeclaration, tkClassProcedureDeclaration,
            tkDestrutorDeclaration, tkClassDestructorDeclaration] then
        begin
          lItem.ImageIndex := 2;
        end
        else
        begin
          lItem.ImageIndex := 1;
        end;
      end;
    end;
  end;
end;

procedure TfrmFindMethod.FixWidthColumnsOfListMethods;
var
  lItem: TListItem;
  lItemWidth, lSubItemWidth, lSubItemMaxWidth, lMaxWidth: Integer;
  lIndex: Integer;
begin
  lMaxWidth := 0;
  lSubItemMaxWidth := 0;
  for lIndex := 0 to lvMethods.Items.Count - 1 do
  begin
    lItem := lvMethods.Items.Item[lIndex];
    lItemWidth := lvMethods.Canvas.TextWidth(lItem.Caption);
    lSubItemWidth := lvMethods.Canvas.TextWidth(lItem.SubItems.Strings[0]);
    if (lItemWidth > lMaxWidth) then
    begin
      lMaxWidth := lItemWidth;
    end;
    if (lSubItemWidth > lSubItemMaxWidth) then
    begin
      lSubItemMaxWidth := lSubItemWidth;
    end;
  end;
  lvMethods.Column[0].Width := lMaxWidth + 100;
  lvMethods.Column[1].Width := lSubItemMaxWidth + 30;
end;

procedure TfrmFindMethod.SetUnitElement(pUnitElement: TUnitElement);
begin
  FUnitElement := pUnitElement;
  FixMethodList('');
  FixWidthColumnsOfListMethods;
end;

end.
