unit AM.Freedom.frmBase;

interface

uses
  {$IFNDEF ORMTEST}
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
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ImgList,
  System.StrUtils;

type
  TfrmBase = class(TForm)
    BalloonHint: TBalloonHint;
    ImageList1: TImageList;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FIniFile: TIniFile;
    procedure InitializeHistory;
    procedure InitializeFormConstraints;
    function CalculateWidth: Integer;
    function CalculateHeight: Integer;
    function CalculateLeft: Integer;
    function CalculateTop: Integer;
    procedure FinalizeFormConstraints;
  protected
    property IniFile: TIniFile read FIniFile;
    procedure ShowBalloHintForControl(pControl: TControl; pDescription: string);
    procedure InitializeIniFile;
    procedure FinalizeHistory;
    procedure ReloadIniFile;
    procedure DoInitializeHistory; virtual;
    procedure DoFinalizeHistory; virtual;
    function ExtractClassUnit(pClassName: String): String;
    function GetSectionNameFromComponentName(pComponentName: String): String;
    function CanAskForClose: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses AM.Freedom.frmClassUnitName, AM.Freedom.dclFreedomORMConfig;

function TfrmBase.CalculateHeight: Integer;
begin
  Result := Height;
  if (BorderStyle <> bsSingle) then
  begin
    if (IniFile.ReadInteger(ClassName, 'Height', 0) > 0) and
       (IniFile.ReadInteger(ClassName, 'Height', 0) <= Screen.Height) then
    begin
      Result := IniFile.ReadInteger(ClassName, 'Height', 0);
    end
    else if (IniFile.ReadInteger(ClassName, 'Height', 0) > 0) then
    begin
      Result := Screen.Height;
    end;
  end;
end;

function TfrmBase.CalculateLeft: Integer;
begin
  if (Width = Screen.Width) then
  begin
    Result := 0;
  end
  else if (IniFile.ReadInteger(ClassName, 'Left', 0) > 0) and
     (IniFile.ReadInteger(ClassName, 'Left', 0) <= (Screen.DesktopWidth - Width)) then
  begin
    Result := IniFile.ReadInteger(ClassName, 'Left', 0);
  end
  else
  begin
    Result := Trunc((Screen.Width - Width) / 2);
  end;
end;

function TfrmBase.CalculateTop: Integer;
begin
  if (Height = Screen.Height) then
  begin
    Result := 0;
  end
  else if (IniFile.ReadInteger(ClassName, 'Top', 0) > 0) and
     (IniFile.ReadInteger(ClassName, 'Top', 0) <= Screen.DesktopHeight - Height) then
  begin
    Result := IniFile.ReadInteger(ClassName, 'Top', 0);
  end
  else
  begin
    Result := Trunc((Screen.DesktopHeight - Height) / 2);
  end;
end;

function TfrmBase.CalculateWidth: Integer;
begin
  Result := Width;
  if (BorderStyle <> bsSingle) then
  begin
    if (IniFile.ReadInteger(ClassName, 'Width', 0) > 0) and
       (IniFile.ReadInteger(ClassName, 'Width', 0) <= Screen.Width) then
    begin
      Result := IniFile.ReadInteger(ClassName, 'Width', 0);
    end
    else if (IniFile.ReadInteger(ClassName, 'Width', 0) > 0) then
    begin
      Result := Screen.Width;
    end;
  end;
end;

function TfrmBase.CanAskForClose: Boolean;
begin
  Result := False;
end;

constructor TfrmBase.Create(AOwner: TComponent);
begin
  inherited;
  InitializeIniFile;
  InitializeHistory;
  InitializeFormConstraints;
end;

destructor TfrmBase.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

procedure TfrmBase.DoFinalizeHistory;
begin
end;

procedure TfrmBase.DoInitializeHistory;
begin
end;

function TfrmBase.ExtractClassUnit(pClassName: String): String;
begin
  Result := '';
  if (pClassName <> '') then
  begin
    if SameText('TFreedomObject', pClassName) then
    begin
      Result := 'AM.Freedom.FreedomObject';
    end
    else if SameText('TFreedomObjectList', pClassName) then
    begin
      Result := 'AM.Freedom.FreedomObjectList';
    end
    else
    begin
      Result := FIniFile.ReadString('ClassToUnitName', pClassName, '');
      if (Result = '') then
      begin
        Result := TfrmClassUnitName.GetUnitNameForClassName(pClassName);
      end;
    end;
  end;
end;

procedure TfrmBase.InitializeIniFile;
begin
  if (Assigned(FIniFile)) then
  begin
    FreeAndnil(FIniFile);
  end;
  FIniFile := TIniFile.Create(TdclFreedomORMConfig.GetInstance.IniFileDirectory + 'FreedomORM.cfg');
end;

procedure TfrmBase.ReloadIniFile;
begin
  InitializeIniFile;
  InitializeHistory;
end;

procedure TfrmBase.ShowBalloHintForControl(pControl: TControl; pDescription: string);
begin
  if Assigned(pControl) then
  begin
    BalloonHint.Title := 'Error';
    BalloonHint.Description := pDescription;
    BalloonHint.ShowHint(pControl);
    TWinControl(pControl).SetFocus;
  end;
end;

procedure TfrmBase.InitializeFormConstraints;
begin
  Width := CalculateWidth;
  Height := CalculateHeight;
  Left := CalculateLeft;
  Top := CalculateTop;
end;

procedure TfrmBase.InitializeHistory;
var
  lIndex: Integer;
  lSections: TStrings;
  lIdent, lCompSection: string;
begin
  lSections := TStringList.Create;
  try
    for lIndex := 0 to ComponentCount - 1 do
    begin
      if (Components[lIndex].InheritsFrom(TComboBox)) and (TComboBox(Components[lIndex]).Style <> csDropDownList) then
      begin
        lSections.Clear;
        TComboBox(Components[lIndex]).Items.Clear;
        lCompSection := GetSectionNameFromComponentName(TComboBox(Components[lIndex]).Name);
        IniFile.ReadSection(lCompSection, lSections);
        for lIdent in lSections do
        begin
          TComboBox(Components[lIndex]).Items.Add(IniFile.ReadString(lCompSection, lIdent, ''));
        end;
      end;
    end;
  finally
    lSections.Free;
  end;
  DoInitializeHistory;
end;

procedure TfrmBase.FinalizeFormConstraints;
begin
  IniFile.WriteInteger(ClassName, 'Width', Width);
  IniFile.WriteInteger(ClassName, 'Height', Height);
  IniFile.WriteInteger(ClassName, 'Left', Left);
  IniFile.WriteInteger(ClassName, 'Top', Top);
end;

procedure TfrmBase.FinalizeHistory;
var
  lIndex: Integer;
  lCombobox: TComboBox;
  lComboIndex: Integer;
  lCompSection: string;
begin
  for lIndex := 0 to ComponentCount - 1 do
  begin
    if (Components[lIndex].InheritsFrom(TComboBox)) and (TComboBox(Components[lIndex]).Style <> csDropDownList) then
    begin
      lCombobox := TComboBox(Components[lIndex]);
      lCompSection := GetSectionNameFromComponentName(lCombobox.Name);
      if (lCombobox.Text <> '') and (lCombobox.Items.IndexOf(lCombobox.Text) < 0) then
      begin
        IniFile.WriteString(lCompSection, IntToStr(lCombobox.Items.Count), lCombobox.Text);
      end;
      for lComboIndex := 0 to lCombobox.Items.Count - 1 do
      begin
        IniFile.WriteString(lCompSection, IntToStr(lComboIndex), lCombobox.Items.Strings[lComboIndex]);
      end;
    end;
  end;
  IniFile.UpdateFile;
  DoFinalizeHistory;
end;

procedure TfrmBase.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FinalizeHistory;
  FinalizeFormConstraints;
end;

procedure TfrmBase.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (fsModal in FormState) and (ModalResult = mrCancel) then
  begin
    if (CanAskForClose) then
    begin
      CanClose := Application.MessageBox('You realy want to close this form?', PWideChar(Caption), MB_ICONQUESTION + MB_YESNO + MB_DEFBUTTON1) = mrYes;
    end;
    if (not CanClose) then
    begin
      ModalResult := mrNone;
    end;
  end;
end;

function TfrmBase.GetSectionNameFromComponentName(pComponentName: String): String;
begin
  if (ContainsText(pComponentName, 'ListClassName')) then
  begin
    Result := 'ListClassName';
  end
  else if (ContainsText(pComponentName, 'ClassName')) then
  begin
    Result := 'ClassName';
  end
  else if (ContainsText(pComponentName, 'UnitName')) then
  begin
    Result := 'UnitName';
  end
  else
  begin
    Result := pComponentName;
  end;
end;

end.
