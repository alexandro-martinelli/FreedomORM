unit AM.Freedom.frmDeclareVariable;

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
  Vcl.StdCtrls;

type
  TDeclareVariable = class
  private
    FVarName: String;
    FVarType: String;
    FDeclareOnCursor: Boolean;
    FInitialValue: String;
    FInitializeBeforeCurrentRow: Boolean;
  public
    property VarName: String read FVarName write FVarName;
    property VarType: String read FVarType write FVarType;
    property DeclareOnCursor: Boolean read FDeclareOnCursor write FDeclareOnCursor;
    property InitialValue: String read FInitialValue write FInitialValue;
    property InitializeBeforeCurrentRow: Boolean read FInitializeBeforeCurrentRow write FInitializeBeforeCurrentRow;
  end;

  TfrmDeclareVariable = class(TfrmBase)
    lblVarName: TLabel;
    lblVarType: TLabel;
    cbxVarName: TComboBox;
    cbxType: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    lblInitialValue: TLabel;
    cbxInitialValue: TComboBox;
    chkInitializeBeforeCurrentRow: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure cbxVarNameChange(Sender: TObject);
  strict private const
    cVarTypes: array [0..56] of string = ('Boolean', 'ShortInt', 'ShortInt', 'SmallInt', 'Integer', 'Byte', 'Word',
        'Cardinal', 'Int64', 'UInt64', 'NativeInt', 'NativeUInt', 'Single', 'Double', 'Extended', 'Currency', 'Real',
        'ShortString', 'OpenString', 'File', 'Text', 'ByteBool', 'WordBool', 'LongBool', 'Real48', 'Extended80',
        'Pointer', 'PWideChar', 'PAnsiChar', 'Variant', 'OleVariant', 'LongInt', 'LongWord', 'TextFile', 'AnsiChar',
        'Char', 'String', 'AnsiString', 'WideString', 'PChar', 'WideChar', 'UnicodeString', 'TObject', 'TClass',
        'TDate', 'TTime', 'TDateTime', 'TStrings', 'TStringList', 'TStream', 'TMemoryStream', 'TStringStream',
        'TFileStream', 'TList<T>', 'TArray<T>', 'TObjectList<T>', 'TInterfacedObject');
  private
    FIsExtract: Boolean;
    function ValidateEdits: Boolean;
    procedure SetVariable(pVarName, pVarType: String);
    function GetVarName: String;
    function GetVarType: String;
    function GetInitialValue: String;
    procedure LoadVariableCombination;
    procedure SaveVariableCombination;
    function GetInitializeBeforeCurrentRow: Boolean;
    procedure SetIsExtract(pIsExtract: Boolean);
  protected
    procedure DoInitializeHistory; override;
    procedure DoShow; override;
    procedure DoFinalizeHistory; override;
  public
    class function DeclareVariable(pVarName: String; pVarType: String = ''): TDeclareVariable;
    class function ExtractVariable(pVarName: String; pVarType: String = ''): TDeclareVariable;

    property VarName: String read GetVarName;
    property VarType: String read GetVarType;
    property InitialValue: String read GetInitialValue;
    property InitializeBeforeCurrentRow: Boolean read GetInitializeBeforeCurrentRow;
  end;

implementation

{$R *.dfm}

procedure TfrmDeclareVariable.btnOKClick(Sender: TObject);
begin
  if (ValidateEdits) then
  begin
    ModalResult := mrOK;
  end;
end;

procedure TfrmDeclareVariable.cbxVarNameChange(Sender: TObject);
begin
  inherited;
  if (cbxVarName.Text <> '') then
  begin
    LoadVariableCombination;
  end;
end;

class function TfrmDeclareVariable.DeclareVariable(pVarName, pVarType: String): TDeclareVariable;
var
  lfrmDeclareVariable: TfrmDeclareVariable;
begin
  Result := TDeclareVariable.Create;
  lfrmDeclareVariable := TfrmDeclareVariable.Create(nil);
  try
    lfrmDeclareVariable.SetVariable(pVarName, pVarType);
    lfrmDeclareVariable.SetIsExtract(False);
    if lfrmDeclareVariable.ShowModal = mrOK then
    begin
      Result.VarName := lfrmDeclareVariable.VarName;
      Result.VarType := lfrmDeclareVariable.VarType;
      Result.DeclareOnCursor := pVarName = '';
      Result.InitialValue := lfrmDeclareVariable.InitialValue;
    end;
  finally
    lfrmDeclareVariable.Free;
  end;
end;

procedure TfrmDeclareVariable.DoFinalizeHistory;
begin
  SaveVariableCombination;
  inherited;
end;

procedure TfrmDeclareVariable.DoInitializeHistory;
var
  lIndex: Integer;
begin
  for lIndex := Low(cVarTypes) to High(cVarTypes) do
  begin
    if (cbxType.Items.IndexOf(cVarTypes[lIndex]) < 0) then
    begin
      cbxType.Items.Add(cVarTypes[lIndex]);
    end;
  end;
end;

procedure TfrmDeclareVariable.DoShow;
begin
  inherited;
  if (cbxVarName.Enabled) then
  begin
    cbxVarName.SetFocus;
  end
  else
  begin
    cbxType.SetFocus;
  end;
end;

class function TfrmDeclareVariable.ExtractVariable(pVarName, pVarType: String): TDeclareVariable;
var
  lfrmDeclareVariable: TfrmDeclareVariable;
begin
  Result := TDeclareVariable.Create;
  lfrmDeclareVariable := TfrmDeclareVariable.Create(nil);
  try
    lfrmDeclareVariable.SetVariable(pVarName, pVarType);
    lfrmDeclareVariable.SetIsExtract(True);
    if (lfrmDeclareVariable.ShowModal = mrOK) then
    begin
      Result.VarName := lfrmDeclareVariable.VarName;
      Result.VarType := lfrmDeclareVariable.VarType;
      Result.DeclareOnCursor := pVarName = '';
      Result.InitialValue := lfrmDeclareVariable.InitialValue;
      Result.InitializeBeforeCurrentRow := lfrmDeclareVariable.InitializeBeforeCurrentRow;
    end;
  finally
    lfrmDeclareVariable.Free;
  end;
end;

function TfrmDeclareVariable.GetInitializeBeforeCurrentRow: Boolean;
begin
  Result := chkInitializeBeforeCurrentRow.Checked;
end;

function TfrmDeclareVariable.GetInitialValue: String;
begin
  Result := '';
  if (cbxInitialValue.Enabled) then
  begin
    Result := cbxInitialValue.Text;
  end;
end;

function TfrmDeclareVariable.GetVarName: String;
begin
  Result := cbxVarName.Text;
end;

function TfrmDeclareVariable.GetVarType: String;
begin
  Result := cbxType.Text;
end;

procedure TfrmDeclareVariable.LoadVariableCombination;
var
  lVarCombinationName: String;
begin
  lVarCombinationName := Format('VariableCombination_%s', [cbxVarName.Text]);
  cbxType.Text := IniFile.ReadString(lVarCombinationName, 'type', '');
  cbxInitialValue.Text := IniFile.ReadString(lVarCombinationName, 'InitialValue', '');
end;

procedure TfrmDeclareVariable.SaveVariableCombination;
var
  lVarCombinationName: String;
begin
  lVarCombinationName := Format('VariableCombination_%s', [cbxVarName.Text]);
  IniFile.WriteString(lVarCombinationName, 'type', cbxType.Text);
  IniFile.WriteString(lVarCombinationName, 'InitialValue', cbxInitialValue.Text);
end;

procedure TfrmDeclareVariable.SetIsExtract(pIsExtract: Boolean);
begin
  FIsExtract := pIsExtract;
  chkInitializeBeforeCurrentRow.Enabled := pIsExtract;
end;

procedure TfrmDeclareVariable.SetVariable(pVarName, pVarType: String);
begin
  cbxVarName.Text := pVarName;
  LoadVariableCombination;
  cbxVarName.Enabled := pVarName = '';
  cbxInitialValue.Enabled := cbxVarName.Enabled;
  if (pVarType <> '') then
  begin
    cbxType.Text := pVarType;
  end;
end;

function TfrmDeclareVariable.ValidateEdits: Boolean;
begin
  Result := False;
  if (Trim(cbxVarName.Text) = '') then
  begin
    ShowBalloHintForControl(cbxVarName, 'The variable name is required');
  end
  else if (Trim(cbxType.Text) = '') then
  begin
    ShowBalloHintForControl(cbxType, 'The variable type is required');
  end
  else
  begin
    Result := True;
  end;
end;

end.
