unit AM.Freedom.frmMethodParameter;

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
  AM.Freedom.MethodParameter,
  Vcl.StdCtrls;

type
  TfrmMethodParameter = class(TfrmBase)
    lblParameterName: TLabel;
    lblParameterType: TLabel;
    lblParameterDirective: TLabel;
    cbxParameterName: TComboBox;
    cbxParameterType: TComboBox;
    cbxParameterDirective: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure cbxParameterNameChange(Sender: TObject);
  strict private const
    cVarTypes: array [0..56] of string = ('Boolean', 'ShortInt', 'ShortInt', 'SmallInt', 'Integer', 'Byte', 'Word',
        'Cardinal', 'Int64', 'UInt64', 'NativeInt', 'NativeUInt', 'Single', 'Double', 'Extended', 'Currency', 'Real',
        'ShortString', 'OpenString', 'File', 'Text', 'ByteBool', 'WordBool', 'LongBool', 'Real48', 'Extended80',
        'Pointer', 'PWideChar', 'PAnsiChar', 'Variant', 'OleVariant', 'LongInt', 'LongWord', 'TextFile', 'AnsiChar',
        'Char', 'String', 'AnsiString', 'WideString', 'PChar', 'WideChar', 'UnicodeString', 'TObject', 'TClass',
        'TDate', 'TTime', 'TDateTime', 'TStrings', 'TStringList', 'TStream', 'TMemoryStream', 'TStringStream',
        'TFileStream', 'TList<T>', 'TArray<T>', 'TObjectList<T>', 'TInterfacedObject');
  private
    function GetParameterName: String;
    function GetParameterType: String;
    function GetDirective: TParameterDirective;
    function ValidateFields: Boolean;
    procedure SetParameter(pParameter: TMethodParameter);
    procedure LoadParameterCombination;
    procedure SaveParameterCombination;
  protected
    procedure DoInitializeHistory; override;
  public
    class function NewParameter: TMethodParameter;
    class function EditParameter(pParameter: TMethodParameter): TMethodParameter;
    constructor Create(AOwner: TComponent); override;
    property ParameterName: String read GetParameterName;
    property ParameterType: String read GetParameterType;
    property Directive: TParameterDirective read GetDirective;
  end;

implementation

{$R *.dfm}

{ TfrmMethodParameter }

procedure TfrmMethodParameter.btnOKClick(Sender: TObject);
begin
  inherited;
  if (ValidateFields) then
  begin
    SaveParameterCombination;
    ModalResult := mrOK;
  end;
end;

procedure TfrmMethodParameter.cbxParameterNameChange(Sender: TObject);
begin
  inherited;
  if (Trim(cbxParameterName.Text) <> '') then
  begin
    LoadParameterCombination;
  end;
end;

procedure TfrmMethodParameter.LoadParameterCombination;
var
  lCombination: String;
begin
  lCombination := Format('Parameter_%s', [cbxParameterName.Text]);
  cbxParameterType.Text := IniFile.ReadString(lCombination, 'Parameter_Type', cbxParameterType.Text);
  cbxParameterDirective.ItemIndex := IniFile.ReadInteger(lCombination, 'Parameter_Directive', cbxParameterDirective.ItemIndex);
end;

constructor TfrmMethodParameter.Create(AOwner: TComponent);
var
  lIndex: Integer;
begin
  inherited;
  cbxParameterDirective.Items.Clear;
  for lIndex := Ord(Low(TParameterDirective)) to Ord(High(TParameterDirective)) do
  begin
    cbxParameterDirective.Items.Add(TParameterDirective(lIndex).ToString);
  end;
  cbxParameterDirective.ItemIndex := 0;
end;

procedure TfrmMethodParameter.DoInitializeHistory;
var
  lIndex: Integer;
begin
  for lIndex := Low(cVarTypes) to High(cVarTypes) do
  begin
    if (cbxParameterType.Items.IndexOf(cVarTypes[lIndex]) < 0) then
    begin
      cbxParameterType.Items.Add(cVarTypes[lIndex]);
    end;
  end;
end;

class function TfrmMethodParameter.EditParameter(pParameter: TMethodParameter): TMethodParameter;
var
  lfrmMethodParameter: TfrmMethodParameter;
begin
  lfrmMethodParameter := TfrmMethodParameter.Create(nil);
  try
    Result := TMethodParameter.Create;
    Result.Assign(pParameter);
    lfrmMethodParameter.SetParameter(Result);
    if (lfrmMethodParameter.ShowModal = mrOK) then
    begin
      Result.ParameterName := lfrmMethodParameter.ParameterName;
      Result.ParameterType := lfrmMethodParameter.ParameterType;
      Result.Directive := lfrmMethodParameter.Directive;
    end
    else
    begin
      FreeAndNil(Result);
    end;
  finally
    lfrmMethodParameter.Free;
  end;
end;

function TfrmMethodParameter.GetDirective: TParameterDirective;
begin
  Result := TParameterDirective(cbxParameterDirective.ItemIndex);
end;

function TfrmMethodParameter.GetParameterName: String;
begin
  Result := cbxParameterName.Text;
end;

function TfrmMethodParameter.GetParameterType: String;
begin
  Result := cbxParameterType.Text;
end;

class function TfrmMethodParameter.NewParameter: TMethodParameter;
var
  lfrmMethodParameter: TfrmMethodParameter;
begin
  Result := nil;
  lfrmMethodParameter := TfrmMethodParameter.Create(nil);
  try
    if lfrmMethodParameter.ShowModal = mrOK then
    begin
      Result := TMethodParameter.Create;
      Result.ParameterName := lfrmMethodParameter.ParameterName;
      Result.ParameterType := lfrmMethodParameter.ParameterType;
      Result.Directive := lfrmMethodParameter.Directive;
    end;
  finally
    lfrmMethodParameter.Free;
  end;
end;

procedure TfrmMethodParameter.SaveParameterCombination;
var
  lCombination: String;
begin
  lCombination := Format('Parameter_%s', [cbxParameterName.Text]);
  IniFile.WriteString(lCombination, 'Parameter_Type', cbxParameterType.Text);
  IniFile.WriteInteger(lCombination, 'Parameter_Directive', cbxParameterDirective.ItemIndex);
end;

procedure TfrmMethodParameter.SetParameter(pParameter: TMethodParameter);
begin
  cbxParameterName.Text := pParameter.ParameterName;
  cbxParameterType.Text := pParameter.ParameterType;
  cbxParameterDirective.ItemIndex := Ord(pParameter.Directive);
end;

function TfrmMethodParameter.ValidateFields: Boolean;
begin
  Result := False;
  if (Trim(cbxParameterName.Text) = '') then
  begin
    ShowBalloHintForControl(cbxParameterName, 'The parameter name is required');
  end
  else if (Trim(cbxParameterType.Text) = '') then
  begin
    ShowBalloHintForControl(cbxParameterType, 'The parameter type is required');
  end
  else
  begin
    Result := True;
  end;
end;

end.
