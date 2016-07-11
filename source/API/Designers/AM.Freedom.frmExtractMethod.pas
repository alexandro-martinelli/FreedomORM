unit AM.Freedom.frmExtractMethod;

interface

uses
  {$REGION 'Uses'}
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
  UnitReader.ClassElement,
  UnitReader.Elements,
  Vcl.ImgList,
  AM.UnitReader.Enumerations,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  AM.Freedom.MethodParameter,
  UnitReader.UnitElement,
  AM.Freedom.frmMethodParameter;
  {$ENDREGION}

type
  TExtractMethodResult = class
  private
    FMethodImplemetation: String;
    FVisibilityScope: TVisibilityScope;
    FDeclareOnLine: Integer;
    FMethodDeclaration: String;
    FMethodResultName: String;
  public
    property MethodImplemetation: String read FMethodImplemetation write FMethodImplemetation;
    property VisibilityScope: TVisibilityScope read FVisibilityScope write FVisibilityScope;
    property DeclareOnLine: Integer read FDeclareOnLine write FDeclareOnLine;
    property MethodDeclaration: String read FMethodDeclaration write FMethodDeclaration;
    property MethodResultName: String read FMethodResultName write FMethodResultName;
  end;

  TfrmExtractMethod = class(TfrmBase)
    {$REGION 'Fields'}
    lblClassName: TLabel;
    lblMethodName: TLabel;
    lblMethodResult: TLabel;
    edtClassName: TEdit;
    chkVirtual: TCheckBox;
    chkFinal: TCheckBox;
    chkDynamic: TCheckBox;
    chkOverload: TCheckBox;
    cbxMethodName: TComboBox;
    cbxType: TComboBox;
    btnOK: TButton;
    btnCancel: TButton;
    cbxVisibilityScope: TComboBox;
    rdbVisibilityScope: TRadioButton;
    rdbAfterMethodDeclaration: TRadioButton;
    cbxMethodList: TComboBox;
    chkIsClassMethod: TCheckBox;
    lblDeclareOn: TLabel;
    pgcMethodComplementation: TPageControl;
    tsMethodImplementation: TTabSheet;
    tsMethodParameters: TTabSheet;
    mmoMethodImplemetation: TMemo;
    lvParameters: TListView;
    pnlButtons: TPanel;
    btnAddParameter: TButton;
    btnAlterParameter: TButton;
    btnDelParameter: TButton;
    {$ENDREGION}
    procedure chkVirtualClick(Sender: TObject);
    procedure rdbVisibilityScopeClick(Sender: TObject);
    procedure rdbAfterMethodDeclarationClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnAddParameterClick(Sender: TObject);
    procedure btnDelParameterClick(Sender: TObject);
    procedure btnAlterParameterClick(Sender: TObject);
  strict private const
    cVarTypes: array [0..56] of string = ('Boolean', 'ShortInt', 'ShortInt', 'SmallInt', 'Integer', 'Byte', 'Word',
        'Cardinal', 'Int64', 'UInt64', 'NativeInt', 'NativeUInt', 'Single', 'Double', 'Extended', 'Currency', 'Real',
        'ShortString', 'OpenString', 'File', 'Text', 'ByteBool', 'WordBool', 'LongBool', 'Real48', 'Extended80',
        'Pointer', 'PWideChar', 'PAnsiChar', 'Variant', 'OleVariant', 'LongInt', 'LongWord', 'TextFile', 'AnsiChar',
        'Char', 'String', 'AnsiString', 'WideString', 'PChar', 'WideChar', 'UnicodeString', 'TObject', 'TClass',
        'TDate', 'TTime', 'TDateTime', 'TStrings', 'TStringList', 'TStream', 'TMemoryStream', 'TStringStream',
        'TFileStream', 'TList<T>', 'TArray<T>', 'TObjectList<T>', 'TInterfacedObject');
    cReservedWords: Array [0..34] of String = ('if', 'then', 'begin', 'end', 'else', 'procedure', 'function', 'String',
        'Integer', 'Double', 'Real', 'Extended', 'Currency', 'Boolean', 'TDate', 'TTime', 'TDateTime', 'Self', 'Array',
        'of', 'Variant', 'and', 'or', 'not', 'xor', 'shr', 'mod', 'div', 'override', 'overload', 'virtual', 'final',
        'abstract', 'inline', 'nil');
    cTextDelimiters: Array [0.. 4] of String = ('.', ',', ';', '(', ')');
  private
    FMethodImplementation: String;
    FClassElement: TClassElement;
    FUnitElement: TUnitElement;
    FParameterList: TMethodParameterList;
    procedure SetVariables(pMethodImplementation, pMethodName: String; pClassElement: TClassElement; pUnitElement: TUnitElement);
    procedure SetMethodName(pMethodName: String);
    procedure DoSetVariables;
    procedure FixComboBoxes;
    function GetVisibilityScope: TVisibilityScope;
    function GetDeclareOnLine: Integer;
    function DoGetMethodStr(pImplementation: Boolean = False): String;
    function GetParametersDeclaration: string;
    function GetMethodDeclaration: string;
    function GetMethodImplementation: String;
    function ValidateNames: Boolean;
    procedure FixParameterListView;
    function GetMethodResultName: String;
    class function IsValidMethodNameForDeclaration(pMethodName: String): Boolean;
    class function NotContainsTextDelimiter(pMethodName: String): Boolean;
    class function NotIsReservedWord(pMethodName: String): Boolean;
  protected
    procedure DoInitializeHistory; override;
  public
    constructor Create(AOwner: TComponent); override;

    class function ExtractMethod(pMethodImplementation: String; pClassElement: TClassElement; pUnitElement: TUnitElement): TExtractMethodResult;

    class function DeclareMethod(pMethodName: String; pClassElement: TClassElement; pUnitElement: TUnitElement): TExtractMethodResult;

    property VisibilityScope: TVisibilityScope read GetVisibilityScope;
    property DeclareOnLine: Integer read GetDeclareOnLine;
    property MethodDeclaration: string read GetMethodDeclaration;
    property MethodImplementation: String read GetMethodImplementation;
    property MethodResultName: String read GetMethodResultName;
  end;

implementation

{$R *.dfm}

uses
  System.StrUtils,
  AM.Freedom.Consts;

{ TfrmExtractMethod }

procedure TfrmExtractMethod.btnAddParameterClick(Sender: TObject);
var
  lParameter: TMethodParameter;
begin
  inherited;
  repeat
    lParameter := TfrmMethodParameter.NewParameter;
    if (Assigned(lParameter)) then
    begin
      if (FParameterList.FindParameter(lParameter.ParameterName) = nil) then
      begin
        FParameterList.Add(lParameter);
      end;
    end;
  until lParameter = nil;
  FixParameterListView;
end;

procedure TfrmExtractMethod.btnAlterParameterClick(Sender: TObject);
var
  lParameter, lSelectedParameter: TMethodParameter;
  lIndex: Integer;
  lCanFixView: Boolean;
begin
  inherited;
  lCanFixView := True;
  if (lvParameters.Selected <> nil) then
  begin
    lSelectedParameter := TMethodParameter(lvParameters.Selected.Data);
    lParameter := TfrmMethodParameter.EditParameter(lSelectedParameter);
    if (Assigned(lParameter)) then
    begin
      if SameText(lParameter.ParameterName, lSelectedParameter.ParameterName) then
      begin
        lSelectedParameter.Assign(lParameter);
      end
      else if (FParameterList.FindParameter(lParameter.ParameterName) = nil) then
      begin
        lIndex := FParameterList.IndexOf(lSelectedParameter);
        FParameterList.Remove(lSelectedParameter);
        if (lIndex > FParameterList.Count - 1) then
        begin
          FParameterList.Add(lParameter)
        end
        else
        begin
          FParameterList.Insert(lIndex, lParameter);
        end;
        lParameter := nil;
      end
      else
      begin
        ShowBalloHintForControl(btnAlterParameter, Format('The parameter named %s alread exists.',
            [lParameter.ParameterName]));
        lCanFixView := False;
      end;
      FreeAndNil(lParameter);
      if (lCanFixView) then
      begin
        FixParameterListView;
      end;
    end;
  end;
end;

procedure TfrmExtractMethod.btnDelParameterClick(Sender: TObject);
begin
  inherited;
  if (lvParameters.Selected <> nil) then
  begin
    FParameterList.Remove(TMethodParameter(lvParameters.Selected.Data));
    FixParameterListView;
  end;
end;

procedure TfrmExtractMethod.btnOKClick(Sender: TObject);
begin
  inherited;
  if (ValidateNames) then
  begin
    ModalResult := mrOK;
  end;
end;

procedure TfrmExtractMethod.chkVirtualClick(Sender: TObject);
begin
  inherited;
  if (Sender = chkVirtual) then
  begin
    if chkDynamic.Checked then
    begin
      chkDynamic.Checked := False;
    end;
  end;
  if (Sender = chkDynamic) then
  begin
    if chkVirtual.Checked then
    begin
      chkVirtual.Checked := False;
    end;
  end;
end;

constructor TfrmExtractMethod.Create(AOwner: TComponent);
begin
  inherited;
  FParameterList := TMethodParameterList.Create;
end;

class function TfrmExtractMethod.DeclareMethod(pMethodName: String; pClassElement: TClassElement;
  pUnitElement: TUnitElement): TExtractMethodResult;
var
  lfrmExtractMethod: TfrmExtractMethod;
begin
  Result := nil;
  if (IsValidMethodNameForDeclaration(pMethodName)) then
  begin
    lfrmExtractMethod := TfrmExtractMethod.Create(nil);
    try
      lfrmExtractMethod.SetVariables('', pMethodName, pClassElement, pUnitElement);
      lfrmExtractMethod.Caption := 'Declare method';
      if (lfrmExtractMethod.ShowModal = mrOk) then
      begin
        Result := TExtractMethodResult.Create;
        Result.MethodImplemetation := lfrmExtractMethod.MethodImplementation;
        Result.VisibilityScope := lfrmExtractMethod.VisibilityScope;
        Result.DeclareOnLine := lfrmExtractMethod.DeclareOnLine;
        Result.MethodDeclaration := lfrmExtractMethod.MethodDeclaration;
        Result.MethodResultName := lfrmExtractMethod.MethodResultName;
      end;
    finally
      lfrmExtractMethod.Free;
    end;
  end;
end;

function TfrmExtractMethod.DoGetMethodStr(pImplementation: Boolean): String;
begin
  Result := '';
  if (chkIsClassMethod.Checked) then
  begin
    Result := 'class ';
  end;
  if (cbxType.Text <> '') then
  begin
    Result := Result + 'function ';
  end
  else
  begin
    Result := Result + 'procedure ';
  end;
  if (pImplementation) then
  begin
    Result := Result + edtClassName.Text + '.';
  end;
  Result := Result + Trim(cbxMethodName.Text);
  if (FParameterList.Count > 0) then
  begin
    Result := Result + '(' + GetParametersDeclaration + ')';
  end;
  if (cbxType.Text <> '') then
  begin
    Result := Result + ': ' + cbxType.Text;
  end;
  Result := Result + ';';
  if (not pImplementation) then
  begin
    if (chkVirtual.Checked) then
    begin
      Result := Result + ' virtual;';
    end
    else if (chkDynamic.Checked) then
    begin
      Result := Result + ' dynamic;';
    end;
    if (chkOverload.Checked) then
    begin
      Result := Result + ' overload;';
    end;
    if (chkFinal.Checked) then
    begin
      Result := Result + ' final;';
    end;
  end;
end;

procedure TfrmExtractMethod.DoInitializeHistory;
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

class function TfrmExtractMethod.ExtractMethod(pMethodImplementation: String; pClassElement: TClassElement; pUnitElement: TUnitElement): TExtractMethodResult;
var
  lfrmExtractMethod: TfrmExtractMethod;
begin
  lfrmExtractMethod := TfrmExtractMethod.Create(nil);
  Result := nil;
  try
    lfrmExtractMethod.SetVariables(pMethodImplementation, '', pClassElement, pUnitElement);
    lfrmExtractMethod.Caption := 'Extract method';
    if (lfrmExtractMethod.ShowModal = mrOk) then
    begin
      Result := TExtractMethodResult.Create;
      Result.MethodImplemetation := lfrmExtractMethod.MethodImplementation;
      Result.VisibilityScope := lfrmExtractMethod.VisibilityScope;
      Result.DeclareOnLine := lfrmExtractMethod.DeclareOnLine;
      Result.MethodDeclaration := lfrmExtractMethod.MethodDeclaration;
      Result.MethodResultName := lfrmExtractMethod.MethodResultName;
    end;
  finally
    lfrmExtractMethod.Free;
  end;
end;

procedure TfrmExtractMethod.FixParameterListView;
var
  lParameter: TMethodParameter;
  lItem: TListItem;
begin
  lvParameters.Clear;
  for lParameter in FParameterList do
  begin
    lItem := lvParameters.Items.Add;
    lItem.Caption := lParameter.ParameterName;
    lItem.SubItems.Add(lParameter.ParameterType);
    lItem.SubItems.Add(lParameter.Directive.ToString);
    lItem.Data := lParameter;
  end;
end;

function TfrmExtractMethod.GetDeclareOnLine: Integer;
begin
  Result := 0;
  if (rdbAfterMethodDeclaration.Checked) then
  begin
    Result := TLineElement(cbxMethodList.Items.Objects[cbxMethodList.ItemIndex]).LineNumber;
  end;
end;

function TfrmExtractMethod.GetMethodDeclaration: string;
begin
  Result := DoGetMethodStr;
end;

function TfrmExtractMethod.GetMethodImplementation: String;
begin
  Result :=
      DoGetMethodStr(True) + sLineBreak +
      'begin' + sLineBreak +
         ifthen(mmoMethodImplemetation.Lines.Text <> '', mmoMethodImplemetation.Lines.Text) + sLineBreak +
      'end;';
end;

function TfrmExtractMethod.GetMethodResultName: String;
begin
  Result := cbxMethodName.Text;
end;

function TfrmExtractMethod.GetParametersDeclaration: string;
var
  lParameter: TMethodParameter;
begin
  Result := '';
  for lParameter in FParameterList do
  begin
    Result := Result + ifthen(Result <> '', '; ') + lParameter.Declaration;
  end;
end;

function TfrmExtractMethod.GetVisibilityScope: TVisibilityScope;
begin
  Result := TVisibilityScope(cbxVisibilityScope.ItemIndex);
end;

class function TfrmExtractMethod.IsValidMethodNameForDeclaration(pMethodName: String): Boolean;
begin
  Result := NotContainsTextDelimiter(pMethodName);
  if (Result) then
  begin
    Result := NotIsReservedWord(pMethodName);
  end;
end;

class function TfrmExtractMethod.NotIsReservedWord(pMethodName: String): Boolean;
var
  lIndex: Integer;
begin
  for lIndex := Low(cReservedWords) to High(cReservedWords) do
  begin
    Result := not SameText(cReservedWords[lIndex], pMethodName);
    if (not Result) then
    begin
      Break;
    end;
  end;
end;

class function TfrmExtractMethod.NotContainsTextDelimiter(pMethodName: String): Boolean;
var
  lIndex: Integer;
begin
  for lIndex := Low(cTextDelimiters) to High(cTextDelimiters) do
  begin
    Result := not ContainsText(pMethodName, cTextDelimiters[lIndex]);
    if (not Result) then
    begin
      Break;
    end;
  end;
end;

procedure TfrmExtractMethod.rdbAfterMethodDeclarationClick(Sender: TObject);
begin
  inherited;
  rdbVisibilityScope.TabStop := True;
  cbxMethodList.Enabled := rdbAfterMethodDeclaration.Checked;
  cbxVisibilityScope.Enabled := False;
end;

procedure TfrmExtractMethod.rdbVisibilityScopeClick(Sender: TObject);
begin
  inherited;
  rdbAfterMethodDeclaration.TabStop := True;
  cbxVisibilityScope.Enabled := rdbVisibilityScope.Checked;
  cbxMethodList.Enabled := False;
end;

procedure TfrmExtractMethod.SetMethodName(pMethodName: String);
var
  lMethodName: String;
  lIndex: Integer;
begin
  for lIndex := 1 to Length(pMethodName) do
  begin
    if IndexText(pMethodName[lIndex], TConsts.cStringDelimiters) < 0 then
    begin
      lMethodName := lMethodName + pMethodName[lIndex];
    end
    else
    begin
      Break;
    end;
  end;
  cbxMethodName.Text := lMethodName;
  cbxMethodName.Enabled := cbxMethodName.Text = '';
  if (not cbxMethodName.Enabled) then
  begin
    cbxMethodName.Color := $00BCBCBC;
    ActiveControl := chkVirtual;
  end;
end;

procedure TfrmExtractMethod.SetVariables(pMethodImplementation, pMethodName: String; pClassElement: TClassElement; pUnitElement: TUnitElement);
begin
  FMethodImplementation := pMethodImplementation;
  FUnitElement := pUnitElement;
  FClassElement := pClassElement;
  SetMethodName(pMethodName);
  DoSetVariables;
end;

procedure TfrmExtractMethod.DoSetVariables;
var
  lIndex: Integer;
begin
  mmoMethodImplemetation.Lines.Text := FMethodImplementation;
  for lIndex := mmoMethodImplemetation.Lines.Count - 1 downto 0 do
  begin
    if (Trim(mmoMethodImplemetation.Lines.Strings[lIndex]) = '') then
    begin
      mmoMethodImplemetation.Lines.Delete(lIndex);
    end;
  end;
  if FMethodImplementation = '' then
  begin
    tsMethodImplementation.TabVisible := False;
    tsMethodParameters.TabVisible := False;
    tsMethodParameters.Show;
  end;
  edtClassName.Text := FClassElement.ClassElementName;
  cbxMethodList.Items.Clear;
  FixComboBoxes;
end;

procedure TfrmExtractMethod.FixComboBoxes;
var
  lMethod: TLineElement;
begin
  for lMethod in FClassElement.MethodDeclarationList do
  begin
    cbxMethodList.AddItem(FUnitElement.ExtractDisplayMethodName(lMethod), lMethod);
  end;
  if (cbxMethodList.Items.Count > 0) then
  begin
    cbxMethodList.ItemIndex := 0;
  end;
end;

function TfrmExtractMethod.ValidateNames: Boolean;
var
  lStrings: TStrings;
begin
  Result := False;
  if (Trim(cbxMethodName.Text) = '') then
  begin
    ShowBalloHintForControl(cbxMethodName, 'The method name is required');
  end
  else
  begin
    lStrings := TStringList.Create;
    try
      ExtractStrings(['.', ',', ';', '(', ')', ' ', '[', ']', ':', '=', #13, #10], [' '], pWideChar(cbxMethodName.Text), lStrings);
      if (lStrings.Count > 1) then
      begin
        ShowBalloHintForControl(cbxMethodName, 'The method name can only contain numbers and letters');
      end
      else
      begin
        Result := True;
      end;
    finally
      lStrings.Free;
    end;
  end;
end;

end.
