unit AM.Freedom.frmNewForeign;

interface

uses
  System.Generics.Collections,
  Winapi.Windows,
  Winapi.Messages,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  AM.Freedom.frmBase,
  AM.Freedom.ObjectMapper.ConstraintMapper,
  Vcl.ImgList,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TfrmNewForeign = class(TfrmBase)
    lblColumnNames: TLabel;
    lblRefColumnNames: TLabel;
    lblOnDelete: TLabel;
    lblReferenceTo: TLabel;
    lblOnUpdate: TLabel;
    cbxColumnNames: TComboBox;
    cbxReferencesTo: TComboBox;
    cbxReferenceColumns: TComboBox;
    cbxOnUpdate: TComboBox;
    cbxOnDelete: TComboBox;
    FlowPanel1: TFlowPanel;
    btnCancel: TButton;
    btnOK: TButton;
    mmoSQL: TMemo;
    lblForeignSQL: TLabel;
    btnOptions: TButton;
    lblSchemaNames: TLabel;
    cbxSchemas: TComboBox;
    procedure cbxColumnNamesChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOptionsClick(Sender: TObject);
  private
    FForeign: TForeignMapper;
    procedure DefineForeignSQL;
    function ValidateForeign: Boolean;
    procedure SendValuesToComponents;
    procedure GetValuesFromComponents;
    procedure AddTextToList(pColumnNames: String; pList: TList<String>);
    function GetTextFromList(pList: TList<String>): String;
    procedure SetForeign(const Value: TForeignMapper);
    procedure InitializeOptions;
  public
    constructor Create(AOwner: TComponent); override;
    class function AddForeign: TForeignMapper;
    class procedure AlterForeign(pForeign: TForeignMapper);
    property Foreign: TForeignMapper read FForeign write SetForeign;
  end;

implementation

{$R *.dfm}

uses
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Helper.ForeignOption,
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.frmOptions;

procedure TfrmNewForeign.AddTextToList(pColumnNames: String; pList: TList<String>);
var
  lColumns: TStrings;
  lColumn: String;
begin
  lColumns := TStringList.Create;
  try
    ExtractStrings([',', ';'], [' '], PWideChar(pColumnNames), lColumns);
    for lColumn in lColumns do
    begin
      if (not pList.Contains(lColumn)) then
      begin
        pList.Add(lColumn);
      end;
    end;
  finally
    lColumns.Free;
  end;
end;

class function TfrmNewForeign.AddForeign: TForeignMapper;
var
  lfrmNewForeign: TfrmNewForeign;
begin
  lfrmNewForeign := TfrmNewForeign.Create(nil);
  Result := nil;
  try
    lfrmNewForeign.Foreign := TForeignMapper.Create;
    lfrmNewForeign.ShowModal;
    if (lfrmNewForeign.ModalResult = mrOk) then
    begin
      Result := lfrmNewForeign.Foreign;
    end
    else
    begin
      lfrmNewForeign.Foreign.Free;
    end;
  finally
    lfrmNewForeign.Free;
  end;
end;

class procedure TfrmNewForeign.AlterForeign(pForeign: TForeignMapper);
var
  lfrmNewForeign: TfrmNewForeign;
begin
  lfrmNewForeign := TfrmNewForeign.Create(nil);
  try
    lfrmNewForeign.Foreign := pForeign;
    lfrmNewForeign.ShowModal;
  finally
    lfrmNewForeign.Free;
  end;

end;

procedure TfrmNewForeign.btnOKClick(Sender: TObject);
begin
  inherited;
  if (ValidateForeign) then
  begin
    ModalResult := mrOk;
  end;
end;

procedure TfrmNewForeign.btnOptionsClick(Sender: TObject);
begin
  TfrmOptions.ShowOptions;
  InitializeOptions;
end;

procedure TfrmNewForeign.cbxColumnNamesChange(Sender: TObject);
begin
  inherited;
  DefineForeignSQL;
end;

constructor TfrmNewForeign.Create(AOwner: TComponent);
var
  xIndex: Integer;
begin
  inherited;
  cbxOnDelete.Items.Clear;
  cbxOnUpdate.Items.Clear;
  for xIndex := Ord(low(TForeignOption)) to Ord(high(TForeignOption)) do
  begin
    cbxOnDelete.Items.Add(TForeignOption(xIndex).ToDescription);
    cbxOnUpdate.Items.Add(TForeignOption(xIndex).ToDescription);
  end;
  InitializeOptions;
end;

procedure TfrmNewForeign.DefineForeignSQL;
var
  lForeignSQL: string;
begin
  lForeignSQL := Format('FK_TABLE_%s foreign key (%s) references %s (%s)',
      [cbxReferencesTo.Text, cbxColumnNames.Text, cbxReferencesTo.Text,
      ifthen(cbxReferenceColumns.Text <> '', cbxReferenceColumns.Text, cbxColumnNames.Text)]);
  if (cbxOnUpdate.ItemIndex > 0) then
  begin
    lForeignSQL := lForeignSQL + ' on update ' + TForeignOption(cbxOnUpdate.ItemIndex).ToString;
  end;
  if (cbxOnDelete.ItemIndex > 0) then
  begin
    lForeignSQL := lForeignSQL + ' on delete ' + TForeignOption(cbxOnDelete.ItemIndex).ToString;
  end;
  mmoSQL.Text := lForeignSQL;
end;

procedure TfrmNewForeign.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  if (ModalResult = mrOk) then
  begin
    GetValuesFromComponents;
  end;
end;

function TfrmNewForeign.GetTextFromList(pList: TList<String>): String;
var
  lColumn: String;
begin
  Result := '';
  for lColumn in pList do
  begin
    Result := Result + ifthen(Result <> '', ';') + lColumn;
  end;
end;

procedure TfrmNewForeign.GetValuesFromComponents;
begin
  FForeign.Columns.Clear;
  FForeign.ReferencesColumns.Clear;
  FForeign.Schemas.Clear;
  AddTextToList(cbxColumnNames.Text, FForeign.Columns);
  AddTextToList(cbxReferenceColumns.Text, FForeign.ReferencesColumns);
  FForeign.ReferencesTo := cbxReferencesTo.Text;
  FForeign.OnUpdate := TForeignOption(cbxOnUpdate.ItemIndex);
  FForeign.OnDelete := TForeignOption(cbxOnDelete.ItemIndex);
  AddTextToList(cbxSchemas.Text, FForeign.Schemas);
end;

procedure TfrmNewForeign.InitializeOptions;
begin
  cbxColumnNames.CharCase := TEditCharCase(IniFile.ReadInteger('Options', 'MappingCharCase', 1));
  cbxReferenceColumns.CharCase := cbxColumnNames.CharCase;
  cbxReferencesTo.CharCase := cbxColumnNames.CharCase;
  cbxSchemas.CharCase := TEditCharCase(IniFile.ReadInteger('Options', 'SchemaCharCase', 2));
end;

procedure TfrmNewForeign.SendValuesToComponents;
begin
  cbxColumnNames.Text := GetTextFromList(FForeign.Columns);
  cbxReferenceColumns.Text := GetTextFromList(FForeign.ReferencesColumns);
  cbxReferencesTo.Text := FForeign.ReferencesTo;
  cbxOnUpdate.ItemIndex := Ord(FForeign.OnUpdate);
  cbxOnDelete.ItemIndex := Ord(FForeign.OnDelete);
  cbxSchemas.Text := GetTextFromList(FForeign.Schemas);
end;

procedure TfrmNewForeign.SetForeign(const Value: TForeignMapper);
begin
  FForeign := Value;
  SendValuesToComponents;
  DefineForeignSQL;
end;

function TfrmNewForeign.ValidateForeign: Boolean;
begin
  Result := False;
  if (Trim(cbxColumnNames.Text) = '') then
  begin
    ShowBalloHintForControl(cbxColumnNames, 'The column names is required');
  end
  else if (Trim(cbxReferencesTo.Text) = '') then
  begin
    ShowBalloHintForControl(cbxReferencesTo, 'The Reference name is required');
  end
  else
  begin
    Result := True;
  end;
end;

end.
