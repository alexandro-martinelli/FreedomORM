unit AM.Freedom.frmFieldTypeFromPropertyTypeName;

interface

uses
  ToolsApi,
  Data.DB,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.IniFiles,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  AM.Freedom.frmBase,
  Vcl.ImgList,
  Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TfrmFieldTypeFromPropertyTypeName = class(TfrmBase)
    lblUnitName: TLabel;
    lblExplain: TLabel;
    lblExplain2: TLabel;
    cbxFieldTypeNames: TComboBox;
    FlowPanel1: TFlowPanel;
    btnCancel: TButton;
    btnFinish: TButton;
  private
    FPropertyTypeName: string;
    class var FHistoryPropertyFieldType: TDictionary<String, TFieldType>;
    class procedure InitializePropertyHistory;
    class procedure FinalizePropertyHistory;
    procedure SetProperties(pPropertyName, pPropertyTypeName: String);
    function GetFieldType: TFieldType;
  public
    class function ExtractFieldType(pPropertyName, pPropertyTypeName: String): TFieldType;
    property FieldType: TFieldType read GetFieldType;
  end;

implementation

uses
  AM.Freedom.dclFreedomORMConfig;

{$R *.dfm}

{ TfrmFieldTypeFromPropertyTypeName }

class function TfrmFieldTypeFromPropertyTypeName.ExtractFieldType(pPropertyName, pPropertyTypeName: String): TFieldType;
var
  lfrmFieldTypeFromPropertyTypeName: TfrmFieldTypeFromPropertyTypeName;
begin
  InitializePropertyHistory;
  try
    if (FHistoryPropertyFieldType.ContainsKey(pPropertyTypeName)) then
    begin
      Result := FHistoryPropertyFieldType.Items[pPropertyTypeName];
    end
    else
    begin
      lfrmFieldTypeFromPropertyTypeName := TfrmFieldTypeFromPropertyTypeName.Create(nil);
      Result := ftUnknown;
      try
        lfrmFieldTypeFromPropertyTypeName.SetProperties(pPropertyName, pPropertyTypeName);
        if lfrmFieldTypeFromPropertyTypeName.ShowModal = mrOK then
        begin
          Result := lfrmFieldTypeFromPropertyTypeName.FieldType;
        end;


      finally
        lfrmFieldTypeFromPropertyTypeName.Free;
      end;
    end;
  finally
    FinalizePropertyHistory;
  end;
end;

class procedure TfrmFieldTypeFromPropertyTypeName.FinalizePropertyHistory;
var
  {$IFNDEF TEST}
  lCurrentProject: IOTAProject;
  {$IFEND}
  lIndex: Integer;
  lIniFile: TIniFile;
  function FieldTypeFromFieldTypeName(pFieldTypeName: String): TFieldType;
  var
    lFieldIndex: Integer;
  begin
    Result := ftUnknown;
    for lFieldIndex := Ord(Low(TFieldType)) to Ord(High(TFieldType)) do
    begin
      if SameText(FieldTypeNames[TFieldType(lFieldIndex)], pFieldTypeName) then
      begin
        Result := TFieldType(lFieldIndex);
        Break;
      end;
    end;
  end;
begin
  {$IFDEF TEST}
  lIniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'FreedomORM.cfg');
  {$ELSE}
  lCurrentProject := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
  lIniFile := TIniFile.Create(TdclFreedomORMConfig.GetInstance.IniFileDirectory + 'FreedomORM.cfg');
  {$IFEND}
  for lIndex := 0 to FHistoryPropertyFieldType.Count - 1 do
  begin
    lIniFile.WriteString('PropertyFieldTypeNames', FHistoryPropertyFieldType.Keys.ToArray[lIndex],
      FieldTypeNames[FHistoryPropertyFieldType.Values.ToArray[lIndex]]);
  end;
  FHistoryPropertyFieldType.Free;
end;

function TfrmFieldTypeFromPropertyTypeName.GetFieldType: TFieldType;
begin
  Result := TFieldType(cbxFieldTypeNames.Items.Objects[cbxFieldTypeNames.ItemIndex]);
  if (Result <> ftUnknown) then
  begin
    FHistoryPropertyFieldType.AddOrSetValue(FPropertyTypeName, Result);
  end;
end;

class procedure TfrmFieldTypeFromPropertyTypeName.InitializePropertyHistory;
var
  {$IFNDEF TEST}
  lCurrentProject: IOTAProject;
  {$IFEND}
  lPropertyTypeNames: TStrings;
  lIndex: Integer;
  lIniFile: TIniFile;
  function FieldTypeFromFieldTypeName(pFieldTypeName: String): TFieldType;
  var
    lFieldIndex: Integer;
  begin
    Result := ftUnknown;
    for lFieldIndex := Ord(Low(TFieldType)) to Ord(High(TFieldType)) do
    begin
      if SameText(FieldTypeNames[TFieldType(lFieldIndex)], pFieldTypeName) then
      begin
        Result := TFieldType(lFieldIndex);
        Break;
      end;
    end;
  end;
begin
  {$IFDEF TEST}
  lIniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'FreedomORM.cfg');
  {$ELSE}
  lCurrentProject := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
  lIniFile := TIniFile.Create(TdclFreedomORMConfig.GetInstance.IniFileDirectory + 'FreedomORM.cfg');
  {$IFEND}
  FHistoryPropertyFieldType := TDictionary<String, TFieldType>.Create;
  lPropertyTypeNames := TStringList.Create;
  try
    lIniFile.ReadSectionValues('PropertyFieldTypeNames', lPropertyTypeNames);
    for lIndex := 0 to lPropertyTypeNames.Count - 1 do
    begin
      FHistoryPropertyFieldType.Add(lPropertyTypeNames.Names[lIndex],
          FieldTypeFromFieldTypeName(lPropertyTypeNames.ValueFromIndex[lIndex]));
    end;
  finally
    lPropertyTypeNames.Free;
  end;
end;

procedure TfrmFieldTypeFromPropertyTypeName.SetProperties(pPropertyName, pPropertyTypeName: String);
var
  lIndex: Integer;
begin
  FPropertyTypeName := pPropertyTypeName;
  lblExplain.Caption := Format(lblExplain.Caption, [pPropertyName, pPropertyTypeName]);
  cbxFieldTypeNames.Items.Clear;
  for lIndex := Ord(Low(TFieldType)) to Ord(High(TFieldType)) do
  begin
    cbxFieldTypeNames.Items.AddObject(FieldTypeNames[TFieldType(lIndex)], Pointer(TFieldType(lIndex)));
  end;
  cbxFieldTypeNames.ItemIndex := 0;
end;

end.
