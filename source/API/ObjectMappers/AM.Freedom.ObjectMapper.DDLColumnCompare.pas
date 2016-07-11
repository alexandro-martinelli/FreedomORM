unit AM.Freedom.ObjectMapper.DDLColumnCompare;

interface

uses
  AM.Freedom.ObjectMapper.DDLObjects,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.FieldCommandFactory,
  AM.Freedom.SQLCommands.Fields,
  AM.Freedom.SQLCommands.SizedFieldCommand,
  AM.Freedom.SQLCommands.ScaledFieldCommand,
  AM.Freedom.IPersistent,
  AM.Freedom.SQLMappers.ISQLMapper;

type
  TDDLColumnCompareParams = class sealed
  private
    FDataBaseColumn: TDDLColumn;
    FObjectColumn: TDDLColumn;
    FPersistent: IPersistent;
    FAlwaysAlterDataTypeOnChangeProperties: Boolean;
    FAlterFieldCommandParams: TAlterFieldCommandParams;
    FSQLMapper: ISQLMapper;
  public
    property ObjectColumn: TDDLColumn read FObjectColumn write FObjectColumn;
    property DataBaseColumn: TDDLColumn read FDataBaseColumn write FDataBaseColumn;
    property Persistent: IPersistent read FPersistent write FPersistent;
    property AlwaysAlterDataTypeOnChangeProperties: Boolean read FAlwaysAlterDataTypeOnChangeProperties write FAlwaysAlterDataTypeOnChangeProperties;
    property AlterFieldCommandParams: TAlterFieldCommandParams read FAlterFieldCommandParams write FAlterFieldCommandParams;
    property SQLMapper: ISQLMapper read FSQLMapper write FSQLMapper;
  end;

  TDDLColumnCompareResult = class sealed
  private
    FFieldCommand: TCustomFieldCommand;
    FChangedProperties: TChangedProperties;
  public
    property FieldCommand: TCustomFieldCommand read FFieldCommand write FFieldCommand;
    property ChangedProperties: TChangedProperties read FChangedProperties write FChangedProperties;
  end;

  TDDLColumnCompare = class sealed
  strict private
    FDDLColumnCompareParams: TDDLColumnCompareParams;
    function DoCompare: TDDLColumnCompareResult;
    function CreateWithDomains: TCustomFieldCommand;
    function HasDomains: Boolean;
    function IsEqualsDomains: Boolean;
    function IsDifferentDomains: Boolean;
    function CreateWithColumnTypes: TCustomFieldCommand;
    function IsDifferentColumnTypes: Boolean;
    function CreateWithSizeOrScale: TCustomFieldCommand;
    function IsDifferentDataSize: Boolean;
    function CreateWithDifferentNullable: TCustomFieldCommand;
    function IsDifferentNullable: Boolean;
    function DoCreateField: TCustomFieldCommand;
    procedure DoWriteChangedProperties(pCompareResult: TDDLColumnCompareResult);
    procedure DoWriteDomain(pCompareResult: TDDLColumnCompareResult);
  public
    class function CompareColumns(pCompareParams: TDDLColumnCompareParams): TDDLColumnCompareResult;
    destructor Destroy; override;
  end;

implementation

uses
  System.SysUtils;

{ TDDLColumnCompare }

class function TDDLColumnCompare.CompareColumns(pCompareParams: TDDLColumnCompareParams): TDDLColumnCompareResult;
var
  lDDLColumnCompare: TDDLColumnCompare;
begin
  lDDLColumnCompare := TDDLColumnCompare.Create;
  try
    lDDLColumnCompare.FDDLColumnCompareParams := pCompareParams;
    Result := lDDLColumnCompare.DoCompare;
  finally
    lDDLColumnCompare.Free;
  end;
end;

function TDDLColumnCompare.CreateWithColumnTypes: TCustomFieldCommand;
begin
  if IsDifferentColumnTypes then
  begin
    Result := DoCreateField;
  end
  else
  begin
    Result := CreateWithSizeOrScale;
  end;
end;

function TDDLColumnCompare.CreateWithDifferentNullable: TCustomFieldCommand;
begin
  Result := nil;
  if IsDifferentNullable and not (DontChangeNullable in FDDLColumnCompareParams.AlterFieldCommandParams) then
  begin
    Result := DoCreateField;
  end;
end;

function TDDLColumnCompare.CreateWithDomains: TCustomFieldCommand;
begin
  Result := nil;
  if not HasDomains or not IsEqualsDomains then
  begin
    if HasDomains then
    begin
      if IsDifferentDomains then
      begin
        Result := DoCreateField;
      end else
      begin
        Result := CreateWithColumnTypes;
      end;
    end
    else
    begin
      Result := CreateWithColumnTypes;
    end;
  end;
end;

function TDDLColumnCompare.CreateWithSizeOrScale: TCustomFieldCommand;
begin
  if IsDifferentDataSize then
  begin
    Result := DoCreateField;
  end
  else
  begin
    Result := CreateWithDifferentNullable;
  end;
end;

destructor TDDLColumnCompare.Destroy;
begin
  FDDLColumnCompareParams.Free;
  inherited;
end;

function TDDLColumnCompare.DoCompare: TDDLColumnCompareResult;
begin
  Result := TDDLColumnCompareResult.Create;
  Result.FieldCommand := CreateWithDomains;
  DoWriteChangedProperties(Result);
  DoWriteDomain(Result);
  if (Result.FieldCommand <> nil) and (Result.ChangedProperties = []) then
  begin
    Result.FieldCommand.Free;
    Result.FieldCommand := nil;
  end;
end;

function TDDLColumnCompare.DoCreateField: TCustomFieldCommand;
var
  lParams: TCreateFieldCommandParams;
begin
  lParams := TCreateFieldCommandParams.Create;
  try
    lParams.Column := FDDLColumnCompareParams.ObjectColumn;
    LParams.SQLMapper := FDDLColumnCompareParams.SQLMapper;
    Result := TFieldCommandFactory.CreateFieldCommandFromDDLColumn(lParams);
  finally
    lParams.Free;
  end;
end;

procedure TDDLColumnCompare.DoWriteChangedProperties(pCompareResult: TDDLColumnCompareResult);
begin
  if Assigned(pCompareResult.FieldCommand) then
  begin
    pCompareResult.ChangedProperties := [];
    if IsDifferentDataSize then
    begin
      pCompareResult.ChangedProperties := pCompareResult.ChangedProperties + [DataSize];
    end;
    if IsDifferentNullable then
    begin
      pCompareResult.ChangedProperties := pCompareResult.ChangedProperties + [Nullable];
    end;
    if IsDifferentColumnTypes or ((pCompareResult.ChangedProperties <> []) and
        FDDLColumnCompareParams.AlwaysAlterDataTypeOnChangeProperties) then
    begin
      pCompareResult.ChangedProperties := pCompareResult.ChangedProperties + [DataType];
    end;
  end;
end;

procedure TDDLColumnCompare.DoWriteDomain(pCompareResult: TDDLColumnCompareResult);
begin
  if Assigned(pCompareResult.FieldCommand) and
     ((FDDLColumnCompareParams.DataBaseColumn.Domain <> '') and (FDDLColumnCompareParams.ObjectColumn.Domain = '')) then
  begin
    pCompareResult.FieldCommand.Domain := FDDLColumnCompareParams.DataBaseColumn.Domain;
  end;
end;

function TDDLColumnCompare.HasDomains: Boolean;
begin
  Result := (FDDLColumnCompareParams.ObjectColumn.Domain <> '') or (FDDLColumnCompareParams.DataBaseColumn.Domain <> '');
end;

function TDDLColumnCompare.IsDifferentColumnTypes: Boolean;
begin
  Result := not FDDLColumnCompareParams.Persistent.CompareDDLColumnTypes(FDDLColumnCompareParams.ObjectColumn.ColumnType,
         FDDLColumnCompareParams.DataBaseColumn.ColumnType)
end;

function TDDLColumnCompare.IsDifferentDataSize: Boolean;
begin
  Result := (FDDLColumnCompareParams.ObjectColumn.Size > FDDLColumnCompareParams.DataBaseColumn.Size) or
            (FDDLColumnCompareParams.ObjectColumn.Scale > FDDLColumnCompareParams.DataBaseColumn.Scale);
end;

function TDDLColumnCompare.IsDifferentDomains: Boolean;
begin
  Result := FDDLColumnCompareParams.ObjectColumn.Domain <> '';
  if Result then
  begin
    Result := not IsEqualsDomains;
  end;
end;

function TDDLColumnCompare.IsDifferentNullable: Boolean;
begin
  Result := (Required in FDDLColumnCompareParams.ObjectColumn.ColumnOptions) <>
            (Required in FDDLColumnCompareParams.DataBaseColumn.ColumnOptions);
end;

function TDDLColumnCompare.IsEqualsDomains: Boolean;
begin
  Result := SameText(FDDLColumnCompareParams.ObjectColumn.Domain, FDDLColumnCompareParams.DataBaseColumn.Domain)
end;

end.

