unit AM.Freedom.ObjectMapper.DDLCompare;

interface

uses
  System.Generics.Collections,
  AM.Freedom.ObjectMapper.DDLObjects,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLCommands.TableCommands,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.FieldCommandFactory,
  AM.Freedom.ObjectMapper.DDLColumnCompare,
  AM.Freedom.SQLCommands.Constraints,
  AM.Freedom.SQLCommands.SequenceCommands,
  AM.Freedom.IPersistent,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.SQLMappers.IDDLExtracter, AM.Freedom.SQLCommands.SchemaCommands;

type
  TDDLCompareParams = class sealed
  private
    FDataBaseDDL: TDDLEntity;
    FObjectDDL: TDDLEntity;
    FGetFieldCommandClass: TGetFieldCommandClass;
    FPersistent: IPersistent;
    FSQLMapper: ISQLMapper;
    FDDLExtracter: IDDLExtracter;
  public
    function CurrentSchema: String;
    property ObjectDDL: TDDLEntity read FObjectDDL write FObjectDDL;
    property DataBaseDDL: TDDLEntity read FDataBaseDDL write FDataBaseDDL;
    property GetFieldCommandClass: TGetFieldCommandClass read FGetFieldCommandClass write FGetFieldCommandClass;
    property Persistent: IPersistent read FPersistent write FPersistent;
    property SQLMapper: ISQLMapper read FSQLMapper write FSQLMapper;
    property DDLExtracter: IDDLExtracter read FDDLExtracter write FDDLExtracter;
  end;

  TDDLCompare = class
  strict private
    FDDLCompareParams: TDDLCompareParams;
    FAlterTableCommand: TAlterTableCommand;
    FCreateTableCommand: TCreateTableCommand;
    FDomainNames: String;
    function DoCompare: TCommandList;
    function CreateFieldFromDDLColumn(pColumn: TDDLColumn; pSchema: String): TCustomFieldCommand;
    function GenerateCreateTableCommand: TCommandList;
    procedure AddFields;
    procedure AddConstraints;
    function GetCreateSequenceCommand: TCreateSequenceCommand;
    function GenerateAlterTableCommand: TCommandList;
    procedure GetCommands;
    procedure GetDropCommands;
    procedure GetAddOrAlterCommands;
    procedure GetAddOrAlterFields;
    function GenerateAlterFieldCommand(pParams: TDDLColumnCompareParams): TDDLColumnCompareResult;
    procedure GetDropFields;
    procedure GetAddOrAlterConstraints;
    function IsEqualsConstraints(pObjectConstraint, pDataBaseConstraint: TCustomConstraint): Boolean;
    procedure GetDropConstraints;
    function GetSequenceCommand: TCreateSequenceCommand;
    procedure VerifyDomains(pCommandList: TCommandList);
    function AlreadInDomainCreateList(pDomain: String): Boolean;
    procedure VerifySchema(pCommandList: TCommandList);
  public
    class function CompareDDLS(pDDLCompareParams: TDDLCompareParams): TCommandList;
    destructor Destroy; override;
  end;

implementation

{ TDDLCompare }

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.ObjectMapper.DDLConstraintCompare,
  AM.Freedom.SQLCommands.DomainCommands,
  AM.Freedom.SQLCommands.FieldOptions,
  AM.Freedom.SQLCommands.CustomTableCommand;

procedure TDDLCompare.AddConstraints;
var
  lConstraint: TCustomConstraint;
begin
  for lConstraint in FDDLCompareParams.ObjectDDL.Constraints do
  begin
    case lConstraint.ConstraintType of
      PrimaryKey:
        FCreateTableCommand.AddPrimaryKey(TPrimaryKey(lConstraint.CreateNew));
      ForeignKey:
        FCreateTableCommand.AddForeignKey(TForeignKey(lConstraint.CreateNew));
      UniqueKey:
        FCreateTableCommand.AddUniqueKey(TUniqueKey(lConstraint.CreateNew));
    end;
  end;
end;

procedure TDDLCompare.AddFields;
var
  lColumn: TDDLColumn;
  lField: TCustomFieldCommand;
begin
  for lColumn in FDDLCompareParams.ObjectDDL.Columns do
  begin
    lField := CreateFieldFromDDLColumn(lColumn, FDDLCompareParams.ObjectDDL.Schema);
    if Assigned(lField) then
    begin
      FCreateTableCommand.AddField(lField);
    end;
  end;
end;

function TDDLCompare.AlreadInDomainCreateList(pDomain: String): Boolean;
begin
  Result := ContainsText(FDomainNames, pDomain);
  if (not Result) then
  begin
    FDomainNames := FDomainNames + ifthen(FDomainNames <> '', ';') + pDomain;
  end;
end;

function TDDLCompare.IsEqualsConstraints(pObjectConstraint, pDataBaseConstraint: TCustomConstraint): Boolean;
begin
  Result := TDDLConstraintCompare.IsEqualsContraint(pObjectConstraint, pDataBaseConstraint);
end;

procedure TDDLCompare.VerifyDomains(pCommandList: TCommandList);
var
  lDomains: TDDLDomains;
  lDDLColumn: TDDLColumn;
  lDomainOptions: TDomainOptions;
  lCreateDomain: TCreateDomainCommand;
begin
  FDomainNames := '';
  lDomains := FDDLCompareParams.DDLExtracter.ExtractDDLDomains(FDDLCompareParams.CurrentSchema);
  try
    for lDDLColumn in FDDLCompareParams.ObjectDDL.Columns do
    begin
      if lDDLColumn.Domain <> '' then
      begin
        if (lDomains.FindDomain(lDDLColumn.Domain) = nil) and (not AlreadInDomainCreateList(lDDLColumn.Domain)) then
        begin
          lDomainOptions := TDomainOptions.Create;
          if (Required in lDDLColumn.ColumnOptions) then
          begin
            lDomainOptions.Nullable := nNotNull;
          end;
          lCreateDomain := TCreateDomainCommand.Create(lDDLColumn.Domain, lDomainOptions,
            lDDLColumn.ColumnType, lDDLColumn.Size, lDDLColumn.Scale, FDDLCompareParams.CurrentSchema);
          pCommandList.Insert(0, lCreateDomain);
        end;
      end;
    end;
  finally
    lDomains.Free;
    FDomainNames := '';
  end;
end;

procedure TDDLCompare.VerifySchema(pCommandList: TCommandList);
var
  lSchemas: TList<string>;
  lCreateSchema: TCreateSchemaCommand;
begin
  if FDDLCompareParams.ObjectDDL.Schema <> '' then
  begin
    lSchemas := FDDLCompareParams.DDLExtracter.ExtractDDLSchemas;
    try
      if not lSchemas.Contains(LowerCase(FDDLCompareParams.ObjectDDL.Schema)) then
      begin
        lCreateSchema := TCreateSchemaCommand.Create(FDDLCompareParams.ObjectDDL.Schema);
        pCommandList.Insert(0, lCreateSchema);
      end;
    finally
      lSchemas.Free;
    end;
  end;
end;

class function TDDLCompare.CompareDDLS(pDDLCompareParams: TDDLCompareParams): TCommandList;
var
  lCompare: TDDLCompare;
begin
  lCompare := TDDLCompare.Create;
  try
    lCompare.FDDLCompareParams := pDDLCompareParams;
    Result := lCompare.DoCompare;
  finally
    lCompare.Free;
  end;
end;

function TDDLCompare.CreateFieldFromDDLColumn(pColumn: TDDLColumn; pSchema: String): TCustomFieldCommand;
var
  lParams: TCreateFieldCommandParams;
begin
  lParams := TCreateFieldCommandParams.Create;
  try
    lParams.Column := pColumn;
    lParams.Schema := pSchema;
    lParams.SQLMapper := FDDLCompareParams.SQLMapper;
    Result := TFieldCommandFactory.CreateFieldCommandFromDDLColumn(lParams);
  finally
    lParams.Free;
  end;
end;

destructor TDDLCompare.Destroy;
begin
  FDDLCompareParams.Free;;
  FCreateTableCommand := nil;
  FAlterTableCommand := nil;
  inherited;
end;

function TDDLCompare.DoCompare: TCommandList;
begin
  if not Assigned(FDDLCompareParams.DataBaseDDL) then
  begin
    Result := GenerateCreateTableCommand;
  end
  else
  begin
    Result := GenerateAlterTableCommand;
  end;
  if Assigned(Result) then
  begin
    VerifyDomains(Result);
  end;
  if Assigned(Result)  then
  begin
    VerifySchema(Result);
  end;
end;

function TDDLCompare.GenerateAlterFieldCommand(pParams: TDDLColumnCompareParams): TDDLColumnCompareResult;
begin
  pParams.SQLMapper := FDDLCompareParams.SQLMapper;
  Result := TDDLColumnCompare.CompareColumns(pParams);
end;

function TDDLCompare.GenerateAlterTableCommand: TCommandList;
var
  lSequence: TCustomCommand;
begin
  FAlterTableCommand := TAlterTableCommand.Create;
  FAlterTableCommand.Name := FDDLCompareParams.ObjectDDL.Name;
  FAlterTableCommand.Schema := FDDLCompareParams.ObjectDDL.Schema;
  GetCommands;
  if (FAlterTableCommand.Commands.Count <= 0) then
  begin
    FAlterTableCommand.Free;
    FAlterTableCommand := nil;
  end;
  Result := TCommandList.Create;
  if Assigned(FAlterTableCommand) then
  begin
    Result.Add(FAlterTableCommand);
  end;
  lSequence := GetSequenceCommand;
  if Assigned(lSequence) then
  begin
    Result.Add(lSequence);
  end;
end;

function TDDLCompare.GenerateCreateTableCommand: TCommandList;
var
  lSequence: TCustomCommand;
begin
  FCreateTableCommand := TCreateTableCommand.Create;
  FCreateTableCommand.Name := FDDLCompareParams.ObjectDDL.Name;
  FCreateTableCommand.Schema := FDDLCompareParams.ObjectDDL.Schema;
  AddFields;
  AddConstraints;
  Result := TCommandList.Create;
  Result.Add(FCreateTableCommand);
  lSequence := GetCreateSequenceCommand;
  if Assigned(lSequence) then
  begin
    Result.Add(lSequence);
  end;
end;

procedure TDDLCompare.GetAddOrAlterCommands;
begin
  GetAddOrAlterFields;
  GetAddOrAlterConstraints;
end;

procedure TDDLCompare.GetAddOrAlterConstraints;
var
  lObjectConstraint: TCustomConstraint;
  lDataBaseConstraint: TCustomConstraint;
begin
  for lObjectConstraint in FDDLCompareParams.ObjectDDL.Constraints do
  begin
    lDataBaseConstraint := FDDLCompareParams.DataBaseDDL.Constraints.FindConstraint(lObjectConstraint.Name);
    if not Assigned(lDataBaseConstraint) then
    begin
      FAlterTableCommand.AddConstraint(lObjectConstraint.CreateNew);
    end
    else
    begin
      if not IsEqualsConstraints(lObjectConstraint, lDataBaseConstraint) then
      begin
        FAlterTableCommand.DropConstraint(lDataBaseConstraint.Name);
        FAlterTableCommand.AddConstraint(lObjectConstraint.CreateNew);
      end;
    end;
  end;
end;

procedure TDDLCompare.GetAddOrAlterFields;
var
  lObjectColumn: TDDLColumn;
  lDataBaseColumn: TDDLColumn;
  lParams: TDDLColumnCompareParams;
  lCompareResult: TDDLColumnCompareResult;
  lField: TCustomFieldCommand;
begin
  for lObjectColumn in FDDLCompareParams.ObjectDDL.Columns do
  begin
    lDataBaseColumn := FDDLCompareParams.DataBaseDDL.Columns.FindColumn(lObjectColumn.Name, lObjectColumn.ColumnType);
    if (not Assigned(lDataBaseColumn)) then
    begin
      lDataBaseColumn := FDDLCompareParams.DataBaseDDL.Columns.FindColumn(lObjectColumn.Name, ctyUnknow, [ctyJoin, ctyDetail, ctyExtension]);
      if (Assigned(lDataBaseColumn)) then
      begin
        if (not FDDLCompareParams.SQLMapper.AllowedColumnTypeConversion(lDataBaseColumn.ColumnType, lObjectColumn.ColumnType)) then
        begin
          lDataBaseColumn := nil;
        end;
      end;
    end;
    if not Assigned(lDataBaseColumn) then
    begin
      lField := CreateFieldFromDDLColumn(lObjectColumn, FDDLCompareParams.ObjectDDL.Schema);
      if Assigned(lField) then
      begin
        FAlterTableCommand.AddField(lField);
      end;
    end
    else
    begin
      lParams := TDDLColumnCompareParams.Create;
      lParams.ObjectColumn := lObjectColumn;
      lParams.DataBaseColumn := lDataBaseColumn;
      lParams.Persistent := FDDLCompareParams.Persistent;
      lParams.AlwaysAlterDataTypeOnChangeProperties := FDDLCompareParams.SQLMapper.AlwaysAlterDataTypeOnChangeProperties;
      lParams.AlterFieldCommandParams := FDDLCompareParams.SQLMapper.AlterFieldCommandParams;
      lParams.SQLMapper := FDDLCompareParams.SQLMapper;
      lCompareResult := GenerateAlterFieldCommand(lParams);
      try
        if Assigned(lCompareResult.FieldCommand) then
        begin
          FAlterTableCommand.AlterField(lCompareResult.FieldCommand, lCompareResult.ChangedProperties);
        end;
      finally
        lCompareResult.Free;
      end;
    end;
  end;
end;

procedure TDDLCompare.GetCommands;
begin
  GetDropCommands;
  GetAddOrAlterCommands;
end;

function TDDLCompare.GetCreateSequenceCommand: TCreateSequenceCommand;
begin
  Result := nil;
  if FDDLCompareParams.ObjectDDL.Columns.IDCOlumn.IdOptions.IsValidSequence(FDDLCompareParams.ObjectDDL.Schema) then
  begin
    Result := TCreateSequenceCommand.Create(FDDLCompareParams.ObjectDDL.Columns.IDCOlumn.IdOptions.SequenceName,
        FDDLCompareParams.ObjectDDL.Schema);
  end;
end;

procedure TDDLCompare.GetDropCommands;
begin
  GetDropConstraints;
  GetDropFields;
end;

procedure TDDLCompare.GetDropConstraints;
var
  lDataBaseConstraint, lObjectConstraint: TCustomConstraint;
begin
  for lDataBaseConstraint in FDDLCompareParams.DataBaseDDL.Constraints do
  begin
    lObjectConstraint := FDDLCompareParams.ObjectDDL.Constraints.FindConstraint(lDataBaseConstraint.Name);
    if not Assigned(lObjectConstraint) then
    begin
      FAlterTableCommand.DropConstraint(lDataBaseConstraint.Name);
    end;
  end;
end;

procedure TDDLCompare.GetDropFields;
var
  lObjectColumn: TDDLColumn;
  lDataBaseColumn: TDDLColumn;
  lCommands: TList<TCustomTableCommand>;
  lCommand: TCustomTableCommand;
begin
  for lDataBaseColumn in FDDLCompareParams.DataBaseDDL.Columns do
  begin
    lObjectColumn := FDDLCompareParams.ObjectDDL.Columns.FindColumn(lDataBaseColumn.Name, lDataBaseColumn.ColumnType);
    if (not Assigned(lObjectColumn)) then
    begin
      lObjectColumn := FDDLCompareParams.ObjectDDL.Columns.FindColumn(lDataBaseColumn.Name, ctyUnknow, [ctyJoin, ctyDetail, ctyExtension]);
      if (Assigned(lObjectColumn)) then
      begin
        if (not FDDLCompareParams.SQLMapper.AllowedColumnTypeConversion(lDataBaseColumn.ColumnType, lObjectColumn.ColumnType)) then
        begin
          lObjectColumn := nil;
        end;
      end;
    end;
    if not Assigned(lObjectColumn) then
    begin
      lCommands := FDDLCompareParams.DDLExtracter.VerifyDropField(FDDLCompareParams.ObjectDDL.Name, lDataBaseColumn.Name);
      if (Assigned(lCommands)) then
      begin
        for lCommand in lCommands do
        begin
          FAlterTableCommand.Commands.Add(lCommand);
        end;
        lCommands.Free;
      end;
      FAlterTableCommand.DropField(lDataBaseColumn.Name);
    end;
  end;
end;

function TDDLCompare.GetSequenceCommand: TCreateSequenceCommand;
var
  lObjectIdColumn, lIdColumn: TDDLColumn;
begin
  Result := nil;
  lObjectIdColumn := FDDLCompareParams.ObjectDDL.Columns.IDColumn;
  if Assigned(lObjectIdColumn) then
  begin
    lIdColumn := FDDLCompareParams.DataBaseDDL.Columns.FindColumn(lObjectIdColumn.Name, lObjectIdCOlumn.ColumnType);
    if Assigned(lIdColumn) and (lIdColumn.IdOptions.IdOption = TIdOption.Sequence) and
       not SameText(lIdColumn.IdOptions.SequenceName, lObjectIdColumn.IdOptions.SequenceName) then
    begin
      Result := GetCreateSequenceCommand;
    end;
  end;
end;

{ TDDLCompareParams }

function TDDLCompareParams.CurrentSchema: String;
begin
  Result := FObjectDDL.Schema;
end;

end.
