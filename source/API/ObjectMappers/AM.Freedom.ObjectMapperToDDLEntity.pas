unit AM.Freedom.ObjectMapperToDDLEntity;

interface

uses
  AM.Freedom.ObjectMapper,
  AM.Freedom.ObjectMapper.DDLObjects,
  AM.Freedom.ObjectMapper.CustomColumnMapper,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.IDBPersistent;

type
  TObjectMapperToDDLEntity = class
  strict private
    FObjectMapper: TObjectMapper;
    FDDLEntity: TDDLEntity;
    FColumn: TCustomColumnMapper;
    FPersistent: IDBPersistent;
    function DoExtractDDL: TDDLEntity;
    procedure AddColumn;
    function GetColumnTypeFromColumnMapper: TColumnType;
    procedure ExtractDefaultValue(pColumn: TDDLColumn);
    procedure AddForeignConstraint;
    procedure AddPrimaryConstraint;
    procedure AddUniqueConstraints;
    procedure AddForeignConstraints;
    procedure FixConstraintNames;
  public
    class function ExtractDDL(pObjectMapper: TObjectMapper; pPersistent: IDBPersistent): TDDLEntity;
  end;

implementation

uses
  AM.Freedom.Helper.ColumnType,
  AM.Freedom.ObjectMapper.ColumnMappers,
  AM.Freedom.SQLCommands.Constraints,
  System.SysUtils,
  AM.Freedom.ObjectMapper.ConstraintMapper,
  AM.Freedom.Helper.Variant;

{ TObjectMapperToDDLEntity }

procedure TObjectMapperToDDLEntity.AddColumn;
var
  lColumn: TDDLColumn;
  lColumnType: TColumnType;
  lCreatedColumn: Boolean;
begin
  if (FColumn.ColumnType in [ctyDetail, ctyJoin, ctyBoolean, ctyEnumerator]) then
  begin
    lColumnType := GetColumnTypeFromColumnMapper;
  end
  else
  begin
    lColumnType := FColumn.ColumnType;
  end;
  lColumn := FDDLEntity.Columns.FindColumn(FColumn.Name, lColumnType);
  lCreatedColumn := False;
  if not Assigned(lColumn) then
  begin
    lColumn := TDDLColumn.Create;
    lColumn.Name := FPersistent.AdjustNameLength(FColumn.Name);
    lCreatedColumn := True;
    FDDLEntity.Columns.Add(lColumn);
  end;
  if (not (FColumn.ColumnType in [ctyDetail, ctyJoin])) or (lCreatedColumn) then
  begin
    lColumn.ColumnOptions := FColumn.ColumnOptions;
    if (FObjectMapper.CurrentSchema = '') or (FColumn.IdOptions.Schemas.FindSchema(FObjectMapper.CurrentSchema) <> nil) then
    begin
      lColumn.IdOptions.Assign(FColumn.IdOptions);
    end;
    if (FColumn.IdOptions.IsId) and (not (Required in FColumn.ColumnOptions)) then
    begin
      lColumn.ColumnOptions := lColumn.ColumnOptions + [Required];
    end;
    ExtractDefaultValue(lColumn);
    lColumn.Size := FColumn.Size;
    lColumn.Scale := FColumn.Scale;
    lColumn.Domain := FColumn.Domain;
  end;
  if lColumn.ColumnType = ctyUnknow then
  begin
    lColumn.ColumnType := GetColumnTypeFromColumnMapper;
    if (lColumn.Size = 0) and (lColumn.ColumnType in [ctyChar, ctyString]) then
    begin
      lColumn.Size := 1;
    end;
  end;
  if FColumn.InheritsFrom(TReferenceColumnMapper) and FPersistent.MakeForeignConstraintsWithJoinColumn then
  begin
    AddForeignConstraint;
  end;
end;

procedure TObjectMapperToDDLEntity.AddForeignConstraint;
var
  lForeign: TForeignKey;
  lColumn: TJoinedColumnMapper;
  lForeignName: String;
begin
  if not FColumn.ColumnType.IsEquals(ctyDetail) then
  begin
    lColumn := TJoinedColumnMapper(FColumn);
    lForeignName := FPersistent.AdjustNameLength(Format('FK_%s_%s', [FObjectMapper.Name, lColumn.RefObjectName]));
    lForeign := TForeignKey(FDDLEntity.Constraints.FindConstraint(lForeignName));
    if not Assigned(lForeign) then
    begin
      lForeign := TForeignKey.Create;
      lForeign.Name := lForeignName;
      lForeign.AddField(lColumn.Name);
      lForeign.ReferencesTo(lColumn.RefObjectName).
          ReferencesFields([lColumn.RefColumnName]).
          Update(lColumn.UpdateAction).Delete(lColumn.DeleteAction);
      FDDLEntity.Constraints.Add(lForeign);
    end;
  end;
end;

procedure TObjectMapperToDDLEntity.AddForeignConstraints;
var
  lForeignKey: TForeignKey;
  lForeignName: string;
  lForeign: TForeignMapper;
  lColumnName: String;
  lField: String;
begin
  for lForeign in FObjectMapper.Foreigns do
  begin
    lForeignName := FPersistent.AdjustNameLength(Format(lForeign.Name, [FObjectMapper.Name]));
    for lColumnName in lForeign.Columns do
    begin
      lForeignKey := TForeignKey.Create;
      lForeignKey.Name := lForeignName;
      lForeignKey.AddField(lColumnName);
      lForeignKey.References := lForeign.ReferencesTo;
      lForeignKey.OnUpdate := lForeign.OnUpdate;
      lForeignKey.OnDelete := lForeign.OnDelete;
      for lField in lForeign.ReferencesColumns do
      begin
        lForeignKey.ReferenceFields.Add(lField);
      end;
      FDDLEntity.Constraints.Add(lForeignKey);
    end;
  end;
end;

procedure TObjectMapperToDDLEntity.AddPrimaryConstraint;
var
  lPrimary: TPrimaryKey;
  lPrimaryName: string;
  lColumnName: string;
  lPrimaryMapper: TPrimaryMapper;
begin
  if FObjectMapper.Primarys.Count > 0 then
  begin
    if FObjectMapper.CurrentSchema = '' then
    begin
      lPrimaryMapper := FObjectMapper.Primarys.Items[0];
    end
    else
    begin
      lPrimaryMapper := FObjectMapper.Primarys.FindSchema(FObjectMapper.CurrentSchema);
    end;
    if Assigned(lPrimaryMapper) then
    begin
      lPrimaryName := FPersistent.AdjustNameLength(Format(lPrimaryMapper.Name, [FObjectMapper.Name]));
      lPrimary := TPrimaryKey(FDDLEntity.Constraints.FindConstraint(lPrimaryName));
      for lColumnName in lPrimaryMapper.Columns do
      begin
        if not Assigned(lPrimary) then
        begin
          lPrimary := TPrimaryKey.Create;
          lPrimary.Name := lPrimaryName;
          lPrimary.AddField(lColumnName);
          FDDLEntity.Constraints.Add(lPrimary);
        end
        else if not lPrimary.Fields.FindField(lColumnName) then
        begin
          lPrimary.AddField(lColumnName);
        end;
      end;
    end;
  end;
end;

procedure TObjectMapperToDDLEntity.AddUniqueConstraints;
var
  lUniqueKey: TUniqueKey;
  lUniqueName: string;
  lUnique: TUniqueMapper;
  lColumnName: String;
begin
  for lUnique in FObjectMapper.Uniques do
  begin
    lUniqueName := FPersistent.AdjustNameLength(Format(lUnique.Name, [FObjectMapper.Name]));
    lUniqueKey := TUniqueKey(FDDLEntity.Constraints.FindConstraint(lUniqueName));
    for lColumnName in lUnique.Columns do
    begin
      if not Assigned(lUniqueKey) then
      begin
        lUniqueKey := TUniqueKey.Create;
        lUniqueKey.Name := lUniqueName;
        lUniqueKey.AddField(lColumnName);
        FDDLEntity.Constraints.Add(lUniqueKey);
      end
      else if not lUniqueKey.Fields.FindField(lColumnName) then
      begin
        lUniqueKey.AddField(lColumnName);
      end;
    end;
  end;
end;

function TObjectMapperToDDLEntity.DoExtractDDL: TDDLEntity;
var
  lColumn: TCustomColumnMapper;
begin
  FDDLEntity := TDDLEntity.Create;
  FDDLEntity.Name := FObjectMapper.Name;
  FDDLEntity.Schema := FObjectMapper.CurrentSchema;
  for lColumn in FObjectMapper.Columns do
  begin
    if (not lColumn.IsExtension) then
    begin
      FColumn := lColumn;
      AddColumn;
    end;
  end;
  AddPrimaryConstraint;
  AddUniqueConstraints;
  AddForeignConstraints;
  FixConstraintNames;
  Result := FDDLEntity;
end;

class function TObjectMapperToDDLEntity.ExtractDDL(pObjectMapper: TObjectMapper; pPersistent: IDBPersistent): TDDLEntity;
var
  lDDLObject: TObjectMapperToDDLEntity;
begin
  lDDLObject := TObjectMapperToDDLEntity.Create;
  try
    lDDLObject.FObjectMapper := pObjectMapper;
    lDDLObject.FPersistent := pPersistent;
    Result := lDDLObject.DoExtractDDL;
  finally
    lDDLObject.Free;
  end;
end;

procedure TObjectMapperToDDLEntity.ExtractDefaultValue(pColumn: TDDLColumn);
var
  lDefaultValue: Variant;
  lBoolean: Boolean;
begin
  pColumn.DefaultValueOptions.Assign(FColumn.DefaultValueOptions);
  if (not FColumn.DefaultValueOptions.IsNow) and (FColumn.DefaultValueOptions.Value.IsNotNull) then
  begin
    lDefaultValue := FColumn.DefaultValueOptions.Value;
    pColumn.DefaultValueOptions.Value := lDefaultValue;
    if (FColumn.ColumnType = ctyBoolean) then
    begin
      lBoolean := lDefaultValue;
      if (TBooleanColumnMapper(FColumn).InternalColumnType in [ctyByte, ctyString, ctyChar]) then
      begin
        if lBoolean then
        begin
          pColumn.DefaultValueOptions.Value := TBooleanColumnMapper(FColumn).ValueTrue;
        end
        else
        begin
          pColumn.DefaultValueOptions.Value := TBooleanColumnMapper(FColumn).ValueFalse;
        end;
      end
      else
      begin
        pColumn.DefaultValueOptions.Value := FPersistent.GetSQLMapper.BooleanToDefaultExpression(lBoolean);
      end;
    end
    else if (FColumn.ColumnType = ctyEnumerator) and (TEnumerationColumnMapper(FColumn).EnumType = emChar) then
    begin
      pColumn.DefaultValueOptions.Value := TEnumerationColumnMapper(FColumn).EnumCharOf[Integer(lDefaultValue)];
    end;
  end;

end;

procedure TObjectMapperToDDLEntity.FixConstraintNames;
var
  lIndex, lInternalIndex, lConstraintCount: Integer;
  lConstraint, lCompareConstraint: TCustomConstraint;
begin
  for lIndex := 0 to FDDLEntity.Constraints.Count - 1 do
  begin
    lConstraint := FDDLEntity.Constraints.Items[lIndex];
    lConstraintCount := 0;
    for lInternalIndex := lIndex + 1 to FDDLEntity.Constraints.Count - 1 do
    begin
      lCompareConstraint := FDDLEntity.Constraints.Items[lInternalIndex];
      if SameText(lCompareConstraint.Name, lConstraint.Name) then
      begin
        Inc(lConstraintCount);
        lCompareConstraint.Name := Format('%s_%d', [lCompareConstraint.Name, lConstraintCount]);
        lCompareConstraint.Name := FPersistent.AdjustNameLength(lCompareConstraint.Name);
        if SameText(lCompareConstraint.Name, lConstraint.Name) then
        begin
          lCompareConstraint.Name := Copy(lCompareConstraint.Name, 1, Length(lCompareConstraint.Name) - 2);
          lCompareConstraint.Name := Format('%s_%d', [lCompareConstraint.Name, lConstraintCount]);
        end;
      end;
    end;
  end;
end;

function TObjectMapperToDDLEntity.GetColumnTypeFromColumnMapper: TColumnType;
begin
  Result := ctyUnknow;
  if FColumn.ColumnType.IsSimpleType or FColumn.ColumnType.IsBlob then
  begin
    Result := FColumn.ColumnType;
  end
  else if FColumn.ColumnType.IsOrdinal then
  begin
    case FColumn.ColumnType of
      ctyBoolean: Result := TBooleanColumnMapper(FColumn).InternalColumnType;
      ctyEnumerator:
        case TEnumerationColumnMapper(FColumn).EnumType of
          emByte: Result := ctyByte;
          emChar: Result := ctyString;
        end;
    end;
  end
  else if FColumn.InheritsFrom(TReferenceColumnMapper) then
  begin
    Result := TReferenceColumnMapper(FColumn).RefColumnType;
  end;
  Result := FPersistent.FixIntColumnTypeForDDLColumn(Result);
end;

end.
