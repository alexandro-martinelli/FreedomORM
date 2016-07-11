unit AM.Freedom.ObjectMapper.DDLConstraintCompare;

interface

uses
  AM.Freedom.SQLCommands.Constraints;

type
  TDDLConstraintCompare = class sealed
  strict private
    FObjectConstraint: TCustomConstraint;
    FDataBaseConstraint: TCustomConstraint;
    function IsEquals: Boolean;
    function IsEqualsPrimaryOrUniqueKey: Boolean;
    function FindAllConstraintFields: Boolean;
    function IsEqualsForeignKey: Boolean;
    function IsEqualsForeignData: Boolean;
    function IsEqualsForeignFields: Boolean;
    function IsEqualsForeignReferenceFields: Boolean;
    function IsEqualsForeignActions: Boolean;
  public
    class function IsEqualsContraint(pObjectConstraint, pDataBaseConstraint: TCustomConstraint): Boolean;
  end;

implementation

uses
  AM.Freedom.EnumerationTypes, System.SysUtils;

{ TDDLConstraintCompare }

class function TDDLConstraintCompare.IsEqualsContraint(pObjectConstraint, pDataBaseConstraint: TCustomConstraint): Boolean;
var
  lDDLConstraintCompare: TDDLConstraintCompare;
begin
  lDDLConstraintCompare := TDDLConstraintCompare.Create;
  try
    lDDLConstraintCompare.FObjectConstraint := pObjectConstraint;
    lDDLConstraintCompare.FDataBaseConstraint := pDataBaseConstraint;
    Result := lDDLConstraintCompare.IsEquals;
  finally
    lDDLConstraintCompare.Free;
  end;
end;

function TDDLConstraintCompare.IsEqualsForeignActions: Boolean;
begin
  Result := TForeignKey(FObjectConstraint).OnUpdate = TForeignKey(FDataBaseConstraint).OnUpdate;
  if Result then
  begin
    Result := TForeignKey(FObjectConstraint).OnDelete = TForeignKey(FDataBaseConstraint).OnDelete;
  end;
end;

function TDDLConstraintCompare.IsEqualsForeignData: Boolean;
begin
  Result := SameText(TForeignKey(FObjectConstraint).References, TForeignKey(FDataBaseConstraint).References);
  if Result then
  begin
    Result := IsEqualsForeignFields;
  end;
end;

function TDDLConstraintCompare.IsEqualsForeignFields: Boolean;
begin
  Result := TForeignKey(FObjectConstraint).ReferenceFields.Count = TForeignKey(FDataBaseConstraint).ReferenceFields.Count;
  if Result then
  begin
    Result := IsEqualsForeignReferenceFields;
    if Result then
    begin
      Result := IsEqualsForeignActions;
    end;
  end;
end;

function TDDLConstraintCompare.IsEqualsForeignKey: Boolean;
begin
  Result := IsEqualsPrimaryOrUniqueKey;
  if Result then
  begin
    Result := IsEqualsForeignData;
  end;
end;

function TDDLConstraintCompare.IsEqualsForeignReferenceFields: Boolean;
var
  lField, lConstraintField: String;
  lObjectKey, lDataBaseKey: TForeignKey;
begin
  lObjectKey := TForeignKey(FObjectConstraint);
  lDataBaseKey := TForeignKey(FDataBaseConstraint);
  Result := False;
  for lField in lObjectKey.ReferenceFields do
  begin
    for lConstraintField in lDataBaseKey.ReferenceFields do
    begin
      Result := SameText(lConstraintField, lField);
      if Result then
      begin
        Break;
      end;
    end;
    if not Result then
    begin
      Break;
    end;
  end;
end;

function TDDLConstraintCompare.IsEqualsPrimaryOrUniqueKey: Boolean;
begin
  Result := FObjectConstraint.Fields.Count = FDataBaseConstraint.Fields.Count;
  if not Result then
  begin
    Result := FindAllConstraintFields;
  end;
end;

function TDDLConstraintCompare.FindAllConstraintFields: Boolean;
var
  lField, lConstraintField: String;
begin
  Result := False;
  for lField in FObjectConstraint.Fields do
  begin
    for lConstraintField in FDataBaseConstraint.Fields do
    begin
      Result := SameText(lConstraintField, lField);
      if Result then
      begin
        Break;
      end;
    end;
    if not Result then
    begin
      Break;
    end;
  end;
end;

function TDDLConstraintCompare.IsEquals: Boolean;
begin
  case FObjectConstraint.ConstraintType of
    PrimaryKey, UniqueKey: Result := IsEqualsPrimaryOrUniqueKey;
    else
      Result := IsEqualsForeignKey;
  end;
end;

end.
