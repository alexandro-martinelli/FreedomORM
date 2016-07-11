unit AM.Freedom.ObjectMapper.ColumnsList;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.CustomColumnMapper;

type
  TCustomColumns = class abstract(TObjectList<TCustomColumnMapper>)
  strict private
    FColumnNameForNext: String;
    FStartNext: Boolean;
    FNextColumnType: TColumnType;
    FNextIgnoreTypes: TColumnTypes;
    FLastIndex: Integer;
  public
    constructor Create(pOwnsObjects: Boolean = True); reintroduce;
    function FindColumn(pName: String; pColumnType: TColumnType = ctyUnknow; pIgnoreTypes: TColumnTypes = []): TCustomColumnMapper;
    function FindColumnByFieldOrPropName(pFieldOrPropName: String; pColumnType: TColumnType = ctyUnknow): TCustomColumnMapper;
    function FindNextColumn: TCustomColumnMapper;
    procedure StartNext;
    procedure ResetNext;
  end;

  TColumnsList = class sealed(TCustomColumns)
  strict private
  type
    TEntityColumns = class(TCustomColumns);
    TDetailColumns = class(TCustomColumns);
    TSimpleColumns = class(TCustomColumns);
    TOrdinalColumns = class(TCustomColumns);
    TBlobColumns = class(TCustomColumns);
    TJoinColumns = class(TCustomColumns);
    TNullableColumns = class(TCustomColumns);
    TExtensionColumns = class(TCustomColumns);
  strict private
    FEntityColumns: TEntityColumns;
    FDetailColumns: TDetailColumns;
    FSimpleColumns: TSimpleColumns;
    FOrdinalColumns: TOrdinalColumns;
    FBlobColumns: TBlobColumns;
    FJoinColumns: TJoinColumns;
    FNullableColumns: TNullableColumns;
    FExtensionColumns: TExtensionColumns;
    FCurrentSchema: String;
    function GetDetailColumns: TDetailColumns;
    function GetSimpleColumns: TSimpleColumns;
    function GetOrdinalColumns: TOrdinalColumns;
    function GetBlobColumns: TBlobColumns;
    function GetJoinColumns: TJoinColumns;
    function GetIDColumn: TCustomColumnMapper;
    function GetNullableColumns: TNullableColumns;
    function GetExtensionColumns: TExtensionColumns;
  public
    destructor Destroy; override;
    property DetailColumns: TDetailColumns read GetDetailColumns;
    property SimpleColumns: TSimpleColumns read GetSimpleColumns;
    property OrdinalColumns: TOrdinalColumns read GetOrdinalColumns;
    property BlobColumns: TBlobColumns read GetBlobColumns;
    property JoinColumns: TJoinColumns read GetJoinColumns;
    property NullableColumns: TNullableColumns read GetNullableColumns;
    property ExtensionColumns: TExtensionColumns read GetExtensionColumns;
    property IDColumn: TCustomColumnMapper read GetIDColumn;
    property CurrentSchema: String read FCurrentSchema write FCurrentSchema;
  end;

implementation

uses
  System.StrUtils,
  AM.Freedom.Helper.ColumnType;

{ TCustomColumns }

constructor TCustomColumns.Create(pOwnsObjects: Boolean);
begin
  inherited Create(pOwnsObjects);
  ResetNext;
end;

function TCustomColumns.FindColumn(pName: String; pColumnType: TColumnType; pIgnoreTypes: TColumnTypes): TCustomColumnMapper;
var
  lItem: TCustomColumnMapper;
begin
  Result := nil;
  if (FStartNext) then
  begin
    FColumnNameForNext := pName;
    FNextColumnType := pColumnType;
    FNextIgnoreTypes := pIgnoreTypes;
  end;
  for lItem in Self do
  begin
    if SameText(lItem.Name, pName) and (pColumnType in [lItem.ColumnType, ctyUnknow]) and not (lItem.ColumnType in pIgnoreTypes) then
    begin
      Result := lItem;
      Break;
    end;
  end;
  if (FStartNext) then
  begin
    FLastIndex := 0;
    if (Assigned(Result)) then
    begin
      FLastIndex := IndexOf(Result) + 1;
    end;
  end;
end;

function TCustomColumns.FindColumnByFieldOrPropName(pFieldOrPropName: String; pColumnType: TColumnType): TCustomColumnMapper;
var
  lColumn: TCustomColumnMapper;
begin
  Result := nil;
  for lColumn in Self do
  begin
    if (Assigned(lColumn.RttiOptions.RttiField)) then
    begin
      if SameText(lColumn.RttiOptions.RttiField.Name, pFieldOrPropName) and
         (pColumnType in [lColumn.ColumnType, ctyUnknow]) then
      begin
        Result := lColumn;
        Break;
      end;
    end
    else if (Assigned(lColumn.RttiOptions.RttiProperty)) then
    begin
      if SameText(lColumn.RttiOptions.RttiProperty.Name, pFieldOrPropName) and
         (pColumnType in [lColumn.ColumnType, ctyUnknow]) then
      begin
        Result := lColumn;
        Break;
      end;
    end;
  end;
end;

function TCustomColumns.FindNextColumn: TCustomColumnMapper;
var
  lIndex: Integer;
  lItem: TCustomColumnMapper;
begin
  Result := nil;
  if (FStartNext) then
  begin
    for lIndex := FLastIndex to Count - 1 do
    begin
      lItem := Items[lIndex];
      if SameText(lItem.Name, FColumnNameForNext) and (FNextColumnType in [lItem.ColumnType, ctyUnknow]) and not (lItem.ColumnType in FNextIgnoreTypes) then
      begin
        Result := lItem;
        Break
      end;
    end;
    if (Assigned(Result)) then
    begin
      FLastIndex := IndexOf(Result) + 1;
    end
    else
    begin
      FLastIndex := Count;
    end;
  end;
end;

procedure TCustomColumns.ResetNext;
begin
  FStartNext := False;
  FColumnNameForNext := '';
  FNextColumnType := ctyUnknow;
  FNextIgnoreTypes := [];
  FLastIndex := 0;
end;

procedure TCustomColumns.StartNext;
begin
  FStartNext := True;
  FColumnNameForNext := '';
  FNextColumnType := ctyUnknow;
  FNextIgnoreTypes := [];
  FLastIndex := 0;
end;

{ TColumnsList }

destructor TColumnsList.Destroy;
begin
  FreeAndNil(FSimpleColumns);
  FreeAndNil(FEntityColumns);
  FreeAndNil(FDetailColumns);
  FreeAndNil(FOrdinalColumns);
  FreeAndNil(FBlobColumns);
  FreeAndNil(FJoinColumns);
  FreeAndNil(FNullableColumns);
  FreeAndNil(FExtensionColumns);
  inherited;
end;

function TColumnsList.GetBlobColumns: TBlobColumns;
var
  lColumn: TCustomColumnMapper;
begin
  if not Assigned(FBlobColumns) then
  begin
    FBlobColumns := TBlobColumns.Create(False);
    for lColumn in Self do
    begin
      if lColumn.ColumnType.IsBlob then
      begin
        FBlobColumns.Add(lColumn);
      end;
    end;
  end;
  Result := FBlobColumns;
end;

function TColumnsList.GetIDColumn: TCustomColumnMapper;
var
  lColumn: TCustomColumnMapper;
begin
  Result := nil;
  for lColumn in Self do
  begin
    if lColumn.IdOptions.IsId and ((FCurrentSchema = '') or (lColumn.IdOptions.Schemas.FindSchema(FCurrentSchema) <> nil)) then
    begin
      Result := lColumn;
      Break;
    end;
  end;
end;

function TColumnsList.GetJoinColumns: TJoinColumns;
var
  lColumn: TCustomColumnMapper;
begin
  if not Assigned(FJoinColumns) then
  begin
    FJoinColumns := TJoinColumns.Create(False);
    for lColumn in Self do
    begin
      if lColumn.ColumnType.IsEquals(ctyJoin) then
      begin
        FJoinColumns.Add(lColumn);
      end;
    end;
  end;
  Result := FJoinColumns;
end;

function TColumnsList.GetNullableColumns: TNullableColumns;
var
  lColumn: TCustomColumnMapper;
begin
  if not Assigned(FNullableColumns) then
  begin
    FNullableColumns := TNullableColumns.Create(False);
    for lColumn in Self do
    begin
      if lColumn.IsNullable then
      begin
        FNullableColumns.Add(lColumn);
      end;
    end;
  end;
  Result := FNullableColumns;
end;

function TColumnsList.GetDetailColumns: TDetailColumns;
var
  lColumn: TCustomColumnMapper;
begin
  if not Assigned(FDetailColumns) then
  begin
    FDetailColumns := TDetailColumns.Create(False);
    for lColumn in Self do
    begin
      if lColumn.ColumnType.IsEquals(ctyDetail) then
      begin
        FDetailColumns.Add(lColumn);
      end;
    end;
  end;
  Result := FDetailColumns;
end;

function TColumnsList.GetExtensionColumns: TExtensionColumns;
var
  lColumn: TCustomColumnMapper;
begin
  if (not Assigned(FExtensionColumns)) then
  begin
    FExtensionColumns := TExtensionColumns.Create(False);
    for lColumn in Self do
    begin
      if lColumn.IsExtension then
      begin
        FExtensionColumns.Add(lColumn);
      end;
    end;
  end;
  Result := FExtensionColumns;
end;

function TColumnsList.GetOrdinalColumns: TOrdinalColumns;
var
  lColumn: TCustomColumnMapper;
begin
  if not Assigned(FOrdinalColumns) then
  begin
    FOrdinalColumns := TOrdinalColumns.Create(False);
    for lColumn in Self do
    begin
      if lColumn.ColumnType.IsOrdinal then
        FOrdinalColumns.Add(lColumn);
    end;
  end;
  Result := FOrdinalColumns;
end;

function TColumnsList.GetSimpleColumns: TSimpleColumns;
var
  lColumn: TCustomColumnMapper;
begin
  if not Assigned(FSimpleColumns) then
  begin
    FSimpleColumns := TSimpleColumns.Create(False);
    for lColumn in Self do
    begin
      if lColumn.ColumnType.IsSimpleType then
        FSimpleColumns.Add(lColumn);
    end;
  end;
  Result := FSimpleColumns;
end;

end.

