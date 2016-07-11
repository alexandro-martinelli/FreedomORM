unit AM.Freedom.ObjectMapper.DDLObjects;

interface

uses
  System.Generics.Collections,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ObjectMapper.CustomColumn,
  AM.Freedom.ObjectMapper.CustomMapper,
  AM.Freedom.SQLCommands.Constraints;

type
  TCustomDDLColumn = class(TCustomColumn)
  private
    FSize: UInt32;
    FScale: Byte;
  public
    property Size: UInt32 read FSize write FSize;
    property Scale: Byte read FScale write FScale;
  end;

  TDDLDomain = class(TCustomDDLColumn)
  private
    FSchema: String;
  public
    constructor Create(pSchemaName: string); reintroduce;
    property Schema: String read FSchema write FSchema;
  end;

  TDDLDomains = class(TObjectList<TDDLDomain>)
  public
    function FindDomain(pName: String): TDDLDomain;
  end;

  TDDLColumn = class(TCustomDDLColumn)
  public
    property Domain;
  end;

  TDDLColumns = class(TObjectList<TDDLColumn>)
  public
    function FindColumn(pName: String; pColumnType: TColumnType; pIgnoreColumnTypes: TColumnTypes = []): TDDLColumn;
    function IDCOlumn: TDDLColumn;
  end;

  TDDLConstraints = class(TObjectList<TCustomConstraint>)
  public
    function FindConstraint(pConstraintName: String): TCustomConstraint;
  end;

  TDDLEntity = class(TCustomMapper)
  strict private
    FConstraints: TDDLConstraints;
    FColumns: TDDLColumns;
    FSchema: String;
  public
    constructor Create;
    destructor Destroy; override;
    property Constraints: TDDLConstraints read FConstraints;
    property Columns: TDDLColumns read FColumns;
    property Schema: String read FSchema write FSchema;
  end;


implementation

uses
  System.SysUtils;

{ TDDLTable }

constructor TDDLEntity.Create;
begin
  FConstraints := TDDLConstraints.Create;
  FColumns := TDDLColumns.Create;
end;

destructor TDDLEntity.Destroy;
begin
  FConstraints.Free;
  FColumns.Free;
  inherited;
end;

{ TDDLColumns }

function TDDLColumns.FindColumn(pName: String; pColumnType: TColumnType; pIgnoreColumnTypes: TColumnTypes): TDDLColumn;
var
  lColumn: TDDLColumn;
begin
  Result := nil;
  for lColumn in Self do
  begin
    if SameText(lColumn.Name, pName) and (pColumnType in [lColumn.ColumnType, ctyUnknow]) and
       ((pIgnoreColumnTypes = []) or (not (lColumn.ColumnType in pIgnoreColumnTypes))) then
    begin
      Result := lColumn;
      Break;
    end;
  end;
end;

function TDDLColumns.IDCOlumn: TDDLColumn;
var
  lColumn: TDDLColumn;
begin
  Result := nil;
  for lColumn in Self do
  begin
    if lColumn.IdOptions.IsId then
    begin
      Result := lColumn;
      Break;
    end;
  end;
end;

{ TDDLConstraints }

function TDDLConstraints.FindConstraint(pConstraintName: String): TCustomConstraint;
var
  lConstraint: TCustomConstraint;
begin
  Result := nil;
  for lConstraint in Self do
  begin
    if SameText(lConstraint.Name, pConstraintName) then
    begin
      Result := lConstraint;
      Break;
    end;
  end;
end;

{ TDDLDomains }

function TDDLDomains.FindDomain(pName: String): TDDLDomain;
var
  lDomain: TDDLDomain;
begin
  Result := nil;
  for lDomain in Self do
  begin
    if SameText(lDomain.Name, pName) then
    begin
      Result := lDomain;
      Break;
    end;
  end;
end;

{ TDDLDomain }

constructor TDDLDomain.Create(pSchemaName: string);
begin
  inherited Create;
  FSchema := pSchemaName;
end;

end.
