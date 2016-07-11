unit AM.Freedom.DSPersistent.FireDac;

interface

uses
  System.SysUtils,
  System.Classes,
  Data.FireDACJSONReflect,
  AM.Freedom.Persistent.IDSConnector,
  AM.Freedom.Persistent.CustomDSCursor,
  FireDac.Comp.Client, AM.Freedom.GroupCriteria;

type
  TFDDSCursor = class(TCustomDSCursor<TFDJSONDataSets>)
  strict private
    FMemTable: TFDMemTable;
  strict protected
    function GetColumnCount: Integer; override;
    function GetRecordCount: Integer; override;
    function GetValueByColumnName(pColumnName: string): Variant; override;
    function GetValueByColumnIndex(pColumnIndex: Integer): Variant; override;
    function GetStreamByColumnName(pColumnName: string): TStream; override;
    function GetIsEof: Boolean; override;
    function GetIsBof: Boolean; override;
    function GetRecNo: UInt16; override;
    function GetColumns(pColumnIndex: Integer): string; override;
    procedure DoAssignQuery; override;
  public
    procedure First; override;
    procedure Next; override;
    function IsEmpty: Boolean; override;
  end;

  TFDDSStatement = class(TInterfacedObject, IDSStatement)
  public
    function GetCursor(pClass: TClass; pGroupCriteria: TGroupCriteria): IDSCursor;
  end;

  TFDDSConnector = class(TInterfacedObject, IDSConnector)
  public
    function NewStatement: IDSStatement;
  end;

implementation

uses
  Data.DB,
  AM.Freedom.Exceptions;

{ TFDDataSnapCursor }

function TFDDSCursor.GetColumnCount: Integer;
begin
  Result := FMemTable.FieldCount;
end;

function TFDDSCursor.GetRecordCount: Integer;
begin
  Result := FMemTable.RecordCount;
end;

function TFDDSCursor.GetValueByColumnName(pColumnName: string): Variant;
begin
  Result := FMemTable.FieldByName(pColumnName).Value;
end;

function TFDDSCursor.GetValueByColumnIndex(pColumnIndex: Integer): Variant;
begin
  Result := FMemTable.Fields[pColumnIndex].Value;
end;

function TFDDSCursor.GetStreamByColumnName(pColumnName: string): TStream;
begin
  case FMemTable.FieldByName(pColumnName).DataType of
    ftBlob, ftGraphic, ftOraBlob, ftOraClob:
      Result := TMemoryStream.Create;
    ftMemo, ftFmtMemo, ftWideMemo:
      Result := TStringStream.Create;
  else
    raise EInvalidBlobFieldTypeToStream.Create(FieldTypeNames[FMemTable.FieldByName(pColumnName).DataType], pColumnName);
  end;
  TBlobField(FMemTable.FieldByName(pColumnName)).SaveToStream(Result);
  Result.Position := 0;
end;

function TFDDSCursor.GetIsEof: Boolean;
begin
  Result := FMemTable.Eof;
end;

function TFDDSCursor.GetIsBof: Boolean;
begin
  Result := FMemTable.Bof;
end;

function TFDDSCursor.GetRecNo: UInt16;
begin
  Result := FMemTable.RecNo;
end;

function TFDDSCursor.GetColumns(pColumnIndex: Integer): string;
begin
  Result := FMemTable.Fields.Fields[pColumnIndex].FieldName;
end;

procedure TFDDSCursor.DoAssignQuery;
begin
  if Assigned(FMemTable) then
  begin
    FMemTable := TFDMemTable.Create(nil);
  end;
  FMemTable.AppendData(TFDJSONDataSetsReader.GetListValue(InternalObject, 0));
end;

procedure TFDDSCursor.First;
begin
  FMemTable.First;
end;

procedure TFDDSCursor.Next;
begin
  FMemTable.Next;
end;

function TFDDSCursor.IsEmpty: Boolean;
begin
  Result := FMemTable.IsEmpty;
end;

{ TFDDSConnector }

function TFDDSConnector.NewStatement: IDSStatement;
begin
  Result := TFDDSStatement.Create;
end;

{ TFDDSStatement }

function TFDDSStatement.GetCursor(pClass: TClass; pGroupCriteria: TGroupCriteria): IDSCursor;
begin
//  Result := TFDDSCursor.Create;
//
//  Result.AssignQuery();
end;

end.
