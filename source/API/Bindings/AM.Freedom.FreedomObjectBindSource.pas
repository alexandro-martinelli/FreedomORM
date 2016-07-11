unit AM.Freedom.FreedomObjectBindSource;

interface

uses
  Data.Bind.ObjectScope,
  System.Classes,
  AM.UnitReader.Members.ClassMember;

type
  TFreedomObjectBindSource = class(TCustomAdapterBindSource)
  strict private
    FDataGeneratorAdapter: TDataGeneratorAdapter;
    FObjectClassName: string;
    procedure ClearFields;
  private
    FObjectAdapter: TObject;
    procedure SetObjectAdapter(const Value: TObject);
    function GetFieldDefs: TGeneratorFieldDefs;
    procedure SetFieldDefs(const Value: TGeneratorFieldDefs);
    property Adapter;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GenerateFieldsWithClassMember(pClassMember: TClassMember);
    property ObjectAdapter: TObject read FObjectAdapter write SetObjectAdapter;
  published
    property ObjectClassName: string read FObjectClassName write FObjectClassName;
    property FieldDefs: TGeneratorFieldDefs read GetFieldDefs write SetFieldDefs;
  end;

implementation

uses
  AM.UnitReader.Members.PropertyMember,
  AM.UnitReader.Enumerations;

{ TFreedomObjectBindSource }

procedure TFreedomObjectBindSource.ClearFields;
begin
  FDataGeneratorAdapter.FieldDefs.Clear;
end;

constructor TFreedomObjectBindSource.Create(AOwner: TComponent);
begin
  inherited;
  Active := False;
  AutoActivate := False;
  FDataGeneratorAdapter := TDataGeneratorAdapter.Create(Self);
  FDataGeneratorAdapter.AutoPost := True;
  FDataGeneratorAdapter.AutoEdit := True;
  if csDesigning in ComponentState then
  begin
    FDataGeneratorAdapter.SetSubComponent(False);
  end;
  FDataGeneratorAdapter.Options := [loptAllowInsert,loptAllowDelete,loptAllowModify,loptAllowApplyUpdates,loptAllowCancelUpdates];
  if csDesigning in ComponentState then
  begin
    Adapter := FDataGeneratorAdapter;
    FDataGeneratorAdapter.Active := True;
  end;
end;

procedure TFreedomObjectBindSource.GenerateFieldsWithClassMember(pClassMember: TClassMember);
var
  lField: TGeneratorFieldDef;
  lProperty: TPropertyMember;

  function ToGeneratorFieldType: TGeneratorFieldType;
  begin
    Result := ftString;
    case lProperty.PropertyType of
      mtSmallInt, mtInteger, mtInt64, mtShortInt, mtInt8, mtInt16, mtInt32:
        Result := ftInteger;
      mtByte, mtUInt64, mtUInt8, mtUInt16, mtUInt32, mtWord, mtCardinal:
        Result := ftUInteger;
      mtSingle, mtReal, mtDouble, mtExtended, mtCurrency:
        Result := ftCurrency;
      mtBoolean:
        Result := ftBoolean;
      mtDate:
        Result := ftDate;
      mtTime:
        Result := ftTime;
      mtDateTime:
        Result := ftDateTime;
      mtTStrings:
        Result := ftTStrings;
    end;
  end;

begin
  ClearFields;
  if Assigned(pClassMember) then
  begin
    for lProperty in pClassMember.Properties do
    begin
      if lProperty.PropertyType <> mtUnknow then
      begin
        lField := FDataGeneratorAdapter.FieldDefs.AddFieldDef;
        lField.Name := lProperty.Name;
        lField.FieldType := ToGeneratorFieldType;
        lField.Generator := '';
        lField.Options := [];
      end;
    end;
  end;
end;

function TFreedomObjectBindSource.GetFieldDefs: TGeneratorFieldDefs;
begin
  Result := FDataGeneratorAdapter.FieldDefs;
end;

procedure TFreedomObjectBindSource.SetFieldDefs(const Value: TGeneratorFieldDefs);
begin
  FDataGeneratorAdapter.FieldDefs := Value;
end;

procedure TFreedomObjectBindSource.SetObjectAdapter(const Value: TObject);
begin
  AutoActivate := False;
  Active := False;
  FObjectAdapter := Value;
  if Assigned(FObjectAdapter) then
  begin
    Adapter := TObjectBindSourceAdapter.Create(Self, FObjectAdapter, FObjectAdapter.ClassType, False);
    AutoActivate := True;
    Active := True;
    FDataGeneratorAdapter.Active := True;
  end;
end;

end.
