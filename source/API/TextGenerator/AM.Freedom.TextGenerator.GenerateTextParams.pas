unit AM.Freedom.TextGenerator.GenerateTextParams;

interface

uses
  AM.Freedom.EnumerationTypes;

type
  TGenerateTextParams = class sealed
  private
    FWithAlias: Boolean;
    FBetweenParentesis: Boolean;
    FComparatorType: TComparatorType;
    FAlterFieldCommandParams: TAlterFieldCommandParams;
    FChangedProperties: TChangedProperties;
    FSchema: String;
    FUseIdentity: Boolean;
  public
    constructor Create(pWithAlias, pBetweenParentesis: Boolean; pComparatorType: TComparatorType = cpEqual;
        pAlterFieldCommandParams: TAlterFieldCommandParams = [];
        pChangedProperties: TChangedProperties = []);
    function CreateNew: TGenerateTextParams;
    property WithAlias: Boolean read FWithAlias write FWithAlias;
    property BetweenParentesis: Boolean read FBetweenParentesis write FBetweenParentesis;
    property ComparatorType: TComparatorType read FComparatorType write FComparatorType;
    property AlterFieldCommandParams: TAlterFieldCommandParams read FAlterFieldCommandParams write FAlterFieldCommandParams;
    property ChangedProperties: TChangedProperties read FChangedProperties write FChangedProperties;
    property Schema: String read FSchema write FSchema;
    property UseIdentity: Boolean read FUseIdentity write FUseIdentity;
  end;

implementation

{ TGenerateTextParams }

constructor TGenerateTextParams.Create(pWithAlias, pBetweenParentesis: Boolean; pComparatorType: TComparatorType;
    pAlterFieldCommandParams: TAlterFieldCommandParams; pChangedProperties: TChangedProperties);
begin
  FWithAlias := pWithAlias;
  FBetweenParentesis := pBetweenParentesis;
  FComparatorType := pComparatorType;
  FAlterFieldCommandParams := pAlterFieldCommandParams;
  FChangedProperties := pChangedProperties;
end;

function TGenerateTextParams.CreateNew: TGenerateTextParams;
begin
  Result := TGenerateTextParams.Create(FWithAlias, FBetweenParentesis);
  Result.ComparatorType := FComparatorType;
  Result.AlterFieldCommandParams := FAlterFieldCommandParams;
  Result.ChangedProperties := FChangedProperties;
  Result.Schema := FSchema;
  Result.UseIdentity := FUseIdentity;
end;

end.
