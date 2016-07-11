unit AM.Freedom.SQLGenerator.SimpleSQLGenerator;

interface

uses
  System.Generics.Collections,
  AM.Freedom.SQLMappers.ISQLMapper,
  AM.Freedom.GroupCriteria,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.TextGeneratorRegister,
  AM.Freedom.TextGenerator.AbstractTextGenerator;

type
  TSimpleSQLGenerator = class(TAbstractTextGenerator)
  strict private
    FWherePolicy: TPolicy;
    FSQL: String;
    FGroupCriterias: TList<TGroupCriteria>;
    FGroupCriteria: TGroupCriteria;
    function GetSQL: String;
    function GetWherePolicy: TPolicy;
    procedure SetWherePolicy(const pPolicy: TPolicy);
    procedure SetGroupCriterias(pGroupCriterias: TList<TGroupCriteria>);
    procedure DestroyGroupCriterias;
  public
    constructor Create(pSimpleSQL: String; pISQLMapper: ISQLMapper = nil); reintroduce;
    destructor Destroy; override;
    function Search(pGroupCriterias: TList<TGroupCriteria>): String; overload; virtual;
    function Search(pGroupCriteria: TGroupCriteria): String; overload; virtual;

    property WherePolicy: TPolicy read GetWherePolicy write SetWherePolicy;
    property SQL: String read GetSQL write FSQL;
    property GroupCriterias: TList<TGroupCriteria> read FGroupCriterias;
  end;

implementation

uses
  System.SysUtils,
  AM.Freedom.TextGenerator.CustomTextGenerator;

{ TDataSetSQLGenerator }

constructor TSimpleSQLGenerator.Create(pSimpleSQL: String; pISQLMapper: ISQLMapper);
begin
  inherited Create(pISQLMapper);
  FSQL := pSimpleSQL;
end;

destructor TSimpleSQLGenerator.Destroy;
begin
  FreeAndNil(FGroupCriterias);
  FreeAndNil(FGroupCriteria);
  inherited;
end;

procedure TSimpleSQLGenerator.DestroyGroupCriterias;
begin
  FreeAndNil(FGroupCriterias);
end;

function TSimpleSQLGenerator.GetSQL: String;
begin
  Result := FSQL;
end;

function TSimpleSQLGenerator.GetWherePolicy: TPolicy;
begin
  Result := FWherePolicy;
end;

function TSimpleSQLGenerator.Search(pGroupCriterias: TList<TGroupCriteria>): String;
begin
  SetGroupCriterias(pGroupCriterias);
  Result := GetTextFromGenerator(Self);
end;

function TSimpleSQLGenerator.Search(pGroupCriteria: TGroupCriteria): String;
var
  lList: TList<TGroupCriteria>;
begin
  lList := TList<TGroupCriteria>.Create;
  try
    if Assigned(pGroupCriteria) then
    begin
      lList.Add(pGroupCriteria);
    end;
    SetGroupCriterias(lList);
    Result := GetTextFromGenerator(Self);
  finally
    DestroyGroupCriterias;
  end;
end;

procedure TSimpleSQLGenerator.SetGroupCriterias(pGroupCriterias: TList<TGroupCriteria>);
begin
  if (FGroupCriteria <> pGroupCriterias) then
  begin
    FreeAndNil(FGroupCriterias);
  end;
  FGroupCriterias := pGroupCriterias;
end;

procedure TSimpleSQLGenerator.SetWherePolicy(const pPolicy: TPolicy);
begin
  FWherePolicy := pPolicy;
end;

end.
