unit AM.Freedom.SQLMappers.OrderByClause;

interface

uses
  System.Generics.Collections,
  System.Generics.Defaults,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.SQLMappers.Arguments;

type
  TOrderBy = class
  private
    FField: TFieldArgument;
    FOrderType: TOrderType;
    FDirective: TLiteralArgument;
    FIndex: Byte;
    procedure SetDirective(const pDirective: TLiteralArgument);
    procedure SetField(const pField: TFieldArgument);
  public
    constructor Create; overload;
    constructor Create(pFieldArgument: TFieldArgument; pIndex: Byte; pDirective: TLiteralArgument = nil; pOrderType: TOrderType = TOrderType.Asc); overload;
    constructor Create(const pName: String; pIndex: Byte; pTableAlias: String = ''; pDirective: String = ''; pOrderType: TOrderType = TOrderType.Asc); overload;
    destructor Destroy; override;
    property Field: TFieldArgument read FField write SetField;
    property Directive: TLiteralArgument read FDirective write SetDirective;
    property Order: TOrderType read FOrderType write FOrderType default TOrderType.Asc;
    property Index: Byte read FIndex write FIndex;
  end;

  TOrderByClause = class(TObjectList<TOrderBy>)
  strict private type
    TOrberByComparer = class(TComparer<TOrderBy>)
    public
      function Compare(const Left, Right: TOrderBy): Integer; override;
    end;
  public
    constructor Create; reintroduce;
    procedure InvertAllOrderType;
    function AddItem(const pFieldNameOrIndex: string; pIndex: Byte; const pTableAlias: String = ''; const pDirective: String = '';
        const pOrder: TOrderType = TOrderType.Asc): TOrderBy;
  end;

implementation

uses
  System.SysUtils;

{ TOrderByItem }

constructor TOrderBy.Create(pFieldArgument: TFieldArgument; pIndex: Byte; pDirective: TLiteralArgument; pOrderType: TOrderType);
begin
  Create;
  FField := pFieldArgument;
  FOrderType := pOrderType;
  FDirective := pDirective;
  FIndex := pIndex;
end;

constructor TOrderBy.Create(const pName: String; pIndex: Byte; pTableAlias: String; pDirective: String; pOrderType: TOrderType);
begin
  Create(TFieldArgument.Create(pName, pTableAlias), pIndex, TLiteralArgument.Create(pDirective), pOrderType);
end;

destructor TOrderBy.Destroy;
begin
  FreeAndNil(FField);
  FreeAndNil(FDirective);
  inherited;
end;

procedure TOrderBy.SetDirective(const pDirective: TLiteralArgument);
begin
  if FDirective <> pDirective then
  begin
    FreeAndNil(FDirective);
  end;
  FDirective := pDirective;
end;

procedure TOrderBy.SetField(const pField: TFieldArgument);
begin
  if FField <> pField then
  begin
    FreeAndNil(FField);
  end;
  FField := pField;
end;

constructor TOrderBy.Create;
begin
  inherited;
  FOrderType := TOrderType.Asc;
end;

function TOrderByClause.AddItem(const pFieldNameOrIndex: string; pIndex: Byte; const pTableAlias, pDirective: String;
  const pOrder: TOrderType): TOrderBy;
begin
  Result := TOrderBy.Create(TFieldArgument.Create(pFieldNameOrIndex, pTableAlias, ''), pIndex,
    TLiteralArgument.Create(pDirective), pOrder);
  Add(Result);
end;

constructor TOrderByClause.Create;
begin
  inherited Create(TOrberByComparer.Create, True);
end;

procedure TOrderByClause.InvertAllOrderType;
var
  lOrder: TOrderBy;
begin
  for lOrder in Self do
  begin
    case lOrder.Order of
      Asc: lOrder.Order := Desc;
      Desc: lOrder.Order := Asc;
    end;
  end;
end;

{ TOrderByClause.TOrberByComparer }

function TOrderByClause.TOrberByComparer.Compare(const Left, Right: TOrderBy): Integer;
begin
  Result := Left.Index - Right.Index;
end;

end.
