unit uTestTOrderByClause;

{$I FreedomORM.inc}

interface

uses
  TestFramework, AM.Freedom.SQLMappers.OrderByClause,
  AM.Freedom.SQLMappers.Arguments, System.Generics.Collections,
  AM.Freedom.EnumerationTypes;

type
  TestTOrderBy = class(TTestCase)
  strict private
    FOrderBy: TOrderBy;
  public
    procedure TearDown; override;
  published
    procedure TestCreate;
    procedure TestCreateWithParams;
  end;

  TestTOrderByClause = class(TTestCase)
  strict private
    FOrderByClause: TOrderByClause;
  public
    procedure TearDown; override;
  published
    procedure TestAddItem;
    procedure TestCreate;
    Procedure TestCreateWithParams;
  end;

implementation

uses
  System.SysUtils;

procedure TestTOrderBy.TearDown;
begin
  FOrderBy.Free;
  FOrderBy := nil;
end;

procedure TestTOrderBy.TestCreate;
begin
  FOrderBy := TOrderBy.Create;
  Check(Assigned(FOrderBy));
  Check(Asc = FOrderBy.Order);
end;

procedure TestTOrderBy.TestCreateWithParams;
begin
  FOrderBy := TOrderBy.Create(TFieldArgument.Create('NOME', 'CLI'), 1, TLiteralArgument.Create('NULLS LAST'), TOrderType.Desc);
  Check(Assigned(FOrderBy));
  CheckEqualsString('NOME', FOrderBy.Field.Name);
  CheckEqualsString('CLI', FOrderBy.Field.TableAlias);
  CheckEqualsString('NULLS LAST', FOrderBy.Directive.LiteralValue);
  Check(Desc = FOrderBy.Order);
end;

procedure TestTOrderByClause.TearDown;
begin
  FOrderByClause.Free;
  FOrderByClause := nil;
end;

procedure TestTOrderByClause.TestAddItem;
var
  lReturnValue: TOrderBy;
  lOrder: TOrderType;
  lDirective: string;
  lTableAlias: string;
  lFieldNameOrIndex: string;
begin
  lOrder := Desc;
  lDirective := 'NULLS LAST';
  lTableAlias := 'CLI';
  lFieldNameOrIndex := '1';
  FOrderByClause := TOrderByClause.Create;
  lReturnValue := FOrderByClause.AddItem(lFieldNameOrIndex, 1, lTableAlias, lDirective, lOrder);
  CheckEquals(1, FOrderByClause.Count);
  CheckEqualsString('1', lReturnValue.Field.Name);
  CheckEqualsString('CLI', lReturnValue.Field.TableAlias);
  CheckEqualsString(EmptyStr, lReturnValue.Field.Alias);
  CheckEqualsString('NULLS LAST', lReturnValue.Directive.LiteralValue);
  Check(Desc = lReturnValue.Order);
end;

procedure TestTOrderByClause.TestCreate;
begin
  FOrderByClause := TOrderByClause.Create;
  Check(Assigned(FOrderByClause));
end;

procedure TestTOrderByClause.TestCreateWithParams;
var
  lOrderBy: TOrderBy;
begin
  FOrderByClause := TOrderByClause.Create;
  FOrderByClause.Add(TOrderBy.Create(TFieldArgument.Create('NOME', 'CLI'), 1, TLiteralArgument.Create('NULLS LAST'), TOrderType.Desc));
  FOrderByClause.Add(TOrderBy.Create(TFieldArgument.Create('NOME', 'CLI'), 1, TLiteralArgument.Create('NULLS LAST'), TOrderType.Desc));
  CheckEquals(2, FOrderByClause.Count);
  lOrderBy := FOrderByClause.Items[0];
  Check(Assigned(lOrderBy));
  CheckEqualsString('NOME', lOrderBy.Field.Name);
  CheckEqualsString('CLI', lOrderBy.Field.TableAlias);
  CheckEqualsString('NULLS LAST', lOrderBy.Directive.LiteralValue);
  Check(Desc = lOrderBy.Order);
  lOrderBy := FOrderByClause.Items[1];
  Check(Assigned(lOrderBy));
  CheckEqualsString('NOME', lOrderBy.Field.Name);
  CheckEqualsString('CLI', lOrderBy.Field.TableAlias);
  CheckEqualsString('NULLS LAST', lOrderBy.Directive.LiteralValue);
  Check(Desc = lOrderBy.Order);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTOrderBy.Suite);
  RegisterTest(TestTOrderByClause.Suite);
end.

