unit uTestArguments;

{$I FreedomORM.inc}

interface

uses
  TestFramework, System.Generics.Collections, AM.Freedom.Helper.Variant,
  AM.Freedom.SQLMapper.CustomArgument, AM.Freedom.GroupCriteria.ValueCriteria,
  AM.Freedom.EnumerationTypes, AM.Freedom.SQLMappers.Arguments;

type
  TestTTableArgument = class(TTestCase)
  strict private
    FTableArgument: TTableArgument;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTFieldArgument = class(TTestCase)
  strict private
    FFieldArgument: TFieldArgument;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTLiteralArgument = class(TTestCase)
  strict private
    FLiteralArgument: TLiteralArgument;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTNullArgument = class(TTestCase)
  strict private
    FNullArgument: TNullArgument;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTValueArgument = class(TTestCase)
  strict private
    FValueArgument: TValueArgument;
  public
    procedure TearDown; override;
  published
    procedure TestCreateAs;
    procedure TestCreateAsString;
    procedure TestCreateAsSmallint;
    procedure TestCreateAsInteger;
    procedure TestCreateAsInt64;
    procedure TestCreateAsDouble;
    procedure TestCreateAsExtended;
    procedure TestCreateAsCurrency;
    procedure TestCreateAsDate;
    procedure TestCreateAsTime;
    procedure TestCreateAsDateTime;
  end;

  TestTBetweenArgument = class(TTestCase)
  strict private
    FBetweenArgument: TBetweenArgument;
  public
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

  TestTInArgument = class(TTestCase)
  strict private
    FInArgument: TInArgument;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestProperties;
  end;

implementation

uses
  System.SysUtils;

procedure TestTTableArgument.SetUp;
begin
  FTableArgument := TTableArgument.Create('CLIENTES', 'CLI');
end;

procedure TestTTableArgument.TearDown;
begin
  FTableArgument.Free;
  FTableArgument := nil;
end;

procedure TestTTableArgument.TestProperties;
begin
  CheckEqualsString('CLIENTES', FTableArgument.Name);
  CheckEqualsString('CLI', FTableArgument.Alias);
end;

procedure TestTFieldArgument.SetUp;
begin
  FFieldArgument := TFieldArgument.Create('NOME', 'CLI', 'NOME_CLIENTE');
end;

procedure TestTFieldArgument.TearDown;
begin
  FFieldArgument.Free;
  FFieldArgument := nil;
end;

procedure TestTFieldArgument.TestProperties;
begin
  CheckEqualsString('NOME', FFieldArgument.Name);
  CheckEqualsString('CLI', FFieldArgument.TableAlias);
  CheckEqualsString('NOME_CLIENTE', FFieldArgument.Alias);
end;

procedure TestTLiteralArgument.SetUp;
begin
  FLiteralArgument := TLiteralArgument.Create('NULLS LAST');
end;

procedure TestTLiteralArgument.TearDown;
begin
  FLiteralArgument.Free;
  FLiteralArgument := nil;
end;

procedure TestTLiteralArgument.TestProperties;
begin
  CheckEqualsString('NULLS LAST', FLiteralArgument.LiteralValue);
end;

procedure TestTNullArgument.SetUp;
begin
  FNullArgument := TNullArgument.Create;
end;

procedure TestTNullArgument.TearDown;
begin
  FNullArgument.Free;
  FNullArgument := nil;
end;

procedure TestTNullArgument.TestProperties;
begin
  CheckEqualsString('null', FNullArgument.LiteralValue);
end;

procedure TestTValueArgument.TearDown;
begin
  FreeAndNil(FValueArgument);
end;

procedure TestTValueArgument.TestCreateAs;
var
  lNow: TDateTime;
begin
  FValueArgument := TValueArgument.CreateAs<Integer>(1);
  CheckEquals(1, FValueArgument.AsInteger);
  FreeAndNil(FValueArgument);
  FValueArgument := TValueArgument.CreateAs<String>('1');
  CheckEqualsString('1', FValueArgument.AsString);
  FreeAndNil(FValueArgument);
  FValueArgument := TValueArgument.CreateAs<Double>(852.63);
  CheckEqualsString(FormatFloat('#,##0.00', 852.63), FormatFloat('#,##0.00', FValueArgument.AsDouble));
  FreeAndNil(FValueArgument);
  FValueArgument := TValueArgument.CreateAs<Extended>(1);
  CheckEquals(1, FValueArgument.AsExtended);
  FreeAndNil(FValueArgument);
  FValueArgument := TValueArgument.CreateAs<SmallInt>(1);
  CheckEquals(1, FValueArgument.AsSmallint);
  lNow := now;
  FreeAndNil(FValueArgument);
  FValueArgument := TValueArgument.CreateAs<TDate>(lNow);
  CheckEqualsString(DatetoStr(lNow), DatetoStr(FValueArgument.AsDate));
  FreeAndNil(FValueArgument);
  FValueArgument := TValueArgument.CreateAs<TTime>(lNow);
  CheckEqualsString(TimeToStr(lNow), TimeToStr(FValueArgument.AsTime));
  FreeAndNil(FValueArgument);
  FValueArgument := TValueArgument.CreateAs<TDateTime>(lNow);
  CheckEqualsString(DateTimeToStr(lNow), DateTimeToStr(FValueArgument.AsDateTime));
  FreeAndNil(FValueArgument);
  FValueArgument := TValueArgument.CreateAs<Currency>(563.32);
  CheckEqualsString(FormatFloat('#,##0.00', 563.32), FormatFloat('#,##0.00', FValueArgument.AsCurrency));
end;

procedure TestTValueArgument.TestCreateAsString;
var
  lValue: string;
begin
  lValue := 'Alexandro';
  FValueArgument := TValueArgument.CreateAsString(lValue);
  CheckEqualsString(lValue, FValueArgument.AsString);
end;

procedure TestTValueArgument.TestCreateAsSmallint;
var
  lValue: Smallint;
begin
  lValue := 10;
  FValueArgument := TValueArgument.CreateAsSmallint(lValue);
  CheckEquals(lValue, FValueArgument.AsSmallint);
end;

procedure TestTValueArgument.TestCreateAsInteger;
var
  lValue: Integer;
begin
  lValue := 10;
  FValueArgument := TValueArgument.CreateAsInteger(lValue);
  CheckEquals(lValue, FValueArgument.AsInteger);
end;

procedure TestTValueArgument.TestCreateAsInt64;
var
  lValue: Int64;
begin
  lValue := 10;
  FValueArgument := TValueArgument.CreateAsInt64(lValue);
  CheckEquals(lValue, FValueArgument.AsInt64);
end;

procedure TestTValueArgument.TestCreateAsDouble;
var
  lValue: Double;
begin
  lValue := 10;
  FValueArgument := TValueArgument.CreateAsDouble(lValue);
  CheckEquals(lValue, FValueArgument.AsDouble);
end;

procedure TestTValueArgument.TestCreateAsExtended;
var
  lValue: Extended;
begin
  lValue := 10;
  FValueArgument := TValueArgument.CreateAsExtended(lValue);
  CheckEquals(lValue, FValueArgument.AsExtended);
end;

procedure TestTValueArgument.TestCreateAsCurrency;
var
  lValue: Currency;
begin
  lValue := 10;
  FValueArgument := TValueArgument.CreateAsCurrency(lValue);
  CheckEquals(lValue, FValueArgument.AsCurrency);
end;

procedure TestTValueArgument.TestCreateAsDate;
var
  lValue: TDate;
begin
  lValue := 10;
  FValueArgument := TValueArgument.CreateAsDate(lValue);
  CheckEquals(DateToStr(lValue), DateToStr(FValueArgument.AsDate));
end;

procedure TestTValueArgument.TestCreateAsTime;
var
  lValue: TTime;
begin
  lValue := 10;
  FValueArgument := TValueArgument.CreateAsTime(lValue);
  CheckEquals(TimeToStr(lValue), TimeToStr(FValueArgument.AsTime));
end;

procedure TestTValueArgument.TestCreateAsDateTime;
var
  lValue: TDateTime;
begin
  lValue := 10;
  FValueArgument := TValueArgument.CreateAsDateTime(lValue);
  CheckEquals(DateTimeToStr(lValue), DateTimeToStr(FValueArgument.AsDateTime));
end;

procedure TestTBetweenArgument.TearDown;
begin
  FBetweenArgument.Free;
  FBetweenArgument := nil;
end;

procedure TestTInArgument.SetUp;
begin
  inherited;
  FInArgument := TInArgument.Create([TFieldArgument.Create('NOME', 'CLI'),
      TValueArgument.CreateAsInteger(10)]);
end;

procedure TestTInArgument.TearDown;
begin
  FInArgument.Free;
  FInArgument := nil;
end;

procedure TestTBetweenArgument.TestProperties;
var
  lNow: TDate;
  lLeftArgument: TValueArgument;
  lRigthArgument: TValueArgument;
begin
  lNow := Now;
  lLeftArgument := TValueArgument.CreateAsDate(lNow - 30);
  lRigthArgument := TValueArgument.CreateAsDate(lNow);

  FBetweenArgument := TBetweenArgument.Create(lLeftArgument, lRigthArgument);

  Check(Assigned(FBetweenArgument.BetweenArgument));
  CheckEquals(TValueArgument, FBetweenArgument.BetweenArgument.ClassType);
  Check(Assigned(FBetweenArgument.AndArgument));
  CheckEquals(TValueArgument, FBetweenArgument.AndArgument.ClassType);

end;

procedure TestTInArgument.TestProperties;
begin
  Check(Assigned(FInArgument));
  CheckEquals(2, FInArgument.InArguments.Count);
  CheckEquals(TFieldArgument, FInArgument.InArguments.Items[0].ClassType);
  CheckEquals(TValueArgument, FInArgument.InArguments.Items[1].ClassType);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTTableArgument.Suite);
  RegisterTest(TestTFieldArgument.Suite);
  RegisterTest(TestTLiteralArgument.Suite);
  RegisterTest(TestTNullArgument.Suite);
  RegisterTest(TestTValueArgument.Suite);
  RegisterTest(TestTBetweenArgument.Suite);
  RegisterTest(TestTInArgument.Suite);
end.

