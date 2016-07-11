unit AM.Freedom.SQLMappers.Expressions;

interface

uses
  System.Generics.Collections,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.GroupCriteria,
  AM.Freedom.Helper.Variant,
  AM.Freedom.SQLMappers.CustomExpression,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.GroupCriteria.Criteria,
  AM.Freedom.SQLCommands.CustomFieldCommand,
  AM.Freedom.ArgumentDesignerRegister;

type
  TSum = class(TCustomExpression)
  protected
    class function AgumentDesigner: TArgumentDesigner; override;
  end;

  TMin = class(TCustomExpression)
  protected
    class function AgumentDesigner: TArgumentDesigner; override;
  end;

  TMax = class(TCustomExpression)
  protected
    class function AgumentDesigner: TArgumentDesigner; override;
  end;

  TAvg = class(TCustomExpression)
  protected
    class function AgumentDesigner: TArgumentDesigner; override;
  end;

  TCount = class(TCustomExpression)
  protected
    class function AgumentDesigner: TArgumentDesigner; override;
  end;

  TUpper = class(TCustomExpression)
  protected
    class function AgumentDesigner: TArgumentDesigner; override;
  end;

  TLower = class(TCustomExpression)
  protected
    class function AgumentDesigner: TArgumentDesigner; override;
  end;

  TCoalesce = class(TCustomExpression)
  strict private type
    TArguments = TObjectList<TCustomArgument>;
  strict private
    FAnotherArguments: TArguments;
  public
    constructor Create(pArgument: TCustomArgument; pAnotherArguments: Array of TCustomArgument; pAlias: String = ''); reintroduce;
    destructor Destroy; override;
    property Argument;
    property AnotherArguments: TArguments read FAnotherArguments;
  end;

  TCast = class(TCustomExpression)
  private
    FCastAs: TCustomFieldCommand;
  strict protected
    procedure SetCastAs(const pCastAs: TCustomFieldCommand);
  public
    constructor Create(pArgument: TCustomArgument; pCastAs: TCustomFieldCommand; pAlias: String = ''); reintroduce;
    destructor Destroy; override;
    property Argument;
    property CastAs: TCustomFieldCommand read FCastAs write SetCastAs;
  end;

  TCalcItem = class sealed
  strict private
    FCalcType: TCalcExpressionType;
    FArgument: TCustomArgument;
  strict protected
    procedure SetArgument(const pArgument: TCustomArgument);
  public
    constructor Create(pArgument: TCustomArgument; pCalcType: TCalcExpressionType = TCalcExpressionType.Addition);
    destructor Destroy; override;
    property CalcType: TCalcExpressionType read FCalcType write FCalcType default TCalcExpressionType.Addition;
    property Argument: TCustomArgument read FArgument write SetArgument;
  end;

  TCalc = class(TCustomExpression)
  strict private
    FCalcItems: TObjectList<TCalcItem>;
  public
    constructor Create; overload;
    constructor Create(pFirstItem: TCustomArgument; pCalcType: TCalcExpressionType; pSecondItem: TCustomArgument; pItems: Array of TCalcItem; pAlias: String = ''); overload;
    destructor Destroy; override;
    property CalcItems: TObjectList<TCalcItem> read FCalcItems;
  end;

  TCaseWhen = class(TCustomExpression)
  strict private
    FCaseWhen: TGroupCriteria;
    FCaseThen: TCustomArgument;
    FCaseElse: TCustomArgument;
    procedure Initialize;
  strict protected
    procedure SetCaseThen(const pCaseThen: TCustomArgument); virtual;
    procedure SetCaseElse(const pCaseElse: TCustomArgument); virtual;
  public
    constructor Create; overload;
    constructor Create(pCaseWhen: Array of TCriteria; pCaseThen: TCustomArgument; pCaseElse: TCustomArgument; pAlias: String = ''); overload;
    destructor Destroy; override;
    property CaseWhen: TGroupCriteria read FCaseWhen;
    property CaseThen: TCustomArgument read FCaseThen write SetCaseThen;
    property CaseElse: TCustomArgument read FCaseElse write SetCaseElse;
  end;

implementation

uses
  System.SysUtils, System.Rtti, AM.Freedom.SQLMappers.Arguments,
  AM.Freedom.Exceptions;

{ TCoalesce }

constructor TCoalesce.Create(pArgument: TCustomArgument; pAnotherArguments: Array of TCustomArgument; pAlias: String);
var
  I: Integer;
begin
  inherited Create(pArgument, pAlias);
  FAnotherArguments := TObjectList<TCustomArgument>.Create;
  for I := Low(pAnotherArguments) to High(pAnotherArguments) do
  begin
    FAnotherArguments.Add(pAnotherArguments[I]);
  end;
end;

destructor TCoalesce.Destroy;
begin
  FreeAndNil(FAnotherArguments);
  inherited;
end;

{ TCast }

constructor TCast.Create(pArgument: TCustomArgument; pCastAs: TCustomFieldCommand; pAlias: String);
begin
  inherited Create(pArgument, pAlias);
  FCastAs := pCastAs;
end;

destructor TCast.Destroy;
begin
  FreeAndNil(FCastAs);
  inherited;
end;

procedure TCast.SetCastAs(const pCastAs: TCustomFieldCommand);
begin
  if FCastAs <> pCastAs then
  begin
    FreeAndNil(FCastAs);
  end;
  FCastAs := pCastAs;
end;

{ TCalcItem }

constructor TCalcItem.Create(pArgument: TCustomArgument; pCalcType: TCalcExpressionType);
begin
  SetArgument(pArgument);
  FCalcType := pCalcType;
end;

destructor TCalcItem.Destroy;
begin
  FreeAndNil(FArgument);
  inherited;
end;

procedure TCalcItem.SetArgument(const pArgument: TCustomArgument);
begin
  if pArgument.ClassType = TTableArgument then
  begin
    raise EInvalidArgument.Create(TTableArgument.ClassName, Self.ClassName);
  end;
  if FArgument <> pArgument then
  begin
    FreeAndNil(FArgument);
  end;
  FArgument := pArgument;
end;

{ TCalc }

constructor TCalc.Create(pFirstItem: TCustomArgument; pCalcType: TCalcExpressionType; pSecondItem: TCustomArgument; pItems: Array of TCalcItem; pAlias: String);
var
  I: Integer;
begin
  Create;
  Alias := pAlias;
  FCalcItems.Add(TCalcItem.Create(pFirstItem));
  FCalcItems.Add(TCalcItem.Create(pSecondItem, pCalcType));
  for I := Low(pItems) to High(pItems) do
  begin
    FCalcItems.Add(pItems[I]);
  end;
end;

destructor TCalc.Destroy;
begin
  FreeAndNil(FCalcItems);
  inherited;
end;

constructor TCalc.Create;
begin
  inherited Create;
  FCalcItems := TObjectList<TCalcItem>.Create;
end;

{ TCaseWhen }

constructor TCaseWhen.Create;
begin
  inherited Create;
  Initialize;
end;

constructor TCaseWhen.Create(pCaseWhen: array of TCriteria; pCaseThen, pCaseElse: TCustomArgument; pAlias: String);
var
  I: Integer;
begin
  inherited Create;
  Initialize;
  SetCaseThen(pCaseThen);
  SetCaseElse(pCaseElse);
  for I := Low(pCaseWhen) to High(pCaseWhen) do
  begin
    FCaseWhen.ListCriterias.Add(pCaseWhen[I]);
  end;
  Alias := pAlias;
end;

destructor TCaseWhen.Destroy;
begin
  FreeAndNil(FCaseWhen);
  FreeAndNil(FCaseThen);
  FreeAndNil(FCaseElse);
  inherited;
end;

procedure TCaseWhen.Initialize;
begin
  FCaseWhen := TGroupCriteria.Create;
end;

procedure TCaseWhen.SetCaseElse(const pCaseElse: TCustomArgument);
begin
  if FCaseElse <> pCaseElse then
  begin
    FreeAndNil(FCaseElse)
  end;
  FCaseElse := pCaseElse;
end;

procedure TCaseWhen.SetCaseThen(const pCaseThen: TCustomArgument);
begin
  if FCaseThen <> pCaseThen then
  begin
    FreeAndNil(FCaseThen)
  end;
  FCaseThen := pCaseThen;
end;

{ TSum }

class function TSum.AgumentDesigner: TArgumentDesigner;
begin
  Result := TArgumentDesigner.Create;
  Result.ArgumentClassName := ClassName;
  Result.ArgumentCreateName := 'Create';
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pArgument', dptArgument, True));
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pAlias', dptString, False));
end;

{ TMin }

class function TMin.AgumentDesigner: TArgumentDesigner;
begin
  Result := TArgumentDesigner.Create;
  Result.ArgumentClassName := ClassName;
  Result.ArgumentCreateName := 'Create';
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pArgument', dptArgument, True));
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pAlias', dptString, False));
end;

{ TMax }

class function TMax.AgumentDesigner: TArgumentDesigner;
begin
  Result := TArgumentDesigner.Create;
  Result.ArgumentClassName := ClassName;
  Result.ArgumentCreateName := 'Create';
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pArgument', dptArgument, True));
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pAlias', dptString, False));
end;

{ TAvg }

class function TAvg.AgumentDesigner: TArgumentDesigner;
begin
  Result := TArgumentDesigner.Create;
  Result.ArgumentClassName := ClassName;
  Result.ArgumentCreateName := 'Create';
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pArgument', dptArgument, True));
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pAlias', dptString, False));
end;

{ TCount }

class function TCount.AgumentDesigner: TArgumentDesigner;
begin
  Result := TArgumentDesigner.Create;
  Result.ArgumentClassName := ClassName;
  Result.ArgumentCreateName := 'Create';
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pArgument', dptArgument, True));
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pAlias', dptString, False));
end;

{ TUpper }

class function TUpper.AgumentDesigner: TArgumentDesigner;
begin
  Result := TArgumentDesigner.Create;
  Result.ArgumentClassName := ClassName;
  Result.ArgumentCreateName := 'Create';
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pArgument', dptArgument, True));
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pAlias', dptString, False));
end;

{ TLower }

class function TLower.AgumentDesigner: TArgumentDesigner;
begin
  Result := TArgumentDesigner.Create;
  Result.ArgumentClassName := ClassName;
  Result.ArgumentCreateName := 'Create';
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pArgument', dptArgument, True));
  Result.ParameterDesignerList.Add(TParameterDesigner.Create('pAlias', dptString, False));
end;

initialization
  TArgumentDesignerRegister.RegisterArgumentDesigner(TSum.AgumentDesigner);
  TArgumentDesignerRegister.RegisterArgumentDesigner(TMin.AgumentDesigner);
  TArgumentDesignerRegister.RegisterArgumentDesigner(TMax.AgumentDesigner);
  TArgumentDesignerRegister.RegisterArgumentDesigner(TAvg.AgumentDesigner);
  TArgumentDesignerRegister.RegisterArgumentDesigner(TCount.AgumentDesigner);
  TArgumentDesignerRegister.RegisterArgumentDesigner(TUpper.AgumentDesigner);
  TArgumentDesignerRegister.RegisterArgumentDesigner(TLower.AgumentDesigner);

end.
