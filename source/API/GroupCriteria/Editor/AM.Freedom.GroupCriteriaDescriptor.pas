unit AM.Freedom.GroupCriteriaDescriptor;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  AM.Freedom.SQLMapper.CustomArgument,
  AM.Freedom.EnumerationTypes,
  Vcl.ComCtrls,
  AM.Freedom.Helper.Policy;

type
  TListCriterias = class;

  TCriteriaDescriptor = class sealed
  private
    FVarName: string;
    FDescription: string;
    FFreeNotifier: TListCriterias;
  public
    destructor Destroy; override;
    property VarName: string read FVarName write FVarName;
    property Description: string read FDescription write FDescription;
  end;

  TListCriterias = class(TObjectList<TCriteriaDescriptor>)
  private
    procedure FreeNotifier(Sender: TCriteriaDescriptor);
  protected
    procedure Notify(const Value: TCriteriaDescriptor; Action: TCollectionNotification); override;
  end;

  TGroupCriteriaDescriptor = class(TObjectList<TGroupCriteriaDescriptor>)
  strict private
    FListCriterias: TListCriterias;
    FPolicy: TPolicy;
    FLimitRows: UInt32;
    FVarName: string;
    FIntoExistingVarName: string;
    function GetDescription: string;
  private
    procedure ListAllGroups(pList: TList<TGroupCriteriaDescriptor>);
    procedure AddToTreeview(pTreeView: TTreeView; pParent: TTreeNode = nil);
    procedure AddToText(pText: TStrings);
  protected
    procedure Notify(const Value: TGroupCriteriaDescriptor; Action: TCollectionNotification); override;
  public
    constructor Create(pVarName: string; pIntoExistingVarName: string = ''; pPolicy: TPolicy = poAnd; pLimitRows: UInt32 = 0); reintroduce;
    destructor Destroy; override;
    function AddGroupCriteria(pVarName: string; pIntoExistingVarName: string = ''; pPolicy: TPolicy = poAnd; pLimitRows: UInt32 = 0): TGroupCriteriaDescriptor;
    property Policy: TPolicy read FPolicy write FPolicy;
    property LimitRows: UInt32 read FLimitRows write FLimitRows;
    property Description: string read GetDescription;
    property ListCriterias: TListCriterias read FListCriterias write FListCriterias;
    property VarName: string read FVarName write FVarName;
    property IntoExistingVarName: string read FIntoExistingVarName write FIntoExistingVarName;
  end;

  TListGroupDescriptor = class(TObjectList<TGroupCriteriaDescriptor>)
  private
    FListCriterias: TListCriterias;
    function GetTextCriteria: TStrings;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function ListAllGroups: TList<TGroupCriteriaDescriptor>;
    procedure RefreshTreeview(pTreeView: TTreeView);
    property ListCriterias: TListCriterias read FListCriterias write FListCriterias;
    property TextCriteria: TStrings read GetTextCriteria;
  end;

implementation

{ TGroupCriteriaDescriptor }

function TGroupCriteriaDescriptor.AddGroupCriteria(pVarName, pIntoExistingVarName: string; pPolicy: TPolicy; pLimitRows: UInt32): TGroupCriteriaDescriptor;
begin
  Result := TGroupCriteriaDescriptor.Create(pVarName, pIntoExistingVarName, pPolicy, pLimitRows);
  Add(Result);
end;

procedure TGroupCriteriaDescriptor.AddToText(pText: TStrings);
var
  lCriteria: TCriteriaDescriptor;
  lGroup: TGroupCriteriaDescriptor;
begin
  pText.Add(GetDescription);
  for lCriteria in FListCriterias do
  begin
    pText.Add(FVarName + '.AddCriteria(' + lCriteria.Description + ');');
  end;
  for lGroup in Self do
  begin
    lGroup.AddToText(pText);
  end;
end;

procedure TGroupCriteriaDescriptor.AddToTreeview(pTreeView: TTreeView; pParent: TTreeNode);
var
  lParent: TTreeNode;
  lCriteria: TCriteriaDescriptor;
  lGroup: TGroupCriteriaDescriptor;
  lNode: TTreeNode;
begin
  lParent := pTreeView.Items.AddChildObject(pParent, GetDescription, Self);
  lParent.SelectedIndex := 3;
  lParent.StateIndex := 3;
  lParent.ImageIndex := 3;
  for lCriteria in FListCriterias do
  begin
    lNode := pTreeView.Items.AddChildObject(lParent,
        FVarName + '.AddCriteria(' + lCriteria.Description + ');', lCriteria);
    lNode.SelectedIndex := 1;
    lNode.StateIndex := 1;
    lNode.ImageIndex := 1;
  end;
  for lGroup in Self do
  begin
    lGroup.AddToTreeview(pTreeView, lParent);
  end;
end;

constructor TGroupCriteriaDescriptor.Create(pVarName, pIntoExistingVarName: string; pPolicy: TPolicy; pLimitRows: UInt32);
begin
  inherited Create(True);
  FListCriterias := TListCriterias.Create;
  FVarName := pVarName;
  FIntoExistingVarName := pIntoExistingVarName;
  FPolicy := pPolicy;
  FLimitRows := pLimitRows;
end;

destructor TGroupCriteriaDescriptor.Destroy;
begin
  FListCriterias.Free;
  inherited;
end;

function TGroupCriteriaDescriptor.GetDescription: string;
begin
  Result := FVarName + ' :=';
  if (FIntoExistingVarName <> '') then
  begin
    Result := Format('%s %s.AddGroupCriteria', [Result, FIntoExistingVarName])
  end
  else
  begin
    Result := Format('%s TGroupCriteria.Create', [Result])
  end;
  if (FPolicy <> poAnd) or (FLimitRows > 0) then
  begin
    Result := Format('%s(%s', [Result, FPolicy.ToDescriptor])
  end;
  if (FLimitRows > 0) then
  begin
    Result := Result + ', ' + IntToStr(FLimitRows);
  end;
  if (FPolicy <> poAnd) or (FLimitRows > 0) then
  begin
    Result := Result + ')';
  end;
  Result := Result + ';';
end;

procedure TGroupCriteriaDescriptor.ListAllGroups(pList: TList<TGroupCriteriaDescriptor>);
var
  lGroup: TGroupCriteriaDescriptor;
begin
  for lGroup in Self do
  begin
    pList.Add(lGroup);
    lGroup.ListAllGroups(pList);
  end;
end;

procedure TGroupCriteriaDescriptor.Notify(const Value: TGroupCriteriaDescriptor; Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnRemoved) then
  begin
    Extract(Value);
  end;
end;
{ TCriteriaDescriptor }
{ TListGroupDescriptor }

constructor TListGroupDescriptor.Create;
begin
  inherited Create(True);
  FListCriterias := TListCriterias.Create;
end;

destructor TListGroupDescriptor.Destroy;
begin
  FListCriterias.Free;
  inherited;
end;

function TListGroupDescriptor.GetTextCriteria: TStrings;
var
  lCriteria: TCriteriaDescriptor;
  lGroup: TGroupCriteriaDescriptor;
begin
  Result := TStringList.Create;
  for lCriteria in FListCriterias do
  begin
    Result.Add(lCriteria.Description);
  end;
  for lGroup in Self do
  begin
    lGroup.AddToText(Result);
  end;
end;

function TListGroupDescriptor.ListAllGroups: TList<TGroupCriteriaDescriptor>;
var
  lGroup: TGroupCriteriaDescriptor;
begin
  Result := TList<TGroupCriteriaDescriptor>.Create;
  for lGroup in Self do
  begin
    Result.Add(lGroup);
    lGroup.ListAllGroups(Result);
  end;
end;

procedure TListGroupDescriptor.RefreshTreeview(pTreeView: TTreeView);
var
  lGroup: TGroupCriteriaDescriptor;
  lCriteria: TCriteriaDescriptor;
  lNode: TTreeNode;
begin
  for lCriteria in FListCriterias do
  begin
    lNode := pTreeView.Items.AddObject(nil, lCriteria.Description, lCriteria);
    lNode.SelectedIndex := 1;
    lNode.StateIndex := 1;
    lNode.ImageIndex := 1;
  end;
  for lGroup in Self do
  begin
    lGroup.AddToTreeview(pTreeView);
  end;
end;

{ TListCriterias }

procedure TListCriterias.FreeNotifier(Sender: TCriteriaDescriptor);
begin
  Extract(Sender);
end;

procedure TListCriterias.Notify(const Value: TCriteriaDescriptor; Action: TCollectionNotification);
begin
  inherited;
  if (Action = cnAdded) and Assigned(Value) then
  begin
    Value.FFreeNotifier := Self;
  end;
end;

{ TCriteriaDescriptor }

destructor TCriteriaDescriptor.Destroy;
begin
  if (Assigned(FFreeNotifier)) then
  begin
    FFreeNotifier.FreeNotifier(Self);
  end;
  inherited;
end;

end.
