unit AM.Freedom.FreedomObjectNotifier;

interface

uses
  System.Generics.Collections,
  StructureViewAPI,
  ToolsAPI,
  Xml.XMLIntf,
  System.Classes,
  AM.Freedom.SQLMappers.NamedObject,
  AM.Freedom.InterfacedObjects;

type
  TFreedomProperty = class(TNamedObject)
  strict private
    FUpdated: Boolean;
  private
    procedure BeginUpdate;
    procedure Update;
  public
    constructor Create(pName: String);
    property Name;
    property Updated: Boolean read FUpdated;
  end;

  TFreedomClass = class(TObjectList<TFreedomProperty>)
  strict private const
    cPropertyID = 33;
  strict private
    FName: String;
    FParent: TFreedomClass;
    FUpdated: Boolean;
//    FParentName: String;
  private
    procedure BeginUpdate;
    procedure Update(const pStructureNode: IOTAStructureNode);
    procedure EndUpdate;
    procedure NotifyFreeParent(const pClass: TFreedomClass);
//    property Parent: TFreedomClass read FParent write FParent;
//    property ParentName: String read FParentName write FParentName;
  public
    constructor Create(const pName: string; pParent: TFreedomClass);
    function Inherits(pClassName: string): Boolean;
    function FindPropertyByName(pPropertyName: String): TFreedomProperty;
    property Name: String read FName;
    property Updated: Boolean read FUpdated;
  end;

  TFreedomUnit = class(TObjectList<TFreedomClass>)
  strict private
    FName: string;
  private
    procedure BeginUpdate;
    procedure Update(const pStructureNode: IOTAStructureNode);
    procedure EndUpdate;
    function FindClassNode(const pStructureNode: IOTAStructureNode): IOTAStructureNode;
//    procedure NotifyFreeClass(const pClass: TFreedomClass);
//    procedure NotifyAddClass(const pClass: TFreedomClass);
  public
    constructor Create(const pName: string); reintroduce;
    destructor Destroy; override;
    function FindClassByName(pClassName: String): TFreedomClass;
    property Name: String read FName;
  end;

  TFreedomProject = class(TInterfacedObjectList<TFreedomUnit>, IOTAProjectFileStorageNotifier)
  strict private
    class var FInstance: TFreedomProject;
    class var FInstanceIndex: Integer;
    procedure CreatingProject(const ProjectOrGroup: IOTAModule);
    function GetName: string;
    procedure ProjectClosing(const ProjectOrGroup: IOTAModule);
    procedure ProjectLoaded(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
    procedure ProjectSaving(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
    function ExtractStructureNode(pContext: IOTAStructureContext): IOTAStructureNode;
    function ExtractHasClasses(pStructureNode: IOTAStructureNode): Boolean;
  private
    procedure BeginUpdate;
    procedure Update(const pContext: IOTAStructureContext);
    procedure EndUpdate;
  public
    class function GetInstance: TFreedomProject;
    class procedure DestroyInstance;
    function FindUnitByName(pUnitName: String): TFreedomUnit;
  end;

  TFreedomNotifier = class(TInterfacedObject, IOTAStructureNotifier)
  strict private
    class var FInstance: IOTAStructureNotifier;
    class var FInstanceIndex: Integer;
    procedure DefaultNodeAction(const Node: IOTAStructureNode);
    procedure NodeEdited(const Node: IOTAStructureNode);
    procedure NodeFocused(const Node: IOTAStructureNode);
    procedure NodeSelected(const Node: IOTAStructureNode);
    procedure StructureChanged(const Context: IOTAStructureContext);
    procedure VisibleChanged(Visible: WordBool);
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class procedure RegisterNotifier;
    class procedure UnregisterNotifier;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils;

type
  TConsts = class
  public const
    cClass = 'CLASSES';
    cUnit = 'UNIT';
    cPublic = 'PUBLIC';
    cPublished = 'PUBLISHED';
  end;

{ TFreedomNotifier }

procedure TFreedomNotifier.AfterConstruction;
begin
  inherited;
end;

procedure TFreedomNotifier.AfterSave;
begin
end;

procedure TFreedomNotifier.BeforeDestruction;
begin
  inherited;
end;

procedure TFreedomNotifier.BeforeSave;
begin
end;

procedure TFreedomNotifier.DefaultNodeAction(const Node: IOTAStructureNode);
begin
end;

procedure TFreedomNotifier.Destroyed;
begin
end;

procedure TFreedomNotifier.Modified;
begin
end;

procedure TFreedomNotifier.NodeEdited(const Node: IOTAStructureNode);
begin
end;

procedure TFreedomNotifier.NodeFocused(const Node: IOTAStructureNode);
begin
end;

procedure TFreedomNotifier.NodeSelected(const Node: IOTAStructureNode);
begin
end;

class procedure TFreedomNotifier.RegisterNotifier;
begin
  if not Assigned(FInstance) then
  begin
    FInstance := TFreedomNotifier.Create;
    FInstanceIndex := (BorlandIDEServices as IOTAStructureView).AddNotifier(FInstance);
    TFreedomProject.GetInstance;
  end;
end;

procedure TFreedomNotifier.StructureChanged(const Context: IOTAStructureContext);
var
  lContext: IOTAStructureContext;
begin
  if Assigned(Context) then
  begin
    lContext := Context;
  end
  else
  begin
    lContext := (BorlandIDEServices as IOTAStructureView).GetStructureContext;
  end;
  if Assigned(lContext) then
  begin
    TFreedomProject.GetInstance.BeginUpdate;
    try
      TFreedomProject.GetInstance.Update(lContext);
    finally
      TFreedomProject.GetInstance.EndUpdate;
    end;
  end;

end;

class procedure TFreedomNotifier.UnregisterNotifier;
begin
  if Assigned(FInstance) then
  begin
    (BorlandIDEServices as IOTAStructureView).RemoveNotifier(FInstanceIndex);
    FInstance := nil;
    TFreedomProject.DestroyInstance;
  end;
end;

procedure TFreedomNotifier.VisibleChanged(Visible: WordBool);
begin
end;
{ TFreedomProject }

procedure TFreedomProject.BeginUpdate;
var
  lUnit: TFreedomUnit;
begin
  for lUnit in Self do
  begin
    lUnit.BeginUpdate;
  end;
end;

procedure TFreedomProject.CreatingProject(const ProjectOrGroup: IOTAModule);
begin
end;

procedure TFreedomProject.EndUpdate;
var
  lUnit: TFreedomUnit;
begin
  for lUnit in Self do
  begin
    lUnit.EndUpdate;
  end;
end;

function TFreedomProject.ExtractHasClasses(pStructureNode: IOTAStructureNode): Boolean;
var
  lCounter: Integer;
begin
  Result := False;
  for lCounter := pStructureNode.ChildCount - 1 downto 0 do
  begin
    Result := ContainsText(UpperCase(pStructureNode.Child[lCounter].Caption), TConsts.cClass);
    if Result then
    begin
      Break;
    end;
  end;
end;

function TFreedomProject.ExtractStructureNode(pContext: IOTAStructureContext): IOTAStructureNode;
var
  lCounter: Integer;
begin
  Result := nil;
  for lCounter := pContext.RootNodeCount - 1 downto 0 do
  begin
    if ContainsText(pContext.GetRootStructureNode(lCounter).Caption, TConsts.cUnit) then
    begin
      Result := pContext.GetRootStructureNode(lCounter);
      Break;
    end;
  end;
end;

function TFreedomProject.FindUnitByName(pUnitName: String): TFreedomUnit;
var
  lUnit: TFreedomUnit;
begin
  Result := nil;
  for lUnit in Self do
  begin
    if SameText(lUnit.Name, pUnitName) then
    begin
      Result := lUnit;
      Break;
    end;
  end;
end;

class procedure TFreedomProject.DestroyInstance;
begin
  (BorlandIDEServices as IOTAProjectFileStorage).RemoveNotifier(FInstanceIndex);
  FreeAndNil(FInstance);
end;

class function TFreedomProject.GetInstance: TFreedomProject;
begin
  if not Assigned(FInstance) then
  begin
    FInstance := TFreedomProject.Create;
    FInstanceIndex := (BorlandIDEServices as IOTAProjectFileStorage).AddNotifier(FInstance);
  end;
  Result := FInstance;
end;

function TFreedomProject.GetName: string;
begin
  Result := ClassName;
end;

procedure TFreedomProject.ProjectClosing(const ProjectOrGroup: IOTAModule);
begin
  Clear;
end;

procedure TFreedomProject.ProjectLoaded(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
begin
end;

procedure TFreedomProject.ProjectSaving(const ProjectOrGroup: IOTAModule; const Node: IXMLNode);
begin
end;

procedure TFreedomProject.Update(const pContext: IOTAStructureContext);
var
  lStructureNode: IOTAStructureNode;
  lUnit: TFreedomUnit;

  lHasClasses: Boolean;
  lUnitName: string;

begin
  lStructureNode := ExtractStructureNode(pContext);
  if Assigned(lStructureNode) then
  begin
    lHasClasses := ExtractHasClasses(lStructureNode);
    if lHasClasses then
    begin
      lUnitName := RightStr(lStructureNode.Caption, Length(lStructureNode.Caption) - (Length(TConsts.cUnit) + 1));
      lUnit := FindUnitByName(lUnitName);
      if not Assigned(lUnit) then
      begin
        lUnit := TFreedomUnit.Create(lUnitName);
        Add(lUnit);
      end;
      lUnit.Update(lStructureNode);
    end;
  end;
end;

{ TFreedomUnit }

procedure TFreedomUnit.BeginUpdate;
var
  lClass: TFreedomClass;
begin
  for lClass in Self do
  begin
    lClass.BeginUpdate;
  end;
end;

constructor TFreedomUnit.Create(const pName: string);
begin
  inherited Create;
  FName := pName;
end;

destructor TFreedomUnit.Destroy;
begin

  inherited;
end;

procedure TFreedomUnit.EndUpdate;
var
  lCounter: Integer;
begin
  for lCounter := Count - 1 downto 0 do
  begin
    if not Items[lCounter].Updated then
    begin
      Items[lCounter].NotifyFreeParent(Items[lCounter]);
      Delete(lCounter);
    end else
    begin
      Items[lCounter].EndUpdate;
    end;
  end;
end;

function TFreedomUnit.FindClassByName(pClassName: String): TFreedomClass;
var
  lClass: TFreedomClass;
begin
  Result := nil;
  for lClass in Self do
  begin
    if SameText(lClass.Name, pClassName) then
    begin
      Result := lClass;
      Break;
    end;
  end;
end;

function TFreedomUnit.FindClassNode(const pStructureNode: IOTAStructureNode): IOTAStructureNode;
var
  lCounter: Integer;
begin
  Result := nil;
  for lCounter := pStructureNode.ChildCount - 1 downto 0 do
  begin
    if UpperCase(pStructureNode.Child[lCounter].Caption) = TConsts.cClass then
    begin
      Result := pStructureNode.Child[lCounter];
      Break;
    end;
  end;
end;

//procedure TFreedomUnit.NotifyAddClass(const pClass: TFreedomClass);
//var
//  lI: Integer;
//  lClass: TFreedomClass;
//begin
//  for lClass in Self do
//  begin
//    if not Assigned(pClass.Parent) and (lClass.Name = pClass.ParentName)  then
//    begin
//      pClass.Parent := lClass;
//    end;
//  end;
//end;

//procedure TFreedomUnit.NotifyFreeClass(const pClass: TFreedomClass);
//var
//  lI: Integer;
//  lClass: TFreedomClass;
//begin
//  for lClass in Self do
//  begin
//    lClass.NotifyFreeParent(pClass);
//  end;
//end;

procedure TFreedomUnit.Update(const pStructureNode: IOTAStructureNode);
var
  lClassNode: IOTAStructureNode;
  lCurrentClassName: string;
  lCounter: Integer;
  lCurrentClass: TFreedomClass;
begin
  lClassNode := FindClassNode(pStructureNode);
  if Assigned(lClassNode) then
  begin
    for lCounter := 0 to lClassNode.ChildCount - 1 do
    begin
      lCurrentClassName := LeftStr(lClassNode.Child[lCounter].Caption, Pos('(', lClassNode.Child[lCounter].Caption) - 1);
      lCurrentClass := FindClassByName(lCurrentClassName);
      if not Assigned(lCurrentClass) then
      begin
        lCurrentClass := TFreedomClass.Create(lCurrentClassName, nil);
        Add(lCurrentClass);
      end;
      lCurrentClass.Update(lClassNode.Child[lCounter]);
    end;
  end;
end;

{ TFreedomClass }

procedure TFreedomClass.BeginUpdate;
var
  lProperty: TFreedomProperty;
begin
  FUpdated := False;
  for lProperty in Self do
  begin
    lProperty.BeginUpdate;
  end;
end;

constructor TFreedomClass.Create(const pName: string; pParent: TFreedomClass);
begin
  FName := pName;
  FParent := pParent;
end;

procedure TFreedomClass.EndUpdate;
var
  lCounter: Word;
begin
  for lCounter := Count - 1 downto 0 do
  begin
    if not Items[lCounter].Updated then
    begin
      Delete(lCounter);
    end;
  end;
end;

function TFreedomClass.FindPropertyByName(pPropertyName: String): TFreedomProperty;
var
  lProperty: TFreedomProperty;
begin
  Result := nil;
  for lProperty in Self do
  begin
    if SameText(lProperty.Name, pPropertyName) then
    begin
      Result := lProperty;
      Break;
    end;
  end;
end;

function TFreedomClass.Inherits(pClassName: string): Boolean;
begin
  Result := False;
  if Assigned(FParent) then
  begin
    Result := pClassName = FParent.Name;
  end;
end;

procedure TFreedomClass.NotifyFreeParent(const pClass: TFreedomClass);
begin
  if FParent = pClass then
  begin
    FParent := nil;
  end;
end;

procedure TFreedomClass.Update(const pStructureNode: IOTAStructureNode);
var
  lCounter: Word;
  lParentName: string;
  lParentModified: Boolean;
  lCurrentNode: IOTAStructureNode;
  lPropertyNode: IOTAStructureNode;
  lPropertyName: string;
  lPropCounter: Integer;
  lClassProp: TFreedomProperty;

begin
  FUpdated := True;

  lParentName := RightStr(pStructureNode.Caption, Length(pStructureNode.Caption) - Pos('(', pStructureNode.Caption));
  lParentName := LeftStr(lParentName, Length(lParentName) - 1);
  lParentModified := (FParent <> nil) and (FParent.Name <> lParentName);

  if lParentModified then
  begin
    FParent := nil;
  end;

  for lCounter := pStructureNode.ChildCount -1 downto 0 do
  begin
    if (UpperCase(pStructureNode.Child[lCounter].Caption) = TConsts.cPublic) or
      (UpperCase(pStructureNode.Child[lCounter].Caption) = TConsts.cPublished) then
    begin
      lCurrentNode := pStructureNode.Child[lCounter];

      for lPropCounter := 0 to lCurrentNode.ChildCount - 1 do
      begin
        if lCurrentNode.Child[lPropCounter].ImageIndex <> cPropertyID then
          Continue;
        lPropertyNode := lCurrentNode.Child[lPropCounter];
        lPropertyName := LeftStr(lPropertyNode.Caption, Pos(':', lPropertyNode.Caption) - 1);
        if lPropertyName = '' then
          Continue;
        lClassProp := FindPropertyByName(lPropertyName);
        if not Assigned(lClassProp) then
        begin
          lClassProp := TFreedomProperty.Create(lPropertyName);
          Add(lClassProp);
        end;
        lClassProp.Update;
      end;
    end;
  end;

end;

{ TFreedomProperty }

procedure TFreedomProperty.BeginUpdate;
begin
  FUpdated := False;
end;

constructor TFreedomProperty.Create(pName: String);
begin
  Name := pName;
end;

procedure TFreedomProperty.Update;
begin
  FUpdated := True;
end;


initialization

finalization
  TFreedomNotifier.UnregisterNotifier;

end.


