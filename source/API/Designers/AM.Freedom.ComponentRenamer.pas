unit AM.Freedom.ComponentRenamer;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  DesignIntf,
  System.Rtti,
  System.Classes,
  Data.DB,
  AM.Freedom.frmRenameComponent,
  AM.Freedom.RenameParams;

type
  TFreedomComponentRenamer = class sealed
  private
    class var FFixComponents: Boolean;
    FComponents: TList<TComponent>;
    FIsSelectionEditor: Boolean;
    FComponent: TComponent;

    function IsClassName(pClass: TClass; pIgnoredClassName: String): Boolean;
    function ExtractCaptionProperty: TRttiProperty;

    procedure FixComponentSelection(pDesignerSelections: IDesignerSelections);
    procedure AddInternalComponents(pComponent: TComponent);

    procedure DoRename;
    function IsIgnoredClass(pMetaClass: TClass): Boolean;
    function IsUnNamedComponent: Boolean;
    procedure MakeRename;
    function ConfigurePrefix(pMetaClass: TClass): string;
    function FindPrefixByClassType(pMetaClass: TClass): String;
    procedure DoRenameComponent(pPrefix: String);
    function CreateRenameParams(pPrefix: String): TRenameParams;
    function HasCaption: Boolean;
    function ExtractCaption: String;
    procedure InputCaption(pCaption: String);
    procedure AddUnNamedIfInternalIgnoredClass;
    function IsForm(pClass: TClass): Boolean;
    function IsFrame(pClass: TClass): Boolean;
    function IsDataModule(pClass: TClass): Boolean;
    procedure AddUnNamedComponent(pComponent: TComponent);
  public
    class procedure RenameComponents(pDesignerSelections: IDesignerSelections; pIsSelectionEditor: Boolean = False);
    class procedure RemoveUnNamedComponent(pComponent: TComponent);
    destructor Destroy; override;
  end;


implementation

uses
  System.StrUtils,
  AM.Freedom.ComponentRenamerConfig,
  AM.Freedom.ComponentRenamer.PropertiesChange,
  System.TypInfo,
  AM.Freedom.frmDisplayFieldsEditor,
  Vcl.Controls;

{ TFreedomComponentRenamer }

procedure TFreedomComponentRenamer.AddUnNamedComponent(pComponent: TComponent);
var
  lIndex: Integer;
begin
  TComponentRenamerConfig.AddUnNamedComponent(pComponent);
  if (pComponent.ComponentCount > 0) then
  begin
    for lIndex := 0 to pComponent.ComponentCount - 1 do
    begin
      TComponentRenamerConfig.AddUnNamedComponent(pComponent.Components[lIndex]);
    end;
  end;
end;

destructor TFreedomComponentRenamer.Destroy;
begin
  FreeAndNil(FComponents);
  inherited;
end;

procedure TFreedomComponentRenamer.AddUnNamedIfInternalIgnoredClass;
begin
  if not IsUnNamedComponent and (IsForm(FComponent.ClassType) or IsFrame(FComponent.ClassType) or IsDataModule(FComponent.ClassType)) then
  begin
    AddUnNamedComponent(FComponent);
  end;
end;

function TFreedomComponentRenamer.ConfigurePrefix(pMetaClass: TClass): string;
begin
  Result := FindPrefixByClassType(pMetaClass);
  if (Result = '') then
  begin
    Result := TComponentRenamerConfig.ConfigurePrefix(pMetaClass);
  end;
end;

function TFreedomComponentRenamer.CreateRenameParams(pPrefix: String): TRenameParams;
begin
  Result := TRenameParams.Create;
  Result.RenameClassName := FComponent.ClassName;
  Result.Name := FComponent.Name;
  Result.HasCaption := HasCaption;
  Result.Prefix := pPrefix;
  if (Result.HasCaption) then
  begin
    Result.Caption := ExtractCaption;
  end;
end;

procedure TFreedomComponentRenamer.DoRename;
var
  lComponent: TComponent;
begin
  for lComponent in FComponents do
  begin
    FComponent := lComponent;
    if (not IsIgnoredClass(FComponent.ClassType)) and (FIsSelectionEditor or (not IsUnNamedComponent)) then
    begin
      MakeRename;
    end
    else
    begin
      AddUnNamedIfInternalIgnoredClass;
    end;
  end;
end;

procedure TFreedomComponentRenamer.DoRenameComponent(pPrefix: String);
var
  lRepeat: Boolean;
  lParams: TRenameParams;
  lProperties: TPropertiesChange;
begin
  if (FComponent.Name <> '') then
  begin
    lRepeat := True;
    repeat
      lParams := CreateRenameParams(pPrefix);
      lProperties := TfrmRenameComponent.RenameComponent(lParams);
      try
        if (lProperties.Name <> '') then
        begin
          try
            if FComponent.Name = lProperties.Name then
            begin
              AddUnNamedComponent(FComponent);
            end;
            FComponent.Name := lProperties.Name;
            if (lParams.HasCaption) then
            begin
              InputCaption(lProperties.Caption);
            end;
            lRepeat := False;
          except
          end;
        end
        else
        begin
          AddUnNamedComponent(FComponent);
          lRepeat := False;
        end;
      finally
        FreeAndNil(lProperties);
        FreeAndNil(lParams);
      end;
    until not lRepeat;
  end;
end;

function TFreedomComponentRenamer.ExtractCaption: String;
var
  lProperty: TRttiProperty;
begin
  lProperty := ExtractCaptionProperty;
  if (Assigned(lProperty)) then
  begin
    Result := lProperty.GetValue(FComponent).AsString;
  end;
end;

function TFreedomComponentRenamer.ExtractCaptionProperty: TRttiProperty;
var
  lContext: TRttiContext;
  lType: TRttiType;
  lProperty: TRttiProperty;
begin
  lType := lContext.GetType(FComponent.ClassInfo);
  lProperty := lType.GetProperty('Caption');
  Result := nil;
  if (Assigned(lProperty)) then
  begin
    if lProperty.Visibility = mvPublished then
    begin
      Result := lProperty;
    end;
  end;
end;

function TFreedomComponentRenamer.FindPrefixByClassType(pMetaClass: TClass): String;
begin
  Result := TComponentRenamerConfig.FindClassPrefix(pMetaClass);
end;

procedure TFreedomComponentRenamer.FixComponentSelection(pDesignerSelections: IDesignerSelections);
var
  lIndex: Integer;
  lItem: TPersistent;
begin
  if (not Assigned(FComponents)) then
  begin
    FComponents := TList<TComponent>.Create;
  end;
  FComponents.Clear;
  if (Assigned(pDesignerSelections) and (pDesignerSelections.Count > 0)) then
  begin
    for lIndex := 0 to pDesignerSelections.Count - 1 do
    begin
      try
        lItem := pDesignerSelections.Items[lIndex];
        if (lItem is TComponent) then
        begin
          if (Trim(TComponent(lItem).Name) <> '') and (not FComponents.Contains(TComponent(lItem))) then
          begin
            FComponents.Add(TComponent(lItem));
            AddInternalComponents(TComponent(lItem));
          end;
        end;
      except
        Continue;
      end;
    end;
  end;
end;

function TFreedomComponentRenamer.HasCaption: Boolean;
begin
  Result := ExtractCaptionProperty <> nil;
end;

procedure TFreedomComponentRenamer.InputCaption(pCaption: String);
var
  lProperty: TRttiProperty;
begin
  lProperty := ExtractCaptionProperty;
  if (Assigned(lProperty)) then
  begin
    lProperty.SetValue(FComponent, TValue.From<String>(pCaption));
  end;
end;

function TFreedomComponentRenamer.IsDataModule(pClass: TClass): Boolean;
begin
  Result := IsClassName(pClass, 'TDataModule');
end;

function TFreedomComponentRenamer.IsForm(pClass: TClass): Boolean;
begin
  Result := IsClassName(pClass, 'TCustomForm');
end;

function TFreedomComponentRenamer.IsFrame(pClass: TClass): Boolean;
begin
  Result := IsClassName(pClass, 'TCustomFrame');
end;

function TFreedomComponentRenamer.IsIgnoredClass(pMetaClass: TClass): Boolean;
begin
  Result := TComponentRenamerConfig.IsIgnoredClass(pMetaClass);
end;

function TFreedomComponentRenamer.IsClassName(pClass: TClass; pIgnoredClassName: String): Boolean;
var
  lClass: TClass;
begin
  Result := False;
  lClass := pClass;
  while (not Result) and Assigned(lClass) do
  begin
    Result := lClass.ClassNameIs(pIgnoredClassName);
    lClass := lClass.ClassParent;
  end;
end;

function TFreedomComponentRenamer.IsUnNamedComponent: Boolean;
begin
  Result := TComponentRenamerConfig.IsUnNamedComponent(FComponent);
end;

procedure TFreedomComponentRenamer.MakeRename;
var
  lPrefix: string;
begin
  lPrefix := ConfigurePrefix(FComponent.ClassType);
  if (not IsIgnoredClass(FComponent.ClassType)) then
  begin
    if (lPrefix = '') or ((not StartsStr(lPrefix, FComponent.Name)) or FIsSelectionEditor) then
    begin
      DoRenameComponent(lPrefix);
    end;
  end;
end;

class procedure TFreedomComponentRenamer.RemoveUnNamedComponent(pComponent: TComponent);
begin
  TComponentRenamerConfig.RemoveUnNamedComponent(pComponent);
end;

class procedure TFreedomComponentRenamer.RenameComponents(pDesignerSelections: IDesignerSelections; pIsSelectionEditor: Boolean);
var
  lFreedomComponentRenamer: TFreedomComponentRenamer;
begin
  if (not FFixComponents) then
  begin
    FFixComponents := True;
    try
      lFreedomComponentRenamer := TFreedomComponentRenamer.Create;
      try
        lFreedomComponentRenamer.FixComponentSelection(pDesignerSelections);
        lFreedomComponentRenamer.FIsSelectionEditor := pIsSelectionEditor;
        lFreedomComponentRenamer.DoRename;
      finally
        lFreedomComponentRenamer.Free;
      end;
    finally
      FFixComponents := False
    end;
  end;
end;

procedure TFreedomComponentRenamer.AddInternalComponents(pComponent: TComponent);
var
  lWinControl: TWinControl;
  lIndex: Integer;
  lControl: TControl;
begin
  if (pComponent.InheritsFrom(TWinControl)) then
  begin
    lWinControl := TWinControl(pComponent);
    for lIndex := 0 to lWinControl.ControlCount - 1 do
    begin
      lControl := lWinControl.Controls[lIndex];
      if (lControl.InheritsFrom(TComponent)) and (not FComponents.Contains(TComponent(lControl)))
         and (TComponent(lControl).Name <> '') then
      begin
        FComponents.Add(TComponent(lControl));
        AddInternalComponents(TComponent(lControl));
      end;
    end;
  end;
end;

end.
