unit AM.Freedom.DesignNotificator;

interface

uses
  System.SysUtils,
  DesignIntf,
  System.Classes,
  AM.Freedom.InterfacedObjects;

type
  TFreedomDesignNotificator = class(TFreedomInterfacedObject, IDesignNotification)
  strict private
    class var FDesignNotificator: TFreedomDesignNotificator;
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
    procedure ItemsModified(const ADesigner: IDesigner);
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections);
    procedure DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
    procedure ValidateNameForSelectedComponents(pDesigner: IDesigner);
  private
    FIsInserting: Boolean;
  public
    class function GetInstance: TFreedomDesignNotificator;
    class function TryGetInstance: TFreedomDesignNotificator;
    class procedure DestroyInstance;
  end;

implementation

uses
  AM.Freedom.ComponentRenamer,
  Data.DB,
  AM.Freedom.frmDisplayFieldsEditor,
  AM.Freedom.ComponentRenamerConfig;

{ TFreedomDesignNotificator }

procedure TFreedomDesignNotificator.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
begin
  if not AGoingDormant and (ADesigner <> nil) and (ADesigner.Root <> nil) then
  begin
    TFreedomComponentRenamer.RemoveUnNamedComponent(ADesigner.Root);
  end;
end;

procedure TFreedomDesignNotificator.DesignerOpened(const ADesigner: IDesigner; AResurrecting: Boolean);
var
  lIndex: Integer;
begin
  if Assigned(ADesigner) and (ADesigner.Root <> nil) then
  begin
    if (ADesigner.Root.ComponentCount > 0) then
    begin
      for lIndex := 0 to ADesigner.Root.ComponentCount - 1 do
      begin
        TComponentRenamerConfig.AddUnNamedComponent(ADesigner.Root.Components[lIndex]);
      end;
    end;
  end;
end;

class procedure TFreedomDesignNotificator.DestroyInstance;
begin
  FreeAndNil(FDesignNotificator);
end;

class function TFreedomDesignNotificator.GetInstance: TFreedomDesignNotificator;
begin
  if (not Assigned(FDesignNotificator)) then
  begin
    FDesignNotificator := TFreedomDesignNotificator.Create;
  end;
  Result := FDesignNotificator;
end;

procedure TFreedomDesignNotificator.ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  if AItem.InheritsFrom(TComponent) then
  begin
    TFreedomComponentRenamer.RemoveUnNamedComponent(TComponent(AItem));
  end;
end;

procedure TFreedomDesignNotificator.ItemInserted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  //Do nothing
end;

procedure TFreedomDesignNotificator.ItemsModified(const ADesigner: IDesigner);
begin
  if (not FIsInserting) then
  begin
    FIsInserting := True;
    try
      if Assigned(ADesigner) then
      begin
        ValidateNameForSelectedComponents(ADesigner);
      end;
    finally
      FIsInserting := False;
    end;
  end;
end;

procedure TFreedomDesignNotificator.SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections);
begin
  // do nothing
end;

class function TFreedomDesignNotificator.TryGetInstance: TFreedomDesignNotificator;
begin
  Result := FDesignNotificator;
end;

procedure TFreedomDesignNotificator.ValidateNameForSelectedComponents(pDesigner: IDesigner);
var
  lDesignerSelections: IDesignerSelections;
begin
  lDesignerSelections := CreateSelectionList;
  try
    if (Assigned(lDesignerSelections)) then
    begin
      pDesigner.GetSelections(lDesignerSelections);
      if (lDesignerSelections.Count > 0) then
      begin
        TFreedomComponentRenamer.RenameComponents(lDesignerSelections);
      end;
    end;
  finally
    lDesignerSelections := nil;
  end;
end;

end.
