unit AM.Freedom.RegFreedomObjectUnit;

interface

uses
  ToolsAPI;

procedure Register;

implementation

uses
  System.StrUtils,
  Vcl.Menus,
  AM.Freedom.FreedomObjectUnitWizard,
  AM.Freedom.FreedomDBObjectUnitWizard,
  AM.Freedom.FreedomDBObjectUnitMenuItem,
  AM.Freedom.FreedomORMMenuItem,
  AM.Freedom.FreedomObjectUnitMenuItem, AM.Freedom.FreedomORMOptionsMenuItem;

procedure Register;
var
  lParentMenu: TMenuItem;
  lIndex: Integer;
begin
  lParentMenu := (BorlandIDEServices as INTAServices).MainMenu.Items;
  lIndex := lParentMenu.IndexOf(lParentMenu.Find('Window'));
  lParentMenu.Insert(lIndex, TFreedomORMMenuItem.GetInstance.MenuItem);
  TFreedomORMMenuItem.GetInstance.MenuItem.Add(TFreedomObjectUnitMenuItem.GetInstance.MenuItem);
  TFreedomORMMenuItem.GetInstance.MenuItem.Add(TFreedomDBObjectUnitMenuItem.GetInstance.MenuItem);
  TFreedomORMMenuItem.GetInstance.MenuItem.Add(TFreedomORMOptionsMenuItem.GetInstance.MenuItem);
  RegisterPackageWizard(TFreedomObjectUnitWizard.Create);
  RegisterPackageWizard(TFreedomDBObjectUnitWizard.Create);
end;

end.
