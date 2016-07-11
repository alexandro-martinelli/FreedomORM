unit AM.Freedom.RegDesigners;

interface

uses
  System.Classes,
  System.SysUtils,
  ToolsAPI;

procedure Register;

implementation

uses
  DesignIntf,
  Data.DB,
  AM.Freedom.DesignNotificator,
  AM.Freedom.RenameComponentEditor,
  AM.Freedom.ComponentNameEditor,
  AM.Freedom.dclFreedomORMConfig,
  AM.Freedom.FreedomORMMenuItem,
  AM.Freedom.FreedomORMOptionsMenuItem;

procedure Register;
begin
  if (TdclFreedomORMConfig.GetInstance.ActiveRename) then
  begin
    RegisterDesignNotification(TFreedomDesignNotificator.GetInstance);
    RegisterSelectionEditor(TComponent, TFreedomRenameComponentEditor);
    RegisterPropertyEditor(TypeInfo(TComponentName), TComponent, 'Name', TFreedomComponentNameEditor);
  end;
end;

initialization

finalization
  if (TFreedomDesignNotificator.TryGetInstance <> nil) then
  begin
    UnregisterDesignNotification(TFreedomDesignNotificator.GetInstance);
    TFreedomDesignNotificator.DestroyInstance;
  end;
end.
