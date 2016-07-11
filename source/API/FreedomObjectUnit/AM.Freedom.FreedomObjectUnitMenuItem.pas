unit AM.Freedom.FreedomObjectUnitMenuItem;

interface

uses
  AM.Freedom.CustomMenuItem,
  System.Classes,
  ToolsAPI,
  AM.Freedom.FreedomObjectUnit;

type
  TFreedomObjectUnitMenuItem = class(TCustomMenuItem)
  strict private
    class var FFreedomObjectUnitMenuItem: TFreedomObjectUnitMenuItem;
  strict protected
    function GetCaption: string; override;
    procedure DoMenuClick(Sender: TObject); override;
    function GetShortCut: TShortCut; override;
  public
    class function GetInstance: TFreedomObjectUnitMenuItem;
    class procedure DestroyInstance;
  end;

implementation

uses
  AM.Freedom.frmNewClass,
  AM.Freedom.FreedomObjectDescriptor,
  AM.Freedom.frmNewUnit,
  AM.Freedom.dclFreedomORMConfig;

{ TFreedomObjectUnitMenuItem }

class procedure TFreedomObjectUnitMenuItem.DestroyInstance;
begin
  if Assigned(FFreedomObjectUnitMenuItem) then
  begin
    FFreedomObjectUnitMenuItem.Free;
  end;
end;

procedure TFreedomObjectUnitMenuItem.DoMenuClick(Sender: TObject);
var
  lCurrentProject: IOTAProject;
  lIDE: IOTAModuleServices;
  lDescriptor: TFreedomUnitDescriptor;
  lCreator: IOTAModuleCreator;
begin
  lIDE := (BorlandIDEServices as IOTAModuleServices);
  try
    lCurrentProject := lIDE.GetActiveProject;
    try
      if Assigned(lCurrentProject) then
      begin
        lDescriptor := TfrmNewUnit.CreateNewUnit;
        if (Assigned(lDescriptor)) then
        begin
          lCreator := TFreedomObjectUnitCreator.Create;
          try
            TFreedomObjectUnitCreator(lCreator).UnitDescriptor := lDescriptor;
            lIDE.CreateModule(lCreator);
            lCurrentProject.AddFile(lCreator.ImplFileName, True);
          finally
            lCreator := nil;
          end;
        end;
      end;
    finally
      lCreator := nil;
      lCurrentProject := nil;
    end;
  finally
    lIDE := nil;
  end;
end;

function TFreedomObjectUnitMenuItem.GetCaption: string;
begin
  Result := 'New Freedom Object';
end;

class function TFreedomObjectUnitMenuItem.GetInstance: TFreedomObjectUnitMenuItem;
begin
  if not Assigned(FFreedomObjectUnitMenuItem) then
  begin
    FFreedomObjectUnitMenuItem := TFreedomObjectUnitMenuItem.Create;
  end;
  Result := FFreedomObjectUnitMenuItem;
end;

function TFreedomObjectUnitMenuItem.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.NewFreedomObject;
end;

end.
