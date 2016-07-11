unit AM.Freedom.FreedomDBObjectUnitMenuItem;

interface

uses
  AM.Freedom.CustomMenuItem,
  System.Classes,
  ToolsAPI,
  AM.Freedom.FreedomObjectUnit,
  AM.Freedom.frmNewDBObjectEditor;

type
  TFreedomDBObjectUnitMenuItem = class(TCustomMenuItem)
  strict private
    class var FFreedomDBObjectUnitMenuItem: TFreedomDBObjectUnitMenuItem;
  strict protected
    function GetCaption: string; override;
    procedure DoMenuClick(Sender: TObject); override;
    function GetShortCut: TShortCut; override;
  public
    class function GetInstance: TFreedomDBObjectUnitMenuItem;
    class procedure DestroyInstance;
  end;

implementation

uses
  AM.Freedom.frmNewClass,
  AM.Freedom.FreedomObjectDescriptor,
  AM.Freedom.frmNewUnit,
  AM.Freedom.dclFreedomORMConfig;

{ TFreedomObjectUnitMenuItem }

class procedure TFreedomDBObjectUnitMenuItem.DestroyInstance;
begin
  if Assigned(FFreedomDBObjectUnitMenuItem) then
  begin
    FFreedomDBObjectUnitMenuItem.Free;
  end;
end;

procedure TFreedomDBObjectUnitMenuItem.DoMenuClick(Sender: TObject);
var
  lCurrentProject: IOTAProject;
  lIDE: IOTAModuleServices;
  lDescriptor: TFreedomProjectDescriptor;
  lCreator: IOTAModuleCreator;
  lUnit: TFreedomUnitDescriptor;
begin
  lIDE := (BorlandIDEServices as IOTAModuleServices);
  try
    lCurrentProject := lIDE.GetActiveProject;
    try
      if Assigned(lCurrentProject) then
      begin
        lDescriptor := TfrmNewDBObjectEditor.CreateNewDBUnit;
        if (Assigned(lDescriptor)) then
        begin
          for lUnit in lDescriptor do
          begin
            lCreator := TFreedomObjectUnitCreator.Create;
            TFreedomObjectUnitCreator(lCreator).UnitDescriptor := lUnit;
            lIDE.CreateModule(lCreator);
            lCurrentProject.AddFile(lCreator.ImplFileName, True);
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

function TFreedomDBObjectUnitMenuItem.GetCaption: string;
begin
  Result := 'New Freedom DB Object';
end;

class function TFreedomDBObjectUnitMenuItem.GetInstance: TFreedomDBObjectUnitMenuItem;
begin
  if not Assigned(FFreedomDBObjectUnitMenuItem) then
  begin
    FFreedomDBObjectUnitMenuItem := TFreedomDBObjectUnitMenuItem.Create;
  end;
  Result := FFreedomDBObjectUnitMenuItem;
end;

function TFreedomDBObjectUnitMenuItem.GetShortCut: TShortCut;
begin
  Result := TdclFreedomORMConfig.GetInstance.NewFreedomDBObject;
end;

end.
