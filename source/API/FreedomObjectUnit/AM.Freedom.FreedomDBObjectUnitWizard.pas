unit AM.Freedom.FreedomDBObjectUnitWizard;

interface

uses
  ToolsAPI,
  AM.Freedom.CustomFreedomObjectUnitWizard;

type
  TFreedomDBObjectUnitWizard = class(TCustomFreedomObjectUnitWizard)
  public
    function GetName: string; override;
    procedure Execute; override;
    class procedure ExecuteFreedomDBObjectUnit;
    function GetComment: string; override;
  end;

implementation

uses
  AM.Freedom.FreedomObjectDescriptor,
  AM.Freedom.frmNewDBObjectEditor, AM.Freedom.FreedomObjectUnit;

{ TFreedomDBObjectUnitWizard }

procedure TFreedomDBObjectUnitWizard.Execute;
begin
  ExecuteFreedomDBObjectUnit;
end;

class procedure TFreedomDBObjectUnitWizard.ExecuteFreedomDBObjectUnit;
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

function TFreedomDBObjectUnitWizard.GetComment: string;
begin
  Result := 'Create new Unit with a descendenting Freedom Object class with a database especification.' + sLineBreak + 'You can create a new classes inherited of TFreedomObject and yours generics' + ' list classes inherited of TFreedomObjectList<T>.' +
    sLineBreak + 'This wizard gives to you a database objects for contructing your classes and units.';
end;

function TFreedomDBObjectUnitWizard.GetName: string;
begin
  Result := 'Freedom DBObject';
end;

end.
