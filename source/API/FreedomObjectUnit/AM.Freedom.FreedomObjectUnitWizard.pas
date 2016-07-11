unit AM.Freedom.FreedomObjectUnitWizard;

interface

uses
  ToolsAPI,
  AM.Freedom.CustomFreedomObjectUnitWizard;

type
  TFreedomObjectUnitWizard = class(TCustomFreedomObjectUnitWizard)
  public
    function GetName: string; override;
    procedure Execute; override;
    class procedure ExecuteNewFreedomObject;
    function GetComment: string; override;
  end;

implementation

{ TFreedomObjectUnitWizard }

uses
  AM.Freedom.frmNewUnit,
  AM.Freedom.FreedomObjectUnit,
  AM.Freedom.FreedomObjectDescriptor;

procedure TFreedomObjectUnitWizard.Execute;
begin
  ExecuteNewFreedomObject;
end;

class procedure TFreedomObjectUnitWizard.ExecuteNewFreedomObject;
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
          TFreedomObjectUnitCreator(lCreator).UnitDescriptor := lDescriptor;
          lIDE.CreateModule(lCreator);
          lCurrentProject.AddFile(lCreator.ImplFileName, True);
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

function TFreedomObjectUnitWizard.GetComment: string;
begin
  Result := 'Create new Unit with a descendenting Freedom Object class.' + sLineBreak +
    'You can create a new classes inherited of TFreedomObject and yours generics' +
    ' list classes inherited of TFreedomObjectList<T>.' + sLineBreak +
    'You can specify property and mapped fields(Attributes(Column, Id, Domain, Order, Schema, Default Values))';
end;

function TFreedomObjectUnitWizard.GetName: string;
begin
  Result := 'Freedom Object';
end;

end.
