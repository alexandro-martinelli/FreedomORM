unit AM.Freedom.frmObjectUnitEditor;

interface

uses
  ToolsAPI,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  AM.Freedom.frmObjectUnitOptions;

type
  TfrmObjectUnitEditor_old = class(TFrame)
    procedure btnNewObjectUnitClick(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  AM.Freedom.frmNewFreedomObject,
  AM.Freedom.FreedomObjectDescriptor,
  AM.Freedom.frmNewFreedomUnit,
  AM.Freedom.FreedomObjectUnit;

{$R *.dfm}

procedure TfrmObjectUnitEditor_old.btnNewObjectUnitClick(Sender: TObject);
var
  lCurrentProject: IOTAProject;
  lIDE: IOTAModuleServices;
begin
  lIDE := (BorlandIDEServices as IOTAModuleServices);
  try
    lCurrentProject := lIDE.GetActiveProject;
    try
      if Assigned(lCurrentProject) then
      begin
        TfrmNewFreedomUnit.CreateNewUnit(tsNewUnit, Self);
      end;
    finally
      lCurrentProject := nil;
    end;
  finally
    lIDE := nil;
  end;
end;

procedure TfrmObjectUnitEditor_old.btnOptionsClick(Sender: TObject);
begin
  TfrmObjectUnitOptions.ShowOptions;
end;

constructor TfrmObjectUnitEditor_old.Create(AOwner: TComponent);
begin
  inherited;
  tsEditor.TabVisible := False;
  tsNewUnit.TabVisible := False;
  tsNewDBObjectUnit.TabVisible := False;
  tsEditor.Show;
  Name := 'frmObjectUnitEditor' + FormatDateTime('DDMMYYYHHNNSSzzzz', Now);
end;

procedure TfrmObjectUnitEditor_old.Notification(AComponent: TComponent; Operation: TOperation);
var
  lDescriptor: TFreedomUnitDescriptor;
  lObjectUnit: IOTAModuleCreator;
begin
  if (Operation = opRemove) and (AComponent is TfrmNewFreedomUnit) and (not (csDestroying in ComponentState)) then
  begin
    lDescriptor := TfrmNewFreedomUnit(AComponent).FreedomUnitDescriptor;
    if Assigned(lDescriptor) then
    begin
      lObjectUnit := TFreedomObjectUnitCreator.Create;
      TFreedomObjectUnitCreator(lObjectUnit).UnitDescriptor := lDescriptor;
      (BorlandIDEServices as IOTAModuleServices).CreateModule(lObjectUnit);
      (BorlandIDEServices as IOTAModuleServices).GetActiveProject.AddFile(lObjectUnit.ImplFileName, True);
    end;
    tsEditor.Show;
  end;
  AComponent.RemoveFreeNotification(Self);
  inherited;
end;

end.
