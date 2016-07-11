unit AM.Freedom.ComponentNameEditor;

interface

uses
  System.SysUtils,
  DesignEditors,
  DesignIntf;

type
  TFreedomComponentNameEditor = class(TComponentNameProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;


implementation

{ TFreedomComponentNameEditor }

uses
  AM.Freedom.ComponentRenamer;

procedure TFreedomComponentNameEditor.Edit;
var
  lList: IDesignerSelections;
begin
  lList := CreateSelectionList;
  lList.Add(GetComponent(0));
  TFreedomComponentRenamer.RenameComponents(lList, True);
end;

function TFreedomComponentNameEditor.GetAttributes: TPropertyAttributes;
begin
  Result := Inherited + [paDialog]
end;

end.
