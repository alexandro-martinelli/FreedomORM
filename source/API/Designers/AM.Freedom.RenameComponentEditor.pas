unit AM.Freedom.RenameComponentEditor;

interface

uses
  System.SysUtils,
  System.Classes,
  DesignEditors,
  DesignIntf;

type
  TFreedomRenameComponentEditor = class(TSelectionEditor)
  public
    procedure ExecuteVerb(Index: Integer; const List: IDesignerSelections); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

{ TFreedomRenameComponentEditor }

uses
  AM.Freedom.ComponentRenamer;

procedure TFreedomRenameComponentEditor.ExecuteVerb(Index: Integer; const List: IDesignerSelections);
begin
  if Assigned(List) then
  begin
    TFreedomComponentRenamer.RenameComponents(List, True);
  end;
end;

function TFreedomRenameComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Rename Selectd(s)';
end;

function TFreedomRenameComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

end.
