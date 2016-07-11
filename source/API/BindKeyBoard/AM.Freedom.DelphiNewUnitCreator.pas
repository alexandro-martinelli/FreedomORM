unit AM.Freedom.DelphiNewUnitCreator;

interface

uses
  ToolsAPI;

type
  TDelphiNewUnitCreator = class(TInterfacedObject, IOTAModuleCreator)
  strict private
    function GenerateSource: String;
  private
    FUnitName: string;
    function GetAncestorName: string;
    function GetFormName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    function NewFormFile(const FormIdent: string; const AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent: string; const FormIdent: string; const AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent: string; const FormIdent: string; const AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  public
    constructor Create(pUnitName: String);
  end;

implementation

uses
  AM.Freedom.FreedomObjectUnit,
  System.StrUtils, System.SysUtils;

{ TDelphiNewUnitCreator }

constructor TDelphiNewUnitCreator.Create(pUnitName: String);
begin
  FUnitName := pUnitName;
end;

procedure TDelphiNewUnitCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

function TDelphiNewUnitCreator.GenerateSource: String;
const
  sDoubleLineBreak = sLineBreak + sLineBreak;
begin
  Result :=
    'unit ' + FUnitName + ';' + sDoubleLineBreak +
    'interface' + sDoubleLineBreak +
    'uses' + sLineBreak +
    '  System.SysUtils;' + sDoubleLineBreak +
    'type' + sDoubleLineBreak + sLineBreak +
    'implementation' + sDoubleLineBreak +
    'end.';
end;

function TDelphiNewUnitCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TDelphiNewUnitCreator.GetCreatorType: string;
begin
end;

function TDelphiNewUnitCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TDelphiNewUnitCreator.GetFileSystem: string;
begin
end;

function TDelphiNewUnitCreator.GetFormName: string;
begin
  Result := '';
end;

function TDelphiNewUnitCreator.GetImplFileName: string;
var
  lCurrentProject: IOTAProject;
begin
  Result := FUnitName;
  if not EndsText('.pas', Result) then
  begin
    Result := Result + '.pas';
  end;
  if ExtractFilePath(Result) = '' then
  begin
    lCurrentProject := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
    Result := ExtractFilePath(lCurrentProject.FileName) + Result;
  end;
end;

function TDelphiNewUnitCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TDelphiNewUnitCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TDelphiNewUnitCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TDelphiNewUnitCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TDelphiNewUnitCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TDelphiNewUnitCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TDelphiNewUnitCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TDelphiNewUnitCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := TFreedomObjectUnit.Create;
  TFreedomObjectUnit(Result).FreedomSource := GenerateSource;
end;

function TDelphiNewUnitCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

end.
