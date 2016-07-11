unit AM.Freedom.CustomFreedomObjectUnitWizard;

interface

uses
  ToolsAPI,
  System.SysUtils;


type
  TCustomFreedomObjectUnitWizard = class(TNotifierObject, IOTAWizard, IOTAProjectWizard,
      IOTARepositoryWizard, IUnknown, IOTARepositoryWizard80)
  public
    function GetName: string; virtual; abstract;
    function GetComment: string; virtual; abstract;
    procedure Execute; virtual; abstract;

    function GetIDString: string; virtual;
    function GetState: TWizardState; virtual;
    function GetAuthor: string;
    function GetPage: string; virtual;
    function GetGlyph: Cardinal; virtual;

    function GetDesigner: string;
    function GetGalleryCategory: IOTAGalleryCategory;
    function GetPersonality: string;
  end;

implementation

{ TCustomFreedomObjectUnitWizard }

function TCustomFreedomObjectUnitWizard.GetAuthor: string;
begin
  Result := 'Freedom Soluções em TI';
end;

function TCustomFreedomObjectUnitWizard.GetDesigner: string;
begin
  Result := dAny;
end;

function TCustomFreedomObjectUnitWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  Result := nil;
end;

function TCustomFreedomObjectUnitWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

function TCustomFreedomObjectUnitWizard.GetIDString: string;
begin
  Result := ClassName + 'ID';
end;

function TCustomFreedomObjectUnitWizard.GetPage: string;
begin
  Result := 'Freedom ORM';
end;

function TCustomFreedomObjectUnitWizard.GetPersonality: string;
begin
  Result := 'Delphi.Personality';
end;

function TCustomFreedomObjectUnitWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

end.
