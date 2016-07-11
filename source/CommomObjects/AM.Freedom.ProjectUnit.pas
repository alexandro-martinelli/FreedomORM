unit AM.Freedom.ProjectUnit;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TProjectUnit = class
  private
    FFileName: String;
    FProjectName: String;
  public
    property FileName: String read FFileName write FFileName;
    property ProjectName: String read FProjectName write FProjectName;
  end;

  TProjectUnits = class(TObjectList<TProjectUnit>)
  public
    procedure AddUnit(pFileName, pProjectName: String);
    function Contains(pFileName, pProjectName: string): Boolean;
  end;

implementation

uses
  System.StrUtils;

{ TProjectUnits }

procedure TProjectUnits.AddUnit(pFileName, pProjectName: String);
var
  lUnit: TProjectUnit;
begin
  lUnit := TProjectUnit.Create;
  lUnit.FileName := pFileName;
  lUnit.ProjectName := pProjectName;
  Add(lUnit);
end;

function TProjectUnits.Contains(pFileName, pProjectName: string): Boolean;
var
  lUnit: TProjectUnit;
begin
  Result := False;
  for lUnit in Self do
  begin
    Result := (SameText(pFileName, lUnit.FileName) and SameText(pProjectName, lUnit.ProjectName));
    if (Result) then
    begin
      Break;
    end;
  end;
end;

end.