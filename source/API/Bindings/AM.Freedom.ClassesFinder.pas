unit AM.Freedom.ClassesFinder;

interface

uses
  System.Classes,
  System.IOUtils,
  AM.UnitReader.Members.ClassMember;

type
  TClassesFinder = class sealed
  strict private
    class procedure FindUnitsList(pUnitList: TStrings);
    class procedure DeleteInvalidFiles(pUnitList: TStrings);
  public
    class function GetClasses: TClassList;
  end;

implementation

uses
  ToolsAPI,
  System.StrUtils,
  System.SysUtils,
  AM.UnitReader.Members.UnitMember,
  AM.UnitReader,
  AM.UnitReader.Enumerations;

class procedure TClassesFinder.DeleteInvalidFiles(pUnitList: TStrings);
var
  lCounter: Integer;
  lDfmFile: string;
  lDfmIndex: Integer;
  lFile: string;
  function IsDelphiUnit(pFile: String): Boolean;
  begin
    Result := StartsText('System.', pFile) or StartsText('Data.', pFile) or
       StartsText('Datasnap.', pFile);

  end;
begin
  pUnitList.BeginUpdate;
  try
    lCounter := pUnitList.Count - 1;
    while lCounter >= 0 do
    begin
      if TFile.Exists(pUnitList.Strings[lCounter]) then
      begin
        if EndsText('.pas', pUnitList.Strings[lCounter]) then
        begin
          lFile := pUnitList.Strings[lCounter];
          lDfmFile := ReplaceText(lFile, '.pas', '.dfm');
          lDfmIndex := pUnitList.IndexOf(lDfmFile);
          if lDfmIndex >= 0 then
          begin
            pUnitList.Delete(lDfmIndex);
            pUnitList.Delete(pUnitList.IndexOf(lFile));
            Dec(lCounter, 2);
          end
          else if (IsDelphiUnit(lFile)) then
          begin
            pUnitList.Delete(pUnitList.IndexOf(lFile));
            Dec(lCounter);
          end
          else if not EndsText('.pas', pUnitList.Strings[lCounter]) and not EndsText('.dfm', pUnitList.Strings[lCounter]) then
          begin
            pUnitList.Delete(lCounter);
            Dec(lCounter);
          end
          else
          begin
            Dec(lCounter);
          end;
        end
        else if not EndsText('.pas', pUnitList.Strings[lCounter]) and not EndsText('.dfm', pUnitList.Strings[lCounter]) then
        begin
          pUnitList.Delete(lCounter);
          Dec(lCounter);
        end
        else
        begin
          Dec(lCounter);
        end;
      end
      else
      begin
        pUnitList.Delete(lCounter);
        Dec(lCounter);
      end;
    end;
  finally
    pUnitList.EndUpdate;
  end;
end;

class procedure TClassesFinder.FindUnitsList(pUnitList: TStrings);
var
  lCurrentProject: IOTAProject;
  lIDE: IOTAModuleServices;
  lIndex: Integer;
  lFiles: TStrings;
  lFile: String;
begin
  pUnitList.Clear;
  lCurrentProject := (BorlandIDEServices as IOTAModuleServices).GetActiveProject;
  if Assigned(lCurrentProject) then
  begin
    lIDE := (BorlandIDEServices as IOTAModuleServices);
    try
      lFiles := TStringList.Create;
      try
        for lIndex := 0 to lIDE.ModuleCount - 1 do
        begin
          if (EndsText('.pas', lIDE.Modules[lIndex].FileName) or EndsText('.dfm', lIDE.Modules[lIndex].FileName)) then
          begin
            lIDE.Modules[lIndex].GetAssociatedFilesFromModule(lFiles);
            for lFile in lFiles do
            begin
              pUnitList.Add(lFile);
            end;
            lFiles.Clear;
          end;
        end;
      finally
        lFiles.Free;
      end;
    finally
      lIDE := nil;
    end;
  end;
  if pUnitList.Count > 0 then
  begin
    DeleteInvalidFiles(pUnitList);
  end;
end;

class function TClassesFinder.GetClasses: TClassList;
var
  lFileList: TStrings;
  lUnit: String;
  lUnitMember: TUnitMember;
  lCounter: Integer;
  lPropCounter: Integer;
  lClass: TClassMember;
begin
  lFileList := TStringList.Create;
  try
    FindUnitsList(lFileList);
    Result := nil;
    if lFileList.Count > 0 then
    begin
      Result := TClassList.Create(nil);
      for lUnit in lFileList do
      begin
        if TFile.Exists(lUnit) then
        begin
          lUnitMember := TUnitReader.UnitMemberFromUnitPas(lUnit);
          try
            while lUnitMember.Classes.Count > 0 do
            begin
              lClass := lUnitMember.Classes.Extract(lUnitMember.Classes.First);
              if lClass.Properties.Count > 0 then
              begin
                lClass.UnitClassName := lUnitMember.Name;
                Result.add(lClass);
              end
              else
              begin
                lClass.Free;
              end;
            end;
          finally
            FreeAndnil(lUnitMember);
          end;
        end;
      end;
      for lCounter := Result.Count - 1 downto 0 do
      begin
        lClass := Result.Items[lCounter];
        for lPropCounter := lClass.Properties.Count - 1 downto 0 do
        begin
          if not (lClass.Properties.Items[lPropCounter].Visibility in [vsPublic, vsPublished]) then
          begin
            lClass.Properties.Delete(lPropCounter);
          end;
        end;
        if lClass.Properties.Count = 0 then
        begin
          Result.Delete(lCounter);
        end;
      end;
    end;
  finally
    FreeAndNil(lFileList);
  end;
end;


end.
