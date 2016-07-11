unit AM.UnitReader.Readers.CustomReader;

interface

uses
  AM.UnitReader.Members.CustomMember,
  AM.UnitReader.Enumerations, AM.UnitReader.Utils.StrUtils;

type
  TCustomReader = class abstract
  strict private
    FVisibility: TVisibilityScope;
    FMemberStr: String;
    FCreatedMember: TCustomMember;
  strict protected
    function GetVisibility: TVisibilityScope;
    function GetMemberStr: String;
    function CreatedMember: TCustomMember;

    function RemoveSpaces(pString: String): String;
    function RemoveLineBreaks(pString: String): String;
    function ExtractName: String; virtual; abstract;
    function GetMemberClass: TMemberClass; virtual; abstract;
    function PosEnd(pSubStr, pString: String): Integer;
    function PosEndEx(pSubStr, pString: String; pOffSet: Integer = 1): Integer;
    function PosStart(pSubStr, pString: String): Integer;
    function PosStartEx(pSubStr, pString: String; pOffSet: Integer = 1): Integer;
    function CopyAndTrim(pString: String; pPosIni: Integer; pCount: Integer): String;
    function DeleteAndTrim(pString: String; pPosIni: Integer; pCount: Integer): String;
    procedure DoRead; virtual;
  public
    constructor Create(pMemberStr: String; pVisibility: TVisibilityScope); virtual;
    function Read: TCustomMember;
  end;

implementation

uses
  System.StrUtils, System.SysUtils;

{ TCustomReader }

function TCustomReader.CopyAndTrim(pString: String; pPosIni, pCount: Integer): String;
begin
  Result := Trim(Copy(pString, pPosIni, pCount));
end;

constructor TCustomReader.Create(pMemberStr: String; pVisibility: TVisibilityScope);
begin
  FVisibility := pVisibility;
  FMemberStr := pMemberStr;
end;

function TCustomReader.Read: TCustomMember;
begin
  FCreatedMember := GetMemberClass.Create(ExtractName, GetVisibility);
  Result := FCreatedMember;
  DoRead;
end;

function TCustomReader.CreatedMember: TCustomMember;
begin
  Result := FCreatedMember;
end;

function TCustomReader.DeleteAndTrim(pString: String; pPosIni, pCount: Integer): String;
begin
  Result := pString;
  Delete(Result, pPosIni, pCount);
  Result := Trim(Result);
end;

procedure TCustomReader.DoRead;
begin
  // do when necessary
end;

function TCustomReader.GetMemberStr: String;
begin
  Result := FMemberStr;
end;

function TCustomReader.GetVisibility: TVisibilityScope;
begin
  Result := FVisibility;
end;

function TCustomReader.PosEnd(pSubStr, pString: String): Integer;
begin
  Result := TStrUtils.PosEnd(pSubStr, pString);
end;

function TCustomReader.PosEndEx(pSubStr, pString: String; pOffSet: Integer): Integer;
begin
  Result := TStrUtils.PosEndEx(pSubStr, pString, pOffSet);
end;

function TCustomReader.PosStart(pSubStr, pString: String): Integer;
begin
  Result := TStrUtils.PosStart(pSubStr, pString);
end;

function TCustomReader.PosStartEx(pSubStr, pString: String; pOffSet: Integer): Integer;
begin
  Result := TStrUtils.PosStartEx(pSubStr, pString, pOffSet);
end;

function TCustomReader.RemoveLineBreaks(pString: String): String;
begin
  Result := TStrUtils.ReplaceText(pString, sLineBreak, '');
end;

function TCustomReader.RemoveSpaces(pString: String): String;
begin
  Result := TStrUtils.RemoveSpaces(pString);
end;

end.
