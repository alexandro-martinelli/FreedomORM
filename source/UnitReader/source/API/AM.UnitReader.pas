unit AM.UnitReader;

interface

uses
  System.Classes,
  AM.UnitReader.Enumerations,
  AM.UnitReader.PrefixOptions,
  AM.UnitReader.Members.UnitMember,
  AM.UnitReader.Members.ClassMember,
  AM.UnitReader.Helper.MemberVisibility,
  AM.UnitReader.Members.MethodMember,
  AM.UnitReader.Readers.CustomReader,
  AM.UnitReader.Members.CustomMember,
  AM.UnitReader.Members.ConstantMember,
  AM.UnitReader.Members.FieldMember,
  AM.UnitReader.Readers.ClassReader,
  AM.UnitReader.Utils.StrUtils;

type
  TUnitReader = class(TCustomReader)
  strict private
    FUnitFile: String;
    FUnitText: TStrings;
    FInterfaceSection: String;
    FImplementationSection: String;
    FUnitMember: TUnitMember;
    FCurrentLine: string;
    FCounter: Integer;
    FSourceText: TStrings;
    procedure GetNextLine;
    procedure SetUnitFile(const pFileName: String);
    procedure InitializeRead;
    procedure ReadSections;
    procedure ReadInterfaceSection;
    procedure ReadImplementationSection;
    procedure RemoveUsesInImplementationSection;
    procedure DoReadUnit;
    function IsAttribute: Boolean;
    procedure ReadAttributes;
    function IsClass: Boolean;
    procedure ReadClass;
    function GetClassMemberFromText(pMemberText: String): TClassMember;
    function IsRecord: Boolean;
    procedure ReadRecord;
    function IsMethod: Boolean;
    procedure ReadMethod;
    function GetMethodFromStr(pMethodStr: String; pVisibility: TVisibilityScope): TMethodMember;
    procedure ReadConstants;
    procedure ReadFields;
  strict protected
    function GetMemberClass: TMemberClass; override;
    procedure DoRead; override;
    function ExtractName: string; override;
  public
    constructor Create(pMemberStr: string; pVisibility: TVisibilityScope); override;
    destructor Destroy; override;
    class function UnitMemberFromUnitPas(pUnitFileName: String): TUnitMember;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.UnitReader.Helper.WordType,
  AM.UnitReader.WordVerificator,
  AM.UnitReader.Readers.MethodReader;

{ TUnitReader }

procedure TUnitReader.ReadAttributes;
begin
//  GetNextLine;
end;

procedure TUnitReader.ReadClass;
var
  lTypeCounter: Integer;
  lMemberText: String;
begin
  lTypeCounter := 0;
  lMemberText := '';
  if (not StartsText('//', FCurrentLine)) and (not StartsText('{', FCurrentLine)) and (not StartsText('(*', FCurrentLine)) then
  begin
    if not EndsText(';', FCurrentLine) then
    begin
      while (not TStrUtils.ContainsText(FCurrentLine, 'end;')) or (lTypeCounter > 0) do
      begin
        if TStrUtils.ContainsText(FCurrentLine, 'end;') and (lTypeCounter > 0) then
        begin
          Dec(lTypeCounter);
        end;
        lMemberText := lMemberText + ifthen(lMemberText <> '', sLineBreak) + FCurrentLine;
        GetNextLine;
        if IsClass then
        begin
          Inc(lTypeCounter);
        end
        else if IsRecord then
        begin
          Inc(lTypeCounter);
        end;
      end;
      lMemberText := lMemberText + ifthen(lMemberText <> '', sLineBreak) + FCurrentLine;
    end
    else
    begin
      lMemberText := FCurrentLine;
    end;
    FUnitMember.Classes.Add(GetClassMemberFromText(lMemberText));
  end;
end;

procedure TUnitReader.ReadConstants;
begin
  while (FCounter <= FSourceText.Count - 1) and not TStrUtils.ContainsText(FCurrentLine, 'var')
        and not TStrUtils.ContainsText(FCurrentLine, 'type') and not IsMethod do
  begin
    GetNextLine;
    if FCurrentLine <> '' then
    begin
      FCurrentLine := TStrUtils.RemoveSpaces(Copy(TStrUtils.RemoveSpaces(FCurrentLine), 1, PosStart('=', FCurrentLine)));
      FUnitMember.Constants.Add(TConstantMember.Create(FCurrentLine, vsPublished));
    end;
  end;
end;

procedure TUnitReader.ReadFields;
begin
  while (FCounter <= FSourceText.Count - 1) and not TStrUtils.ContainsText(FCurrentLine, 'const') and
        not TStrUtils.ContainsText(FCurrentLine, 'type') and not IsMethod do
  begin
    GetNextLine;
    if FCurrentLine <> '' then
    begin
      FCurrentLine := TStrUtils.RemoveSpaces(Copy(TStrUtils.RemoveSpaces(FCurrentLine), 1, PosStart(':', FCurrentLine)));
      FUnitMember.Fields.Add(TFieldMember.Create(FCurrentLine, vsPublished));
    end;
  end;
end;

procedure TUnitReader.ReadImplementationSection;
var
  lPosIni, lPosFim: Integer;
begin
  lPosIni := PosEnd('implementation', FUnitText.Text);
  if Pos('initialization', FUnitText.Text) > 0 then
  begin
    lPosFim := PosStart('initialization', FUnitText.Text);
  end else
  begin
    lPosFim := PosStart('end.', FUnitText.Text);
  end;
  FImplementationSection := Copy(FUnitText.Text, lPosIni, lPosFim - lPosIni);
  FImplementationSection := TStrUtils.ReplaceText(FImplementationSection, '=class', '= class');
  FImplementationSection := TStrUtils.ReplaceText(FImplementationSection, '=record', '= record');
  FImplementationSection := TStrUtils.RemoveDuplicateLineBreaks(FImplementationSection);
  RemoveUsesInImplementationSection;
end;

procedure TUnitReader.ReadInterfaceSection;
var
  lPosIni, lPosFim: Integer;
begin
  lPosIni := PosEnd('interface', FUnitText.Text);
  lPosIni := PosEndEx('type', FUnitText.Text, lPosIni);
  lPosFim := PosStart('implementation', FUnitText.Text);
  FInterfaceSection := Copy(FUnitText.Text, lPosIni, lPosFim - lPosIni);
  FInterfaceSection := TStrUtils.ReplaceText(FInterfaceSection, '=class', '= class');
  FInterfaceSection := TStrUtils.ReplaceText(FInterfaceSection, '=record', '= record');
  FInterfaceSection := TStrUtils.RemoveDuplicateLineBreaks(FInterfaceSection);
end;

procedure TUnitReader.ReadMethod;
var
  lEndsText, lMethodStr: string;
begin
  lEndsText := ';';
  if ContainsText(FCurrentLine, '(') then
  begin
    lEndsText := ');';
  end;
  lMethodStr := FCurrentLine;
  while not EndsText(lEndsText, FCurrentLine) do
  begin
    GetNextLine;
    lMethodStr := lMethodStr + FCurrentLine;
  end;
  FUnitMember.Methods.Add(GetMethodFromStr(lMethodStr, vsPublished));
end;

procedure TUnitReader.ReadRecord;
var
  lTypeCounter: Integer;
  lMemberText: String;
begin
  lTypeCounter := 0;
  lMemberText := '';
  while not ContainsText(Trim(FCurrentLine), 'end;') do
  begin
    if TStrUtils.ContainsText(Trim(FCurrentLine), 'end;') and (lTypeCounter > 0) then
    begin
      Dec(lTypeCounter);
    end;
    lMemberText := lMemberText + ifthen(lMemberText <> '', sLineBreak) + FCurrentLine;
    GetNextLine;
    if IsClass then
    begin
      Inc(lTypeCounter);
    end else if IsRecord then
    begin
      Inc(lTypeCounter);
    end;
  end;
end;

procedure TUnitReader.ReadSections;
begin
  ReadInterfaceSection;
  ReadImplementationSection
end;

procedure TUnitReader.RemoveUsesInImplementationSection;
begin
  if Pos('uses', FImplementationSection) > 0 then
  begin
    Delete(FImplementationSection, 1, PosStart('uses', FImplementationSection));
    Delete(FImplementationSection, 1, PosEnd(';', FImplementationSection) + 1);
  end;
end;

constructor TUnitReader.Create(pMemberStr: string; pVisibility: TVisibilityScope);
begin
  inherited Create(pMemberStr, pVisibility);
  FUnitText := TStringList.Create;
  FUnitText.LoadFromFile(pMemberStr);
  SetUnitFile(pMemberStr);
  FCounter := -1;
  FCurrentLine := '';
end;

destructor TUnitReader.Destroy;
begin
  FUnitText.Free;
  FSourceText.Free;
  inherited;
end;

procedure TUnitReader.DoRead;
begin
  inherited;
  FUnitMember := TUnitMember(CreatedMember);
  InitializeRead;
  DoReadUnit;
end;

procedure TUnitReader.DoReadUnit;
var
  lLine: string;
begin
  FSourceText := TStringList.Create;
  FSourceText.Text := FInterfaceSection;
  FCounter := -1;
  while FCounter <= FSourceText.Count - 1 do
  begin
    GetNextLine;
    if IsAttribute then
    begin
      ReadAttributes;
    end
    else if IsClass then
    begin
      ReadClass;
    end
    else if IsRecord then
    begin
      ReadRecord;
    end
    else if SameText(lLine, 'const') then
    begin
      ReadConstants;
    end
    else if SameText(lLine, 'var') then
    begin
      ReadFields;
    end
    else if IsMethod then
    begin
      ReadMethod;
    end;
  end;
end;

function TUnitReader.ExtractName: string;
begin
  FCurrentLine := FUnitText.Strings[0];
  Result := CopyAndTrim(FCurrentLine, Pos(' ', FCurrentLine), Length(FCurrentLine));
  Delete(Result, Length(Result), 1)
end;

procedure TUnitReader.InitializeRead;
begin
  FUnitText.Clear;
  FUnitText.LoadFromFile(FUnitFile);
  ReadSections;
end;

function TUnitReader.IsAttribute: Boolean;
begin
  Result := ContainsText(Trim(FCurrentLine), '[');
end;

function TUnitReader.IsClass: Boolean;
begin
  Result := TStrUtils.ContainsText(Trim(FCurrentLine), '= class');
  if Result then
  begin
    Result := Pos('class of', FCurrentLine) = 0;
    if (Result) then
    begin
      Result := not TStrUtils.ContainsText(Trim(FCurrentLine), '= class;');
    end;
  end;
end;

function TUnitReader.IsMethod: Boolean;
begin
  Result := ContainsText(FCurrentLine, 'procedure') or ContainsText(FCurrentLine, 'function') or ContainsText(FCurrentLine, 'constructor') or
      ContainsText(FCurrentLine, 'destructor');
end;

function TUnitReader.IsRecord: Boolean;
begin
  Result := ContainsText(Trim(FCurrentLine), '= record');
end;

procedure TUnitReader.SetUnitFile(const pFileName: String);
begin
  FUnitFile := pFileName;
end;

class function TUnitReader.UnitMemberFromUnitPas(pUnitFileName: String): TUnitMember;
var
  lUnitReader: TUnitReader;
begin
  lUnitReader := TUnitReader.Create(pUnitFileName, vsPublished);
  try
    Result := TUnitMember(lUnitReader.Read);
  finally
    lUnitReader.Free;
  end;
end;

function TUnitReader.GetClassMemberFromText(pMemberText: String): TClassMember;
begin
  Result := TClassReader.ClassFromStr(pMemberText, vsPublished);
end;

function TUnitReader.GetMemberClass: TMemberClass;
begin
  Result := TUnitMember;
end;

function TUnitReader.GetMethodFromStr(pMethodStr: String; pVisibility: TVisibilityScope): TMethodMember;
begin
  Result := TMethodReader.MethodFromStr(pMethodStr, pVisibility);
end;

procedure TUnitReader.GetNextLine;
begin
  FCurrentLine := '';
  Inc(FCounter);
  if FCounter <= FSourceText.Count - 1 then
  begin
    FCurrentLine := TStrUtils.RemoveSpaces(FSourceText.Strings[FCounter]);
    if FCurrentLine = '' then
    begin
      GetNextLine;
    end;
  end;
end;

end.
