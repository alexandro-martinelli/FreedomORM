unit AM.UnitReader.Readers.ClassReader;

interface

uses
  AM.UnitReader.Readers.CustomReader,
  System.Classes,
  AM.UnitReader.Enumerations,
  AM.UnitReader.Members.MethodMember,
  AM.UnitReader.Members.ClassMember,
  AM.UnitReader.Members.CustomMember,
  AM.UnitReader.Helper.MemberVisibility,
  AM.UnitReader.Members.ConstantMember,
  AM.UnitReader.Members.FieldMember,
  AM.UnitReader.Utils.StrUtils,
  AM.UnitReader.Members.PropertyMember, AM.UnitReader.Helper.MemberType;

type
  TClassReader = class(TCustomReader)
  strict private
    FCounter: Integer;
    FCurrentLine: string;
    FSource: TStrings;
    FVisibilityScope: TVisibilityScope;
    FParentName: String;
    function GetClassMember: TClassMember;
    function ExtractMemberVisibility: TVisibilityScope;
    function GetMethodFromStr(pMethodStr: String; pVisibility: TVisibilityScope): TMethodMember;
    function IsVisibilityScope: Boolean;
    procedure GetNextLine;
    procedure RemoveComments;
    function CurrentLineIsComment: Boolean;
    procedure GetVisibilityScope;
    function IsBeginOfCommentBlock: Boolean;
    function IsEndOfCommentBlock: Boolean;

    procedure GoToEndOfClassTypeDeclaration;
    function ClassDeclarationInMoreRows: Boolean;
    function CurrentLineIsEndOfClass: Boolean;
    procedure PrepareSourceForUse;
    procedure DoReadInternalTypes;
    procedure ReadInternalClass;
    procedure ReadInternalRecord;
    procedure DoReadConstants;
    function IsMethod: Boolean;
    procedure ExtractMethod;
    function IsProcedure: Boolean;
    procedure ReadField;
    function IsProperty: Boolean;
    procedure ReadProperty;
    function IsClass: Boolean;
    function IsVariable: Boolean;
    function IsConstant: Boolean;
    procedure ExtractParentName;
  strict protected
    procedure DoRead; override;
    function GetMemberClass: TMemberClass; override;
    function ExtractName: string; override;
  public
    constructor Create(pMemberStr: string; pVisibility: TVisibilityScope); override;
    destructor Destroy; override;
    class function ClassFromStr(pMemberStr: string; pVisibility: TVisibilityScope): TClassMember;
  end;

implementation

uses
  System.StrUtils,
  System.SysUtils,
  AM.UnitReader.Readers.MethodReader;

class function TClassReader.ClassFromStr(pMemberStr: string; pVisibility: TVisibilityScope): TClassMember;
var
  lClassReader: TClassReader;
begin
  lClassReader := TClassReader.Create(pMemberStr, pVisibility);
  try
    Result := TClassMember(lClassReader.Read);
  finally
    lClassReader.Free;
  end;
end;

function TClassReader.GetClassMember: TClassMember;
begin
  Result := TClassMember(CreatedMember);
end;

function TClassReader.GetMemberClass: TMemberClass;
begin
  Result := TClassMember;
end;

function TClassReader.GetMethodFromStr(pMethodStr: String; pVisibility: TVisibilityScope): TMethodMember;
begin
  Result := TMethodReader.MethodFromStr(pMethodStr, pVisibility);
end;

procedure TClassReader.GetNextLine;
begin
  FCurrentLine := '';
  Inc(FCounter);
  if FCounter <= FSource.Count - 1 then
  begin
    FCurrentLine := Trim(FSource.Strings[FCounter]);
    if FCurrentLine <> '' then
    begin
      FCurrentLine := RemoveSpaces(FCurrentLine);
      RemoveComments;
    end
    else
    begin
      GetNextLine;
    end;
  end;
end;

procedure TClassReader.GetVisibilityScope;
begin
  FVisibilityScope := ExtractMemberVisibility;
end;

procedure TClassReader.GoToEndOfClassTypeDeclaration;
begin
  if ClassDeclarationInMoreRows then
  begin
    while not(EndsText(')', FCurrentLine) or EndsText(');', FCurrentLine)) do
    begin
      GetNextLine;
    end;
  end;
end;

function TClassReader.IsVariable: Boolean;
begin
  Result := not IsMethod and not IsConstant and (Pos(':', FCurrentLine) > 0);
end;

function TClassReader.IsVisibilityScope: Boolean;
begin
  Result := TStrUtils.ContainsText(FCurrentLine, 'private') or TStrUtils.ContainsText(FCurrentLine, 'protected') or
      TStrUtils.ContainsText(FCurrentLine, 'public') or TStrUtils.ContainsText(FCurrentLine, 'published');
end;

function TClassReader.IsBeginOfCommentBlock: Boolean;
begin
  Result := StartsText('{', FCurrentLine) or StartsText('(*', FCurrentLine);
end;

function TClassReader.IsClass: Boolean;
begin
  Result := Pos('= class', FCurrentLine) > 0;
end;

function TClassReader.IsConstant: Boolean;
begin
  Result := not IsClass and not IsMethod and (Pos('=', FCurrentLine) > 0)
end;

function TClassReader.ClassDeclarationInMoreRows: Boolean;
begin
  Result := TStrUtils.ContainsText(FCurrentLine, '(') and (not EndsText(')', FCurrentLine) and not EndsText(';', FCurrentLine));
end;

function TClassReader.IsEndOfCommentBlock: Boolean;
begin
  Result := StartsText('}', FCurrentLine) or StartsText('*)', FCurrentLine);
end;

function TClassReader.IsMethod: Boolean;
begin
  Result := TStrUtils.ContainsText(FCurrentLine, 'procedure') or TStrUtils.ContainsText(FCurrentLine, 'function') or
      TStrUtils.ContainsText(FCurrentLine, 'constructor') or TStrUtils.ContainsText(FCurrentLine, 'destructor');
end;

function TClassReader.IsProcedure: Boolean;
begin
  Result := TStrUtils.ContainsText(FCurrentLine, 'procedure') or TStrUtils.ContainsText(FCurrentLine, 'constructor')
      or TStrUtils.ContainsText(FCurrentLine, 'destructor');
end;

function TClassReader.IsProperty: Boolean;
begin
  Result := TStrUtils.ContainsText(FCurrentLine, 'property');
end;

procedure TClassReader.PrepareSourceForUse;
begin
  FSource.Text := TStrUtils.RemoveDuplicateLineBreaks(FSource.Text);
end;

procedure TClassReader.ReadField;
var
  lFieldName: string;
begin
   lFieldName := RemoveSpaces(Copy(RemoveSpaces(FCurrentLine), 1, PosStart(':', FCurrentLine)));
   if lFieldName <> '' then
   begin
     if Pos('var', lFieldName) > 0 then
     begin
       lFieldName := Copy(lFieldName, PosEnd('var', lFieldName), Length(lFieldName));
     end;
     GetClassMember.Fields.Add(TFieldMember.Create(lFieldName, FVisibilityScope));
   end;
end;

procedure TClassReader.ReadInternalClass;
var
  lTypeCounter: Byte;
  lMemberText: String;
begin
  lMemberText := '';
  lTypeCounter := 0;
  if not EndsText(';', FCurrentLine) then
  begin
    while (not TStrUtils.ContainsText(Trim(FCurrentLine), 'end;')) or (lTypeCounter > 0) do
    begin
      if TStrUtils.ContainsText(Trim(FCurrentLine), 'end;') and (lTypeCounter > 0) then
      begin
        Dec(lTypeCounter);
      end;
      lMemberText := lMemberText + ifthen(lMemberText <> '', sLineBreak) + FCurrentLine;
      GetNextLine;
      if TStrUtils.ContainsText(Trim(FCurrentLine), '= class') then
      begin
        Inc(lTypeCounter);
      end
      else if TStrUtils.ContainsText(Trim(FCurrentLine), '= record') then
      begin
        Inc(lTypeCounter);
      end;
    end;
  end;
  lMemberText := lMemberText + ifthen(lMemberText <> '', sLineBreak) + FCurrentLine;
  GetClassMember.Classes.Add(TClassReader.ClassFromStr(lMemberText, FVisibilityScope));
end;

procedure TClassReader.ReadInternalRecord;
var
  lTypeCounter: Byte;
  lMemberText: String;
begin
  lTypeCounter := 0;
  while (not TStrUtils.ContainsText(Trim(FCurrentLine), 'end;')) or (lTypeCounter > 0) do
  begin
    if TStrUtils.ContainsText(Trim(FCurrentLine), 'end;') and (lTypeCounter > 0) then
    begin
      Dec(lTypeCounter);
    end;
    lMemberText := lMemberText + ifthen(lMemberText <> '', sLineBreak) + FCurrentLine;
    GetNextLine;
    if TStrUtils.ContainsText(Trim(FCurrentLine), '= class') then
    begin
      Inc(lTypeCounter);
    end else if TStrUtils.ContainsText(Trim(FCurrentLine), '= record') then
    begin
      Inc(lTypeCounter);
    end;
  end;
end;

procedure TClassReader.ReadProperty;
var
  lPropertyName: string;
  lPropertyType: TMemberType;
  lStartPos: Integer;
  lEndPos: Integer;
  lPropertyTypeName: String;
begin
  lStartPos := 0;
  lPropertyType := mtUnknow;
  if Pos('[', FCurrentLine) > 0 then
  begin
    lPropertyName := RemoveSpaces(Copy(RemoveSpaces(FCurrentLine), 1, PosStart('[', FCurrentLine)));
    lStartPos := Pos(']', FCurrentLine) + 2;
  end
  else if Pos(':', FCurrentLine) > 0 then
  begin
    lPropertyName := RemoveSpaces(Copy(RemoveSpaces(FCurrentLine), 1, PosStart(':', FCurrentLine)));
    lStartPos := Pos(':', FCurrentLine) + 1;
  end
  else
  begin
    lPropertyName := RemoveSpaces(Copy(RemoveSpaces(FCurrentLine), 1, PosStart(';', FCurrentLine)));
  end;
  if lPropertyName <> '' then
  begin
    lEndPos := PosEx(' ', FCurrentLine, lStartPos + 2);
    lPropertyTypeName := CopyAndTrim(FCurrentLine, lStartPos, lEndPos - lStartPos);
    if lPropertyTypeName <> '' then
    begin
      lPropertyType := TMemberTypeFuncs.MemberTypeFromString(lPropertyTypeName);
    end;
    lPropertyName := CopyAndTrim(lPropertyName, PosEnd('property', lPropertyName), Length(lPropertyName));
    GetClassMember.Properties.Add(TPropertyMember.Create(lPropertyName, lPropertyTypeName, lPropertyType, FVisibilityScope));
  end;
end;

procedure TClassReader.RemoveComments;
begin
  if CurrentLineIsComment then
  begin
    GetNextLine;
  end;
  if IsBeginOfCommentBlock then
  begin
    while not IsEndOfCommentBlock do
    begin
      GetNextLine;
    end;
  end;
end;

constructor TClassReader.Create(pMemberStr: string; pVisibility: TVisibilityScope);
begin
  inherited;
  FSource := TStringList.Create;
  FSource.Text := GetMemberStr;
  PrepareSourceForUse;
  FCounter := -1;
end;

function TClassReader.CurrentLineIsComment: Boolean;
begin
  Result := (StartsText('{', FCurrentLine) and EndsText('}', FCurrentLine)) or StartsText('//', FCurrentLine) or
      (StartsText('(*', FCurrentLine) and EndsText('*)', FCurrentLine));
end;

function TClassReader.CurrentLineIsEndOfClass: Boolean;
begin
  Result := EndsText(');', FCurrentLine) or EndsText(';', FCurrentLine);
end;

destructor TClassReader.Destroy;
begin
  FSource.Free;
  inherited;
end;

procedure TClassReader.DoRead;
begin
  inherited;
  if Pos(';', FSource.Strings[0]) = 0 then
  begin
    GetNextLine;
    GoToEndOfClassTypeDeclaration;
    if not CurrentLineIsEndOfClass then
    begin
      FVisibilityScope := vsPublished;
      GetNextLine;
      while FCounter <= FSource.Count - 1 do
      begin
        if IsVisibilityScope then
        begin
          GetVisibilityScope;
          GetNextLine;
        end;
        if SameText(Trim(FCurrentLine), 'type') then
        begin
          GetNextLine;
          DoReadInternalTypes;
        end
        else if SameText(Trim(FCurrentLine), 'const') then
        begin
          GetNextLine;
          DoReadConstants;
        end
        else
        begin
          if IsMethod then
          begin
            ExtractMethod;
          end
          else if IsProperty then
          begin
            ReadProperty;
          end
          else
          begin
            ReadField;
          end;
        end;
        GetNextLine;
      end;
    end;
  end;
  TClassMember(CreatedMember).ParentName := FParentName;
end;

procedure TClassReader.DoReadConstants;
var
  lConstantName: string;
begin
  while not IsVisibilityScope and (FCurrentLine <> 'end;') and not IsMethod and not IsClass and not IsProperty and not IsVariable do
  begin
    lConstantName := RemoveSpaces(Copy(FCurrentLine, 1, PosStart('=', FCurrentLine)));
    GetClassMember.Constants.Add(TConstantMember.Create(lConstantName, FVisibilityScope));
    GetNextLine;
  end;
end;

procedure TClassReader.DoReadInternalTypes;
begin
  if TStrUtils.ContainsText(Trim(FCurrentLine), '= class') then
  begin
    ReadInternalClass;
  end
  else if TStrUtils.ContainsText(Trim(FCurrentLine), '= record') then
  begin
    ReadInternalRecord;
  end;
end;

procedure TClassReader.ExtractMethod;
var
  lEndsText: String;
  lMethodStr: string;
begin
  lEndsText := ';';
  if TStrUtils.ContainsText(FCurrentLine, '(') then
  begin
    if IsProcedure then
    begin
      lEndsText := ');';
    end else
    begin
      lEndsText := '):';
    end;
  end;
  lMethodStr := FCurrentLine;
  while Pos(lEndsText, FCurrentLine) <= 0 do
  begin
    GetNextLine;
    lMethodStr := lMethodStr + ' ' + FCurrentLine;
  end;
  TClassMember(CreatedMember).Methods.Add(GetMethodFromStr(lMethodStr, FVisibilityScope));
end;

function TClassReader.ExtractName: string;
begin
  GetNextLine;
  Result := RemoveSpaces(Copy(FCurrentLine, 1, PosStart('=', FCurrentLine)));
  ExtractParentName;
  FCounter := -1;
end;

procedure TClassReader.ExtractParentName;
var
  lStartPos, lEndPos: Integer;
begin
  FParentName := 'TObject';
  lStartPos := Pos('(', FCurrentLine) + 1;
  if (lStartPos > 0) then
  begin
    lEndPos := Pos(',', FCurrentLine);
    if lEndPos <= 0 then
    begin
      lEndPos := Pos(')', FCurrentLine);
    end;
    if lEndPos > 0 then
    begin
      FParentName := Copy(FCurrentLine, lStartPos, lEndPos - lStartPos);
    end;
  end;
end;

function TClassReader.ExtractMemberVisibility: TVisibilityScope;
var
  lCounter: Integer;
  lLine, lLineConstOrType: string;
begin
  Result := vsPublished;
  if IsVisibilityScope then
  begin
    lLine := FCurrentLine;
    lLineConstOrType := '';
    if Pos('type', FCurrentLine) > 0 then
    begin
      lLineConstOrType := 'type';
      Delete(FCurrentLine, PosStart('type', FCurrentLine), length(FCurrentLine));
    end
    else if Pos('const', FCurrentLine) > 0 then
    begin
      lLineConstOrType := 'const';
      Delete(FCurrentLine, PosStart('const', FCurrentLine), length(FCurrentLine));
    end;
    for lCounter := Ord(Low(TVisibilityScope)) to Ord(High(TVisibilityScope)) do
    begin
      if SameText(FCurrentLine, TVisibilityScope(lCounter).ToString) then
      begin
        Result := TVisibilityScope(lCounter);
        Break;
      end;
    end;
    if lLineConstOrType <> '' then
    begin
      TStrUtils.ReplaceText(FSource.Text, lLine, Result.ToString + sLineBreak + lLineConstOrType);
    end;
  end;
end;

end.
