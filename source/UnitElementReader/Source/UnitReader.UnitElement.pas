unit UnitReader.UnitElement;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  UnitReader.Elements,
  UnitReader.EnumerationTypes,
  AM.UnitReader.Enumerations,
  UnitReader.ClassElement;

type
  TVariableDeclarationResult = class
  private
    FLineNumber: Integer;
    FIsDeclaration: Boolean;
  public
    property LineNumber: Integer read FLineNumber write FLineNumber;
    property IsDeclaration: Boolean read FIsDeclaration write FIsDeclaration;
  end;

  TConstantDeclarationResult = class(TVariableDeclarationResult);
  TUsesDeclarationResult = class(TVariableDeclarationResult);

  TUnitElement = class(TLines)
  strict private
  procedure ExtractUsesFromLineText(pText: String; pUsesList: TStrings);
  procedure ExtractUnitNameFromText(pText: String);
  private
    FSourceText: TStrings;
    FLoadedUnitName: String;
    FCurrentSection: TTokenType;
    FInterfaceImplementation: TTokenType;
    FParentLine: TBlockElement;
    FClassDeclarationList: TObjectList<TClassElement>;
    FUsesDeclaration: TList<TBlockElement>;
    procedure DoLoad(pText: String);
    procedure DoLoadSourceText;
    function ExtractTokenType(pText: String): TTokenType;
    function CreateLineByTokenType(pTokenType: TTokenType): TLineElement;
    function GetParentLineForMethodImplementation(pLine: TLineElement): TLineElement;
    function ExtractMethodName(pText: String): String;
    function SearchIdentifierInSection(pSectionDeclaration: TBlockElement; pIdentifierName: String): Boolean;
    function InternalContainsText(pSearchText, pText: string): Boolean;
    function SearchVarDeclarationInVar(pVarDeclaration: TBlockElement; pVarName: String): Boolean;
    function SearchConstDeclarationInConst(pConstDeclaration: TBlockElement; pConstName: String): Boolean;
    function ExtractVisibilityScopeFromText(pText: String): TVisibilityScope;
    procedure GetConstantClassDeclarationFromLine(pLine: TLineElement; pClassDeclaration: TBlockElement;
        pConstantDeclaration: TConstantDeclarationResult; pVisibilityScope: TVisibilityScope);
    function LineByTokenType(pTokenType: TTokenType): TLineElement;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure LoadFromFile(pFileName: string);
    procedure LoadFromText(pText: string);
    procedure LoadFromStrings(pStrings: TStrings);
    procedure SaveToFile(pFileName: String);
    function BeginMethodImplementationBeforeLineIndex(pLineNumber: Integer): Integer;
    function BeginMethodImplementationWithMethodName(pMethodName: String): Integer;
    function EndMethodImplementationAfterLineIndex(pLineNumber: Integer): Integer;
    function VariableDeclarationBeforeLineIndex(pLineNumber: Integer): TVariableDeclarationResult;
    function VariableDeclarationExistsBeforeLineIndex(pLineNumber: Integer; pVarName: String): Boolean;
    function ConstantDeclarationBeforeLineIndex(pLineNumber: Integer): TConstantDeclarationResult;
    function ConstantDeclarationExistsBeforeLineIndex(pLineNumber: Integer; pConstName: String): Boolean;
    function ConstantClassDeclaration(pLineNumber: Integer; pVisibilityScope: TVisibilityScope): TConstantDeclarationResult;
    function ClassDeclarationOnLineNumber(pLineNumber: Integer): TClassElement;
    function ClassDeclarationList: TObjectList<TClassElement>;
    function ExtractDisplayMethodName(pMethod: TLineElement): String;
    function EndOfUsesDeclaration(pInterface: Boolean): TUsesDeclarationResult;
    function UsesList: TStrings;
    function SectionByLineNumber(pLineNumber: Integer): TTokenType;
    property LoadedUnitName: String read FLoadedUnitName write FLoadedUnitName;
  end;



implementation

uses
  System.IOUtils,
  System.StrUtils,
  AM.UnitReader.Helper.MemberVisibility,
  UnitReader.TokenTypeHelper;

{ TUnitElement }

function TUnitElement.BeginMethodImplementationBeforeLineIndex(pLineNumber: Integer): Integer;
var
  lLine: TLineElement;
  lInternalLine: TLineElement;
begin
  Result := 0;
  lLine := LineByLineNumber(pLineNumber);
  if (lLine.TokenType.IsMethodImplementation) then
  begin
    for lInternalLine in TBlockElement(lLine).Lines do
    begin
      if (lInternalLine.TokenType = tkBegin) then
      begin
        Result := lInternalLine.LineNumber;
        Break;
      end;
    end;
  end;
  if (Result = 0) then
  begin
    while Assigned(lLine) and (not LLine.TokenType.IsMethodImplementation) do
    begin
      lLine := lLine.Parent;
    end;
    if (Assigned(lLine)) then
    begin
      for lInternalLine in TBlockElement(lLine).Lines do
      begin
        if (lInternalLine.TokenType = tkBegin) then
        begin
          Result := lInternalLine.LineNumber;
          Break;
        end;
      end;
    end;
  end;
end;

function TUnitElement.BeginMethodImplementationWithMethodName(pMethodName: String): Integer;
var
  lLine: TLineElement;
begin
  Result := 0;
  for lLine in Self do
  begin
    if (lLine.TokenType.IsMethodImplementation) then
    begin
      if SameText(ExtractMethodName(lLine.Text), pMethodName) then
      begin
        Result := TBlockElement(lLine).Lines.Last.LineNumber;
        Result := BeginMethodImplementationBeforeLineIndex(Result);
        Break;
      end;
    end;
  end;
end;

function TUnitElement.ClassDeclarationList: TObjectList<TClassElement>;
var
  lLine: TLineElement;
begin
  if (not Assigned(FClassDeclarationList)) then
  begin
    FClassDeclarationList := TObjectList<TClassElement>.Create;
    for lLine in Self do
    begin
      if (lLine.TokenType = tkClassDeclaration) then
      begin
        FClassDeclarationList.Add(TClassElement.Create(TBlockElement(lLine)));
      end;
    end;
  end;
  Result := FClassDeclarationList;
end;

function TUnitElement.ClassDeclarationOnLineNumber(pLineNumber: Integer): TClassElement;
var
  lLine: TLineElement;
begin
  Result := nil;
  lLine := LineByLineNumber(pLineNumber);
  while Assigned(lLine) and (lLine.TokenType <> tkClassDeclaration) do
  begin
    lLine := lLine.Parent;
  end;
  if (Assigned(lLine)) then
  begin
    Result := TClassElement.Create(TBlockElement(lLine));
  end;
end;

function TUnitElement.ConstantClassDeclaration(pLineNumber: Integer; pVisibilityScope: TVisibilityScope): TConstantDeclarationResult;
var
  lLine: TLineElement;
  lInternalLine: TLineElement;
  lClassElement: TClassElement;
begin
  Result := TConstantDeclarationResult.Create;
  lLine := LineByLineNumber(pLineNumber);
  while Assigned(lLine) and (lLine.TokenType <> tkClassDeclaration) do
  begin
    lLine := lLine.Parent;
  end;
  if (Assigned(lLine)) then
  begin
    lClassElement := TClassElement.Create(TBlockElement(lLine));
    for lInternalLine in lClassElement.ScopeList do
    begin
      GetConstantClassDeclarationFromLine(lInternalLine, lClassElement.ClassDeclaration, Result, pVisibilityScope);
      if (Result.LineNumber > 0) then
      begin
        Break;
      end;
    end;
    if (Result.LineNumber = 0) then
    begin
      Result.IsDeclaration := False;
      if (pVisibilityScope = vsStrictPrivate) then
      begin
        Result.LineNumber := lLine.LineNumber;
      end
      else
      begin
        Result.LineNumber := lClassElement.ExtractLineFromInexistentVisibilityScope(pVisibilityScope);
      end;
    end;
  end;
end;

function TUnitElement.ConstantDeclarationBeforeLineIndex(pLineNumber: Integer): TConstantDeclarationResult;
var
  lLine: TLineElement;
  lInternalLine: TLineElement;
begin
  Result := TConstantDeclarationResult.Create;
  lLine := LineByLineNumber(pLineNumber);
  if (lLine.TokenType <> tkConstSection) then
  begin
    if (lLine.Parent <> nil) then
    begin
      if (lLine.Parent.TokenType = tkConstSection) then
      begin
        Result.LineNumber := TBlockElement(lLine.Parent).Lines.Last.LineNumber;
      end
      else
      begin
        lline := lline.Parent;
        if (lLine.TokenType in [tkProcedureImplementation, tkClassProcedureImplementation,
           tkFunctionImplementation, tkClassFunctionImplementation,
           tkConstructorImplementation, tkClassConstructorImplementation,
           tkDestructorImplementation, tkClassDestructorImplementation]) then
        begin
          for lInternalLine in TBlockElement(lLine).Lines do
          begin
            if (lInternalLine.TokenType = tkConstSection) then
            begin
              Result.LineNumber := TBlockElement(lInternalLine).Lines.Last.LineNumber;
              Result.IsDeclaration := True;
              Break;
            end;
          end;
          if (Result.LineNumber = 0) then
          begin
            for lInternalLine in TBlockElement(lLine).Lines do
            begin
              if (lInternalLine.TokenType = tkBegin) then
              begin
                Result.LineNumber := lInternalLine.LineNumber - 1;
                Break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TUnitElement.ConstantDeclarationExistsBeforeLineIndex(pLineNumber: Integer; pConstName: String): Boolean;
var
  lLine: TLineElement;
  lInternalLine: TLineElement;
begin
  Result := False;
  lLine := LineByLineNumber(pLineNumber);
  if (lLine.TokenType = tkConstSection) then
  begin
    Result := SearchConstDeclarationInConst(TBlockElement(lLine), pConstName);
  end;
  if (not Result) then
  begin
    if (lLine.Parent <> nil) then
    begin
      lline := lline.Parent;
      if (lLine.Parent.TokenType = tkConstSection) then
      begin
        Result := SearchConstDeclarationInConst(TBlockElement(lline), pConstName);
      end
      else
      begin
        if (lLine.TokenType in [tkProcedureImplementation, tkClassProcedureImplementation,
           tkFunctionImplementation, tkClassFunctionImplementation,
           tkConstructorImplementation, tkClassConstructorImplementation,
           tkDestructorImplementation, tkClassDestructorImplementation]) then
        begin
          for lInternalLine in TBlockElement(lLine).Lines do
          begin
            if (lInternalLine.TokenType = tkConstSection) then
            begin
              Result := SearchConstDeclarationInConst(TBlockElement(lInternalLine), pConstName);
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
  if (not Result) then
  begin
    while Assigned(lLine) and (lLine.TokenType <> tkClassDeclaration) do
    begin
      lLine := lLine.Parent;
    end;
    if (Assigned(lLine)) then
    begin
      for lInternalLine in TBlockElement(lLine).Lines do
      begin
        if (lInternalLine.TokenType in [tkField, tkUnknow]) or lInternalLine.TokenType.IsMethodDeclaration then
        begin
          Result := InternalContainsText(lInternalLine.Text, pConstName);
          if (Result) then
          begin
            Break;
          end;
        end;
      end;
    end;
  end;
end;

constructor TUnitElement.Create;
begin
  inherited Create(True);
  FSourceText := TStringList.Create;
end;

function TUnitElement.CreateLineByTokenType(pTokenType: TTokenType): TLineElement;
begin
  Result := TTokenTypeElement[pTokenType].Create;
end;

destructor TUnitElement.Destroy;
begin
  FreeAndNil(FSourceText);
  FreeAndNil(FClassDeclarationList);
  inherited;
end;

procedure TUnitElement.DoLoad(pText: String);
begin
  Clear;
  FSourceText.Clear;
  FSourceText.Text := pText;
  DoLoadSourceText;
end;

procedure TUnitElement.DoLoadSourceText;
var
  lLine: TLineElement;
  lIndex: Integer;
  lTokenType: TTokenType;
  lText: string;
  lOldLine: TLineElement;
  lLastLineNumber: Integer;
  procedure DoAddLine;
  begin
    FParentLine.AddLine(lLine);
    lLine.Parent := FParentLine;
  end;
  procedure ChangeParentLine;
  begin
    if (lLine.TokenType.IsBlock) then
    begin
      FParentLine := TBlockElement(lLine);
    end;
  end;
begin
  FCurrentSection := tkUnitSection;
  FreeAndNil(FUsesDeclaration);
  FUsesDeclaration := TList<TBlockElement>.Create;
  FParentLine := nil;
  lOldLine := nil;
  for lIndex := 0 to FSourceText.Count - 1 do
  begin
    try
      lText := FSourceText.Strings[lIndex];
      lTokenType := ExtractTokenType(lText);
      lLine := CreateLineByTokenType(lTokenType);
      lLine.Text := lText;
      lLine.TokenType := lTokenType;
      lLine.LineNumber := lIndex + 1;
      lLastLineNumber := lLine.LineNumber;
      if (lLine.TokenType = tkUnitSection) and (FLoadedUnitName.IsEmpty) then
      begin
        ExtractUnitNameFromText(lText);
      end;
      if (lLine.TokenType = tkUsesSection) then
      begin
        FUsesDeclaration.Add(TBlockElement(lLine));
      end;
      if (lLine.TokenType in [tkImplementationSection, tkEndUnit]) then
      begin
        FParentLine := nil;
      end
      else if (lLine.TokenType.IsMethodImplementation) then
      begin
        FParentLine := TBlockElement(GetParentLineForMethodImplementation(lLine));
        if (FParentLine = nil) then
        begin
          FParentLine := TBlockElement(lOldLine);
        end
        else
        begin
          lOldLine := lLine;
        end;
      end;
      if (Assigned(FParentLine)) then
      begin
        if (lLine.TokenType = tkVisibilityEscope) and FParentLine.Finalized then
        begin
          FParentLine := TBlockElement(FParentLine.Parent);
        end
        else if (lLine.TokenType = tkClassDeclaration) and (FParentLine.TokenType = tkClassDeclaration) then
        begin
          if (FParentLine.Finalized) then
          begin
            FParentLine := TBlockElement(FParentLine.Parent);
          end
          else
          begin
            DoAddLine;
          end;
        end;
        if (lLine.TokenType in [tkConstSection, tkVarDeclaration, tkProcedureImplementation, tkVisibilityEscope,
            tkFunctionImplementation, tkBegin]) and (FParentLine.TokenType in [tkVarDeclaration, tkConstSection]) then
        begin
          FParentLine := TBlockElement(FParentLine.Parent);
        end;
        if (Assigned(FParentLine)) then
        begin
          if not (lLine.TokenType in [tkUsesSection, tkTypeSection, tkConstSection, tkVarSection, tkVarDeclaration,
              tkClassDeclaration, tkBlankLine, tkClassOf, tkClassForward, tkImplementationSection, tkComment]) or
              (FInterfaceImplementation = tkImplementationSection)  then
          begin
            DoAddLine;
          end
          else if (lLine.TokenType = tkClassDeclaration) and (FParentLine.TokenType = tkTypeSection) then
          begin
            DoAddLine;
          end;
        end;
      end;
      AddLine(lLine);
      if (lLine.TokenType = tkVisibilityEscope) then
      begin
        if (ContainsText(lLine.Text, ' const')) then
        begin
          lLine := TBlockElement.Create;
          lLine.Text := lText;
          lLine.TokenType := tkConstSection;
          lLine.LineNumber := lIndex + 1;
          DoAddLine;
          AddLine(lLine);
        end;
      end;
      ChangeParentLine;
    except
      on E:Exception do
      begin
        E.RaiseOuterException(Exception.Create('Error reading line:' + lLastLineNumber.ToString));
      end;
    end;
  end;
end;

procedure TUnitElement.ExtractUnitNameFromText(pText: String);
begin
  FLoadedUnitName := Copy(pText, 6, Length(pText)).Trim;
  FLoadedUnitName := Copy(FLoadedUnitName, 1, Length(FLoadedUnitName) - 1).Trim;
end;

function TUnitElement.EndMethodImplementationAfterLineIndex(pLineNumber: Integer): Integer;
var
  lLine: TLineElement;
  lIndex: Integer;
  lInternalLine: TLineElement;
begin
  Result := 0;
  lLine := LineByLineNumber(pLineNumber);
  while Assigned(lLine) and not (lLine.TokenType.IsMethodImplementation) do
  begin
    lLine := lLine.Parent;
  end;
  if Assigned(lLine) and (lLine.TokenType.IsMethodImplementation) then
  begin
    for lIndex := TBlockElement(lLine).Lines.Count - 1 downto 0 do
    begin
      lInternalLine := TBlockElement(lLine).Lines.Items[lIndex];
      if (lInternalLine.TokenType = tkEnd) then
      begin
        Result := lInternalLine.LineNumber;
        Break;
      end;
    end;
  end;
end;

function TUnitElement.EndOfUsesDeclaration(pInterface: Boolean): TUsesDeclarationResult;
var
  lLine: TLineElement;
  lIndex: Integer;
  lInternalLine: TLineElement;
  lUsesLine: TLineElement;
begin
  Result := TUsesDeclarationResult.Create;
  if (pInterface) then
  begin
    lLine := LineByTokenType(tkInterfaceSection);
  end
  else
  begin
    lLine := LineByTokenType(tkImplementationSection);
  end;
  lUsesLine := nil;
  if (Assigned(lLine)) then
  begin
    for lIndex := IndexOf(lLine) + 1 to Count - 1 do
    begin
      if (Items[lIndex].TokenType = tkUsesSection) then
      begin
        lUsesLine := Items[lIndex];
        Break
      end;
      if (pInterface) then
      begin
        if (Items[lIndex].TokenType = tkImplementationSection) then
        begin
          Break;
        end;
      end;
    end;
    if (Assigned(lUsesLine)) then
    begin
      Result.IsDeclaration := True;
      for lIndex := TBlockElement(lUsesLine).Lines.Count - 1 downto 0 do
      begin
        lInternalLine := TBlockElement(lUsesLine).Lines.Items[lIndex];
        if EndsText(';', lInternalLine.Text) then
        begin
          Result.LineNumber := lInternalLine.LineNumber;
        end;
      end;
    end
    else
    begin
      Result.LineNumber := lLine.LineNumber;
    end;
  end;
end;

function TUnitElement.LineByTokenType(pTokenType: TTokenType): TLineElement;
var
  lLine: TLineElement;
begin
  Result := nil;
  for lLine in Self do
  begin
    if (lLine.TokenType = pTokenType) then
    begin
      Result := lLine;
      Break;
    end;
  end;
end;

function TUnitElement.ExtractMethodName(pText: String): String;
const
  cReservedMethodWords: Array [0..7] of String = (' abstract;', ' sealed;', ' final;', ' override;',
      ' reintroduce;', ' virtual;', ' dynamic;', ' overload;');
var
  lIndex: Integer;
begin
  Result := Trim(pText);
  for lIndex := Low(cReservedMethodWords) to High(cReservedMethodWords) do
  begin
    Result := ReplaceText(Result, cReservedMethodWords[lIndex], '');
  end;
  if (ContainsText(Result, '(')) then
  begin
    Result := Trim(Copy(Result, 1, Pos('(', Result) - 1));
  end
  else if (ContainsText(Result, ':')) then
  begin
    Result := Trim(Copy(Result, 1, Pos(':', Result) - 1));
  end;
  if (Pos('.', Result) > 0) then
  begin
    Result := Trim(Copy(Result, Pos('.', Result) + 1, Length(Result)));
  end
  else
  begin
    repeat
      Result := Trim(Copy(Result, Pos(' ', Result) + 1, Length(Result)));
    until not ContainsText(Result, ' ');
  end;
  if (EndsText(';', Result)) then
  begin
    Result := Trim(Copy(Result, 1, Length(Result) - 1));
  end;
end;

function TUnitElement.ExtractDisplayMethodName(pMethod: TLineElement): String;
var
  lMethod: TLineElement;
begin
  Result := Trim(pMethod.Text);
  if (Pos('(', pMethod.Text) > 0) then
  begin
    Result := Trim(Copy(pMethod.Text, 1, Pos('(', pMethod.Text) - 1));
  end
  else if (Pos(':', pMethod.Text) > 0) then
  begin
    Result := Trim(Copy(pMethod.Text, 1, Pos(':', pMethod.Text) - 1));
  end
  else if (Pos(';', pMethod.Text) > 0) then
  begin
    Result := Trim(Copy(pMethod.Text, 1, Pos(';', pMethod.Text)- 1));
  end;
  if (pMethod.TokenType in [tkFunctionDeclaration, tkClassFunctionDeclaration]) then
  begin
    if (ContainsText(pMethod.Text, '(')) then
    begin
      lMethod := pMethod;
      while Assigned(lMethod) and not ContainsText(lMethod.Text, '):') do
      begin
        lMethod := LineByLineNumber(lMethod.LineNumber + 1);
      end;
      if (Assigned(lMethod)) then
      begin
        Result := Result + Copy(lMethod.Text, Pos('):', lMethod.Text) + 1, Length(lMethod.Text));
      end;
    end
    else
    begin
      Result := Result + Copy(pMethod.Text, Pos(':', pMethod.Text), Length(pMethod.Text));
    end;
  end
  else
  begin
    Result := Result + ';';
  end;
end;

function TUnitElement.ExtractTokenType(pText: String): TTokenType;
var
  lText: string;
  function IsWord(pWord: String): Boolean;
  begin
    Result := SameText(Copy(lText, 1, Pos(' ', lText) - 1), pWord) or SameText(lText, pWord)
  end;
  function ParentLineIsClassDeclaration: Boolean;
  begin
    Result := Assigned(FParentLine) and (FParentLine.TokenType = tkClassDeclaration);
  end;
begin
  Result := tkUnknow;
  lText := Trim(pText);
  if (lText = '') then
  begin
    Result := tkBlankLine;
  end
  else if IsWord('begin') then
  begin
    Result := tkBegin;
  end
  else if IsWord('const') and not EndsText(');', lText) then
  begin
    Result := tkConstSection;
  end
  else if StartsText('end;', lText) then
  begin
    Result := tkEnd;
  end
  else if StartsText('end.', lText) then
  begin
    Result := tkEndUnit;
  end
  else if IsWord('var') and not EndsText(');', lText) and not ParentLineIsClassDeclaration then
  begin
    Result := tkVarDeclaration;
  end
  else if StartsText('class procedure', lText) then
  begin
    case FInterfaceImplementation of
      tkInterfaceSection:
        Result := tkClassProcedureDeclaration;
      else
        Result := tkClassProcedureImplementation;
    end;
  end
  else if StartsText('class function', lText) then
  begin
    case FInterfaceImplementation of
      tkInterfaceSection:
        Result := tkClassFunctionDeclaration;
      else
        Result := tkClassFunctionImplementation;
    end;
  end
  else if IsWord('function') or IsWord('operator') then
  begin
    case FInterfaceImplementation of
      tkInterfaceSection:
        Result := tkFunctionDeclaration;
      else
        Result := tkFunctionImplementation;
    end;
  end
  else if IsWord('procedure') then
  begin
    case FInterfaceImplementation of
      tkInterfaceSection:
        Result := tkProcedureDeclaration;
      else
        Result := tkProcedureImplementation;
    end;
  end
  else if IsWord('unit') then
  begin
    Result := tkUnitSection;
    FCurrentSection := Result;
    FInterfaceImplementation := tkUnknow;
  end
  else if IsWord('interface') then
  begin
    Result := tkInterfaceSection;
    FCurrentSection := Result;
    FInterfaceImplementation := Result;
  end
  else if IsWord('initialization') then
  begin
    Result := tkInitializationSection;
    FCurrentSection := Result;
  end
  else if IsWord('finalization') then
  begin
    Result := tkFinalizationSection;
    FCurrentSection := Result;
  end
  else if IsWord('uses') then
  begin
    Result := tkUsesSection;
    FCurrentSection := Result;
  end
  else if IsWord('type') then
  begin
    Result := tkTypeSection;
    FCurrentSection := Result;
  end
  else if IsWord('implementation') then
  begin
    Result := tkImplementationSection;
    FCurrentSection := Result;
    FInterfaceImplementation := Result;
  end
  else if StartsText('{', lText) or StartsText('//', lText) or StartsText('(*', lText) then
  begin
    Result := tkComment;
  end
  else if IsWord('destructor') then
  begin
    case FInterfaceImplementation of
      tkInterfaceSection:
        Result := tkDestrutorDeclaration;
      else
        Result := tkDestructorImplementation;
    end;
  end
  else if IsWord('constructor') then
  begin
    case FInterfaceImplementation of
      tkInterfaceSection:
        Result := tkConstructorDeclaration;
      else
        Result := tkConstructorImplementation;
    end;
  end
  else if StartsText('class destructor', lText) then
  begin
    case FInterfaceImplementation of
      tkInterfaceSection:
        Result := tkClassDestructorDeclaration;
      else
        Result := tkClassDestructorImplementation;
    end;
  end
  else if StartsText('class constructor', lText) then
  begin
    case FInterfaceImplementation of
      tkInterfaceSection:
        Result := tkClassConstructorDeclaration;
      else
        Result := tkClassConstructorImplementation;
    end;
  end
  else if IsWord('strict') or IsWord('private') or IsWord('protected') or IsWord('public') or IsWord('published') then
  begin
    Result := tkVisibilityEscope;
  end
  else if StartsText('class var', lText) then
  begin
    Result := tkClassField;
  end
  else if StartsText('[', lText) then
  begin
    Result := tkAttribute;
  end
  else if ContainsText(lText, '= class of ') then
  begin
    Result := tkClassOf;
  end
  else if ContainsText(lText, '= class;') then
  begin
    Result := tkClassForward;
  end
  else if ContainsText(lText, '= class') then
  begin
    Result := tkClassDeclaration;
  end
  else if IsWord('property') or StartsText('class property', lText) then
  begin
    Result := tkProperty;
  end
  else if ContainsText(lText, ':') and (not ContainsText(lText, '=')) and not ContainsText(lText, ',') and EndsText(';', lText) then
  begin
    Result := tkField;
  end;
end;

function TUnitElement.ExtractVisibilityScopeFromText(pText: String): TVisibilityScope;
var
  lIndex: Integer;
begin
  Result := vsUnknow;
  for lIndex := Ord(Low(TVisibilityScope)) to Ord(High(TVisibilityScope)) do
  begin
    if (ContainsText(pText, TVisibilityScope(lIndex).ToString)) then
    begin
      Result := TVisibilityScope(lIndex);
      Break;
    end;
  end;
end;

procedure TUnitElement.GetConstantClassDeclarationFromLine(pLine: TLineElement; pClassDeclaration: TBlockElement;
  pConstantDeclaration: TConstantDeclarationResult; pVisibilityScope: TVisibilityScope);
var
  lIndex: Integer;
  lLineAux: TLineElement;
  lConstLine: TLineElement;
begin
  if (ExtractVisibilityScopeFromText(pLine.Text) = pVisibilityScope) then
  begin
    lIndex := pClassDeclaration.Lines.IndexOf(pLine) + 1;
    if (EndsText(' const', pLine.Text)) or ContainsText(pLine.Text, ' const ') then
    begin
      lConstLine := pClassDeclaration.Lines.Items[lIndex];
      pConstantDeclaration.LineNumber := TBlockElement(lConstLine).Lines.Last.LineNumber;
      pConstantDeclaration.IsDeclaration := True;
    end
    else
    begin
      lLineAux := pClassDeclaration.Lines.Items[lIndex];
      while Assigned(lLineAux) and (lLineAux.TokenType <> tkConstSection) do
      begin
        Inc(lIndex);
        if (lIndex <= pClassDeclaration.Lines.Count - 1) then
        begin
          lLineAux := pClassDeclaration.Lines.Items[lIndex];
          if (lLineAux.TokenType = tkVisibilityEscope) then
          begin
            GetConstantClassDeclarationFromLine(lLineAux, pClassDeclaration, pConstantDeclaration, pVisibilityScope);
          end;
        end
        else
        begin
          lLineAux := nil;
        end;
      end;
      if (pClassDeclaration.LineNumber = 0) and (Assigned(lLineAux) and (lLineAux.TokenType = tkConstSection)) then
      begin
        pConstantDeclaration.LineNumber := TBlockElement(lLineAux).Lines.Last.LineNumber;
        pConstantDeclaration.IsDeclaration := True;
      end;
    end;
  end;
end;

function TUnitElement.GetParentLineForMethodImplementation(pLine: TLineElement): TLineElement;
var
  lClassName, lClassNameDeclaration: string;
  lInitialPos: Integer;
  lLine: TLineElement;
  lStrings: TStringList;
  lMethodNameDeclaration: String;
  lMethodNameImplementation: String;
begin
  Result := nil;
  lClassName := Trim(pLine.Text);
  lInitialPos := 0;
  case pLine.TokenType of
    tkFunctionImplementation, tkClassFunctionImplementation: lInitialPos := Pos('FUNCTION', UpperCase(lClassName)) + 8;
    tkProcedureImplementation, tkClassProcedureImplementation: lInitialPos := Pos('PROCEDURE', UpperCase(lClassName)) + 9;
    tkDestructorImplementation, tkClassDestructorImplementation: lInitialPos := Pos('DESTRUCTOR', UpperCase(lClassName)) + 10;
    tkConstructorImplementation, tkClassConstructorImplementation: lInitialPos := Pos('CONSTRUCTOR', UpperCase(lClassName)) + 11;
  end;
  lClassName := Trim(Copy(lClassName, lInitialPos, Length(lClassName)));
  lStrings := TStringList.Create;
  try
    ExtractStrings(['.'], [' '], PWideChar(lClassName), lStrings);
    if (lStrings.Count > 1) then
    begin
      lClassName := lStrings.Strings[lStrings.Count - 2];
    end;
//    lInitialPos := Pos('.', lClassName) - 1;
//    lClassName := Copy(lClassName, 1, lInitialPos);
  finally
    lStrings.Free;
  end;
  if ContainsText(lClassName, '<') then
  begin
    lInitialPos := Pos('<', lClassName) - 1;
    lClassName := Copy(lClassName, 1, lInitialPos);
  end;
  for lLine in Self do
  begin
    if (lLine.TokenType = tkClassDeclaration) then
    begin
      lClassNameDeclaration := Trim(lLine.Text);
      lInitialPos := Pos('=', lClassNameDeclaration) - 1;
      lClassNameDeclaration := Trim(Copy(lClassNameDeclaration, 1, lInitialPos));
      if ContainsText(lClassNameDeclaration, '<') then
      begin
        lInitialPos := Pos('<', lClassNameDeclaration) - 1;
        lClassNameDeclaration := Copy(lClassNameDeclaration, 1, lInitialPos);
      end;
      if SameText(lClassNameDeclaration, lClassName) then
      begin
        Result := lLine;
        Break;
      end;
    end;
  end;
  if (Assigned(Result)) then
  begin
    for lLine in TBlockElement(Result).Lines do
    begin
      if lLine.TokenType.IsMethodDeclaration then
      begin
        lMethodNameDeclaration := ExtractMethodName(lLine.Text);
        lMethodNameImplementation := ExtractMethodName(pLine.Text);
        if SameText(lMethodNameDeclaration, lMethodNameImplementation) then
        begin
          Result := llIne;
          Break;
        end;
      end;
    end;
  end;
end;

function TUnitElement.InternalContainsText(pSearchText, pText: string): Boolean;
var
  lStrings: TStrings;
  lString: String;
begin
  Result := False;
  lStrings := TStringList.Create;
  try
    ExtractStrings(['.', ',', ';', '(', ')', ' ', '[', ']', ':', '=', #13, #10], [' '], pWideChar(pSearchText), lStrings);
    for lString in lStrings do
    begin
      Result := SameText(lString, pText);
      if (Result) then
      begin
        Break;
      end;
    end;
  finally
    lStrings.Free;
  end;
end;

procedure TUnitElement.LoadFromFile(pFileName: string);
var
  lStrings: TStrings;
begin
  if (TFile.Exists(pFileName)) then
  begin
    lStrings := TStringList.Create;
    try
      lStrings.LoadFromFile(pFileName);
      LoadFromText(lStrings.Text);
      FLoadedUnitName := ExtractFileName(pFileName);
    finally
      lStrings.Free;
    end;
  end;
end;

procedure TUnitElement.LoadFromStrings(pStrings: TStrings);
begin
  LoadFromText(pStrings.Text);
end;

procedure TUnitElement.LoadFromText(pText: string);
begin
  if (Trim(pText) <> '') then
  begin
    FLoadedUnitName := '';
    DoLoad(pText);
  end;
end;

procedure TUnitElement.SaveToFile(pFileName: String);
var
  lStrings: TStrings;
  lLine: TLineElement;
begin
  lStrings := TStringList.Create;
  try
    for lLine in Self do
    begin
      lStrings.Add(lLine.Text);
    end;
    lStrings.SaveToFile(pFileName);
  finally
    lStrings.Free;
  end;
end;

function TUnitElement.SearchConstDeclarationInConst(pConstDeclaration: TBlockElement; pConstName: String): Boolean;
begin
  Result := SearchIdentifierInSection(pConstDeclaration, pConstName);
end;

function TUnitElement.SearchIdentifierInSection(pSectionDeclaration: TBlockElement; pIdentifierName: String): Boolean;
var
  lInternalLine: TLineElement;
begin
  Result := False;
  for lInternalLine in pSectionDeclaration.Lines do
  begin
    if not (lInternalLine.TokenType in [tkComment, tkBlankLine]) then
    begin
      Result := InternalContainsText(lInternalLine.Text, pIdentifierName);
      if (Result) then
      begin
        Break;
      end;
    end;
  end;
end;

function TUnitElement.SearchVarDeclarationInVar(pVarDeclaration: TBlockElement; pVarName: String): Boolean;
begin
  Result := SearchIdentifierInSection(pVarDeclaration, pVarName);
end;

function TUnitElement.SectionByLineNumber(pLineNumber: Integer): TTokenType;
var
  lLine: TLineElement;
begin
  lLine := LineByLineNumber(pLineNumber);
  while Assigned(lLine) and (not (lLine.TokenType in [tkInterfaceSection, tkImplementationSection])) do
  begin
    lLine := LineByLineNumber(lLine.LineNumber - 1);
  end;
  if (not Assigned(lLine)) then
  begin
    Result := tkInterfaceSection;
  end
  else
  begin
    Result := lLine.TokenType;
  end;
end;

function TUnitElement.UsesList: TStrings;
var
  lLine: TLineElement;
  lUses: TBlockElement;
begin
  Result := TStringList.Create;
  for lUses in FUsesDeclaration do
  begin
    for lLine in lUses.Lines do
    begin
      if not (lLine.TokenType in [tkBlankLine, tkComment]) then
      begin
        ExtractUsesFromLineText(lLine.Text, Result);
      end;
    end;
  end;
  if (FLoadedUnitName <> '') then
  begin
    Result.Add(FLoadedUnitName);
  end;
end;

procedure TUnitElement.ExtractUsesFromLineText(pText: String; pUsesList: TStrings);
var
  lStrings: TStrings;
  lString: String;
begin
  lStrings := TStringList.Create;
  ExtractStrings([',', ';'], [' '], PWideChar(pText), lStrings);
  for lString in lStrings do
  begin
    pUsesList.Add(lString);
  end;
end;

function TUnitElement.VariableDeclarationBeforeLineIndex(pLineNumber: Integer): TVariableDeclarationResult;
var
  lLine: TLineElement;
  lInternalLine: TLineElement;
begin
  Result := TVariableDeclarationResult.Create;
  lLine := LineByLineNumber(pLineNumber);
  if (lLine.TokenType <> tkVarDeclaration) then
  begin
    if (lLine.Parent <> nil) then
    begin
      if (lLine.Parent.TokenType = tkVarDeclaration) then
      begin
        Result.LineNumber := TBlockElement(lLine.Parent).Lines.Last.LineNumber;
      end
      else
      begin
        lline := lline.Parent;
        if (lLine.TokenType in [tkProcedureImplementation, tkClassProcedureImplementation,
           tkFunctionImplementation, tkClassFunctionImplementation,
           tkConstructorImplementation, tkClassConstructorImplementation,
           tkDestructorImplementation, tkClassDestructorImplementation]) then
        begin
          for lInternalLine in TBlockElement(lLine).Lines do
          begin
            if (lInternalLine.TokenType = tkVarDeclaration) then
            begin
              Result.LineNumber := TBlockElement(lInternalLine).Lines.Last.LineNumber;
              Result.IsDeclaration := True;
              Break;
            end;
          end;
          if (Result.LineNumber = 0) then
          begin
            for lInternalLine in TBlockElement(lLine).Lines do
            begin
              if (lInternalLine.TokenType = tkBegin) then
              begin
                Result.LineNumber := lInternalLine.LineNumber - 1;
                Break;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TUnitElement.VariableDeclarationExistsBeforeLineIndex(pLineNumber: Integer; pVarName: String): Boolean;
var
  lLine: TLineElement;
  lInternalLine: TLineElement;
begin
  Result := False;
  lLine := LineByLineNumber(pLineNumber);
  if (lLine.TokenType = tkVarDeclaration) then
  begin
    Result := SearchVarDeclarationInVar(TBlockElement(lLine), pVarName);
  end;
  if (not Result) then
  begin
    if (lLine.Parent <> nil) then
    begin
      lline := lline.Parent;
      if (lLine.Parent.TokenType = tkVarDeclaration) then
      begin
        Result := SearchVarDeclarationInVar(TBlockElement(lline), pVarName);
      end
      else
      begin
        if (lLine.TokenType in [tkProcedureImplementation, tkClassProcedureImplementation,
           tkFunctionImplementation, tkClassFunctionImplementation,
           tkConstructorImplementation, tkClassConstructorImplementation,
           tkDestructorImplementation, tkClassDestructorImplementation]) then
        begin
          for lInternalLine in TBlockElement(lLine).Lines do
          begin
            if (lInternalLine.TokenType = tkVarDeclaration) then
            begin
              Result := SearchVarDeclarationInVar(TBlockElement(lInternalLine), pVarName);
              Break;
            end;
          end;
        end;
      end;
    end;
  end;
  if (not Result) then
  begin
    while Assigned(lLine) and (lLine.TokenType <> tkClassDeclaration) do
    begin
      lLine := lLine.Parent;
    end;
    if (Assigned(lLine)) then
    begin
      for lInternalLine in TBlockElement(lLine).Lines do
      begin
        if (lInternalLine.TokenType in [tkField, tkUnknow]) or lInternalLine.TokenType.IsMethodDeclaration then
        begin
          Result := InternalContainsText(lInternalLine.Text, pVarName);
          if (Result) then
          begin
            Break;
          end;
        end;
      end;
    end;
  end;
end;

end.