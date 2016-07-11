unit UnitReader.ClassElement;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  UnitReader.EnumerationTypes,
  AM.UnitReader.Enumerations,
  UnitReader.Elements;

type
  TVisibilityScopeDeclaration = class
  private
    FIsDeclaration: Boolean;
    FLineNumber: Integer;
  public
    property IsDeclaration: Boolean read FIsDeclaration write FIsDeclaration;
    property LineNumber: Integer read FLineNumber write FLineNumber;
  end;

  TClassElement = class
  strict private
    FScopeList: TList<TLineElement>;
    FMethodDeclarationList: TList<TLineElement>;
    FClassDeclaration: TBlockElement;
    procedure ExtractLists;
    function ExtractVisibilityScopeFromText(pText: String): TVisibilityScope;
  private
    function GetClassElementName: String;
  public
    constructor Create(pClassDeclaration: TBlockElement);
    function ExtractLineFromInexistentVisibilityScope(pVisibilityScope: TVisibilityScope): Integer;
    function ExtractLineForVisibilityScope(pVisibilityScope: TVisibilityScope): TVisibilityScopeDeclaration;
    property ScopeList: TList<TLineElement> read FScopeList;
    property MethodDeclarationList: TList<TLineElement> read FMethodDeclarationList;
    property ClassDeclaration: TBlockElement read FClassDeclaration;
    property ClassElementName: String read GetClassElementName;
  end;

implementation

{ TClassElement }

uses
  System.StrUtils,
  UnitReader.TokenTypeHelper,
  AM.UnitReader.Helper.MemberVisibility;

constructor TClassElement.Create(pClassDeclaration: TBlockElement);
begin
  FClassDeclaration := pClassDeclaration;
  FScopeList := TList<TLineElement>.Create;
  FMethodDeclarationList := TList<TLineElement>.Create;
  ExtractLists;
end;

function TClassElement.ExtractLineForVisibilityScope(pVisibilityScope: TVisibilityScope): TVisibilityScopeDeclaration;
var
  lScope: TLineElement;
begin
  Result := TVisibilityScopeDeclaration.Create;
  for lScope in FScopeList do
  begin
    if (ExtractVisibilityScopeFromText(lScope.Text) = pVisibilityScope) then
    begin
      Result.IsDeclaration := True;
      Result.LineNumber := FScopeList.Items[FScopeList.IndexOf(lScope) + 1].LineNumber - 1;
      Break;
    end;
  end;
  if (Result.LineNumber = 0) then
  begin
    Result.LineNumber := ExtractLineFromInexistentVisibilityScope(pVisibilityScope);
  end;
end;

function TClassElement.ExtractLineFromInexistentVisibilityScope(pVisibilityScope: TVisibilityScope): Integer;
var
  lLine: TLineElement;
  lVisibilityScope: TVisibilityScope;
  lIndex: Integer;
begin
  Result := 0;
  for lIndex := Ord(pVisibilityScope) + 1 to Ord(High(TVisibilityScope)) do
  begin
    lVisibilityScope := TVisibilityScope(lIndex);
    for lLine in FScopeList do
    begin
      if (ExtractVisibilityScopeFromText(lLine.Text) = lVisibilityScope) then
      begin
        Result := lLine.LineNumber - 1;
        Break;
      end;
    end;
    if (Result > 0) then
    begin
      Break;
    end;
  end;
end;

procedure TClassElement.ExtractLists;
var
  lLine: TLineElement;
begin
  FMethodDeclarationList.Clear;
  FScopeList.Clear;
  for lLine in FClassDeclaration.Lines do
  begin
    if lLine.TokenType.IsMethodDeclaration then
    begin
      FMethodDeclarationList.Add(lLine);
    end
    else if (lLine.TokenType = tkVisibilityEscope) then
    begin
      FScopeList.Add(lLine);
    end;
  end;
end;

function TClassElement.ExtractVisibilityScopeFromText(pText: String): TVisibilityScope;
var
  lIndex: Integer;
begin
  Result := vsUnknow;
  for lIndex := Ord(Low(TVisibilityScope)) to Ord(High(TVisibilityScope)) do
  begin
    if (ContainsText(pText, TVisibilityScope(lIndex).ToString)) and not ContainsText(pText, 'const') then
    begin
      Result := TVisibilityScope(lIndex);
      Break;
    end;
  end;
end;

function TClassElement.GetClassElementName: String;
begin
  Result := FClassDeclaration.Text;
  Result := Trim(Copy(Result, 1, Pos('=', Result) - 1));
end;

end.
