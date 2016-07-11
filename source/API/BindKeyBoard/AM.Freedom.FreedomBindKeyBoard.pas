unit AM.Freedom.FreedomBindKeyBoard;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  ToolsAPI,
  UnitReader.UnitElement,
  AM.Freedom.Consts;

type
  TShortCutArray = array of TShortCut;
  TKeyExecutorClass = class of TCustomKeyExecutor;

  TCustomKeyExecutor = class
  strict private
    function GoBackward(pText: string; var pCanContinue: Boolean): Boolean;
    function GoForward(pText: string; var pCanContinue: Boolean; pIsMethod: Boolean = False): Boolean;
  protected
    function GetShortCut: TShortCut; virtual;
    function GetSelectedBlockText(const pContext: IOTAKeyContext): string;
    function GetBlockText(const pContext: IOTAKeyContext; pInitialCharPos, pEndCharPos: TOTACharPos): string;
    function GetUnitElement(const pContext: IOTAKeyContext): TUnitElement;
    procedure GoBackToPos(const pContext: IOTAKeyContext; pStartPos: TOTAEditPos);
    function ExtractIdentifierAtCursor(const pContext: IOTAKeyContext; var pCanContinue: Boolean; pIsMethod: Boolean = False): string;
    procedure FixSelectedBlock(pContext: IOTAKeyContext);
  public
    property ShortCut: TShortCut read GetShortCut;
    procedure Execute(const pContext: IOTAKeyContext); virtual;
    class function Description: string; virtual;
  end;

  TFreedomBindKeyBoard = class(TNotifierObject, IOTAKeyboardBinding)
  strict private
    class var FKeyExecutors: TObjectList<TCustomKeyExecutor>;
    class var FRegisteredIndex: Integer;
    class procedure CreateExecutors;
  strict private
    procedure BindKeyBoardProc(const Context: IOTAKeyContext; KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
    function GetBindingType: TBindingType;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
    function GetDisplayName: string;
    function GetName: string;
  private
    class procedure DestroyExecutors;
    class procedure RemoveKeyBindings;
  public
    class procedure RegisterKeyExecutor(pKeyExecutorClass: TKeyExecutorClass);
    class procedure UnregisterKeyExecutor(pKeyExecutorClass: TKeyExecutorClass);
    class function KeyExecutorList: TObjectList<TCustomKeyExecutor>;
    class procedure RegisterKeyBindings;
    class procedure RefreshKeyBindings;
  end;

implementation

{ TCustomKeyExecutors }

uses
  AM.Freedom.Exceptions,
  System.StrUtils;

class function TCustomKeyExecutor.Description: string;
begin
  raise EInvalidMethodCallOnClass.Create('Description', ClassName)
end;

procedure TCustomKeyExecutor.Execute(const pContext: IOTAKeyContext);
begin
  raise EInvalidMethodCallOnClass.Create('Execute', ClassName);
end;

function TCustomKeyExecutor.ExtractIdentifierAtCursor(const pContext: IOTAKeyContext; var pCanContinue: Boolean; pIsMethod: Boolean): string;
var
  lSelectText: string;
  lNroIteration: Integer;
  lIsBeginOfLine: Boolean;
begin
  lIsBeginOfLine := False;
  lSelectText := pContext.EditBuffer.EditPosition.Read(1);
  lNroIteration := 1;
  Result := '';
  pCanContinue := True;
  if (lSelectText.Trim = '') then
  begin
    pContext.EditBuffer.EditPosition.MoveRelative(0, -1);
    lSelectText := pContext.EditBuffer.EditPosition.Read(1);
  end;
  while GoBackward(lSelectText, pCanContinue) do
  begin
    if pContext.EditBuffer.EditPosition.MoveRelative(0, -1) then
    begin
      lSelectText := pContext.EditBuffer.EditPosition.Read(1);
      Inc(lNroIteration);
      if (lNroIteration > 250) then
      begin
        Break;
      end;
    end
    else
    begin
      lIsBeginOfLine := True;
      Break;
    end;
  end;
  if (pCanContinue) then
  begin
    if (not lIsBeginOfLine) then
    begin
      pContext.EditBuffer.EditPosition.MoveRelative(0, 1);
    end;
    lNroIteration := 1;
    lSelectText := '';
    while GoForward(lSelectText, pCanContinue, pIsMethod) do
    begin
      lSelectText := pContext.EditBuffer.EditPosition.Read(lNroIteration);
      Inc(lNroIteration);
      if (lNroIteration > 250) then
      begin
        Break;
      end;
    end;
    if pCanContinue and (lSelectText <> '') then
    begin
      Delete(lSelectText, Length(lSelectText), 1);
      Result := Trim(lSelectText);
    end;
  end;
end;

procedure TCustomKeyExecutor.FixSelectedBlock(pContext: IOTAKeyContext);
var
  lBlockStart, lBlockEnd: TOTACharPos;
  lEditPos: TOTAEditPos;
  lCharPos: TOTACharPos;
  lFinalEditPos: TOTAEditPos;
  lFinalCharPos: TOTACharPos;
  lSelectedBlockText: String;
begin
  lSelectedBlockText := GetSelectedBlockText(pContext);
  if (lSelectedBlockText <> '') then
  begin
    if (pContext.EditBuffer.EditBlock.EndingColumn = 1) then
    begin
      lBlockStart := pContext.EditBuffer.BlockStart;
      pContext.EditBuffer.EditPosition.MoveBOL;
      pContext.EditBuffer.EditPosition.MoveRelative(-1, 0);
      pContext.EditBuffer.EditPosition.MoveEOL;
      lEditPos := pContext.EditBuffer.TopView.CursorPos;
      pContext.EditBuffer.TopView.ConvertPos(True, lEditPos, lCharPos);
      pContext.EditBuffer.BlockStart := lBlockStart;
      pContext.EditBuffer.BlockAfter := lCharPos;
      pContext.EditBuffer.BlockVisible := True;
      pContext.EditBuffer.BlockType := btNonInclusive;
    end
    else
    begin
      lBlockStart := pContext.EditBuffer.BlockStart;
      pContext.EditBuffer.EditPosition.MoveEOL;
      lEditPos := pContext.EditBuffer.TopView.CursorPos;
      pContext.EditBuffer.TopView.ConvertPos(True, lEditPos, lCharPos);
      pContext.EditBuffer.BlockStart := lBlockStart;
      pContext.EditBuffer.BlockAfter := lCharPos;
      pContext.EditBuffer.BlockVisible := True;
      pContext.EditBuffer.BlockType := btNonInclusive;
    end;
    if (pContext.EditBuffer.EditBlock.StartingColumn <> 1) then
    begin
      lBlockEnd := pContext.EditBuffer.BlockAfter;
      lBlockStart := pContext.EditBuffer.BlockStart;
      pContext.EditBuffer.EditPosition.GotoLine(lBlockStart.Line);
      pContext.EditBuffer.EditPosition.MoveBOL;
      lEditPos := pContext.EditBuffer.TopView.CursorPos;
      pContext.EditBuffer.TopView.ConvertPos(True, lEditPos, lBlockStart);
      pContext.EditBuffer.BlockStart := lBlockStart;
      pContext.EditBuffer.BlockAfter := lBlockEnd;
      pContext.EditBuffer.BlockVisible := True;
      pContext.EditBuffer.BlockType := btNonInclusive;
    end;
  end
  else if (lSelectedBlockText = '') then
  begin
    pContext.EditBuffer.EditPosition.MoveBOL;
    lEditPos := pContext.EditBuffer.TopView.CursorPos;
    pContext.EditBuffer.EditPosition.MoveEOL;
    lFinalEditPos := pContext.EditBuffer.TopView.CursorPos;
    pContext.EditBuffer.TopView.ConvertPos(True, lEditPos, lCharPos);
    pContext.EditBuffer.TopView.ConvertPos(True, lFinalEditPos, lFinalCharPos);
    pContext.EditBuffer.BlockStart := lCharPos;
    pContext.EditBuffer.BlockAfter := lFinalCharPos;
    pContext.EditBuffer.BlockType := btNonInclusive;
    pContext.EditBuffer.BlockVisible := True;
  end;
end;

function TCustomKeyExecutor.GetBlockText(const pContext: IOTAKeyContext; pInitialCharPos, pEndCharPos: TOTACharPos): string;
var
  lReader: IOTAEditReader;
  lPosition, lCount: Integer;
  lText: AnsiString;
  lIndex: Integer;
begin
  lPosition := pContext.EditBuffer.TopView.CharPosToPos(pInitialCharPos);
  lCount := pContext.EditBuffer.TopView.CharPosToPos(pEndCharPos);
  lCount := lCount - lPosition;
  for lIndex := 1 to lCount do
  begin
    lText := lText + ' ';
  end;
  lReader := pContext.EditBuffer.CreateReader;
  try
    lReader.GetText(lPosition, PAnsiChar(lText), lCount);
  finally
    lReader := nil;
  end;
  Result := UTF8ToAnsi(lText);
end;

function TCustomKeyExecutor.GetSelectedBlockText(const pContext: IOTAKeyContext): string;
begin
  Result := GetBlockText(pContext, pContext.EditBuffer.BlockStart, pContext.EditBuffer.BlockAfter);
end;

function TCustomKeyExecutor.GetShortCut: TShortCut;
begin
  Result := 0;
end;

function TCustomKeyExecutor.GetUnitElement(const pContext: IOTAKeyContext): TUnitElement;
var
  lBeginOfFile, lEndOfFile: TOTACharPos;
  lEditPos: TOTAEditPos;
  lUnitText: string;
begin
  lBeginOfFile.CharIndex := 0;
  lBeginOfFile.Line := 1;
  lEditPos.Line := pContext.EditBuffer.EditPosition.LastRow;
  lEditPos.Col := 5;
  pContext.EditBuffer.TopView.ConvertPos(True, lEditPos, lEndOfFile);
  lUnitText := GetBlockText(pContext, lBeginOfFile, lEndOfFile);
  Result := TUnitElement.Create;
  Result.LoadFromText(lUnitText);
end;

procedure TCustomKeyExecutor.GoBackToPos(const pContext: IOTAKeyContext; pStartPos: TOTAEditPos);
var
  lEditPosition: IOTAEditPosition;
begin
  lEditPosition := pContext.EditBuffer.EditPosition;
  lEditPosition.GotoLine(pStartPos.Line);
  pContext.EditBuffer.EditPosition.MoveBOL;
  pContext.EditBuffer.EditPosition.MoveRelative(0, pStartPos.Col - 1);
end;

function TCustomKeyExecutor.GoBackward(pText: string; var pCanContinue: Boolean): Boolean;
var
  lIndex: Integer;
begin
  for lIndex := low(TConsts.cStringDelimiters) to high(TConsts.cStringDelimiters) do
  begin
    Result := not SameText(TConsts.cStringDelimiters[lIndex], pText);
    if (pText = '.') then
    begin
      pCanContinue := False;
      Result := False;
    end;
    if (not Result) then
    begin
      Break;
    end;
  end;
end;

function TCustomKeyExecutor.GoForward(pText: string; var pCanContinue: Boolean; pIsMethod: Boolean): Boolean;
var
  lIndex: Integer;
begin
  pCanContinue := True;
  for lIndex := low(TConsts.cStringDelimiters) to high(TConsts.cStringDelimiters) do
  begin
    Result := not EndsText(TConsts.cStringDelimiters[lIndex], pText);
    if (EndsText('(', pText)) then
    begin
      pCanContinue := pIsMethod;
      Result := False;
    end;
    if (not Result) then
    begin
      Break;
    end;
  end;
end;
{ TFreedomBindKeyBoard }

procedure TFreedomBindKeyBoard.BindKeyboard(const BindingServices: IOTAKeyBindingServices);
var
  lKeyExecutor: TCustomKeyExecutor;
begin
  for lKeyExecutor in FKeyExecutors do
  begin
    if (lKeyExecutor.ShortCut <> 0) then
    begin
      BindingServices.AddKeyBinding([lKeyExecutor.ShortCut], BindKeyBoardProc, lKeyExecutor);
    end;
  end;
end;

procedure TFreedomBindKeyBoard.BindKeyBoardProc(const Context: IOTAKeyContext; KeyCode: TShortCut; var BindingResult: TKeyBindingResult);
begin
  if (Context.Context <> nil) then
  begin
    try
      TCustomKeyExecutor(Context.Context).Execute(Context);
    finally
      Context.EditBuffer.TopView.Paint;
      Context.EditBuffer.TopView.Paint;
    end;
  end;
  BindingResult := krHandled;
end;

class procedure TFreedomBindKeyBoard.CreateExecutors;
begin
  if (not Assigned(FKeyExecutors)) then
  begin
    FKeyExecutors := TObjectList<TCustomKeyExecutor>.Create;
  end;
end;

class procedure TFreedomBindKeyBoard.DestroyExecutors;
begin
  FreeAndNil(FKeyExecutors);
end;

function TFreedomBindKeyBoard.GetBindingType: TBindingType;
begin
  Result := btPartial;
end;

function TFreedomBindKeyBoard.GetDisplayName: string;
begin
  Result := 'Freedom ORM';
end;

function TFreedomBindKeyBoard.GetName: string;
begin
  Result := 'FreedomORM';
end;

class function TFreedomBindKeyBoard.KeyExecutorList: TObjectList<TCustomKeyExecutor>;
begin
  Result := FKeyExecutors;
end;

class procedure TFreedomBindKeyBoard.RefreshKeyBindings;
begin
  RemoveKeyBindings;
  RegisterKeyBindings;
end;

class procedure TFreedomBindKeyBoard.RemoveKeyBindings;
begin
  if (FRegisteredIndex >= 0) then
  begin
    (BorlandIDEServices as IOTAKeyboardServices).RemoveKeyboardBinding(FRegisteredIndex);
  end;
end;

class procedure TFreedomBindKeyBoard.UnregisterKeyExecutor(pKeyExecutorClass: TKeyExecutorClass);
var
  lKeyExecutor: TCustomKeyExecutor;
  lHasKeyExecutorRemoved: Boolean;
begin
  lHasKeyExecutorRemoved := False;
  CreateExecutors;
  for lKeyExecutor in FKeyExecutors do
  begin
    if (lKeyExecutor.InheritsFrom(pKeyExecutorClass)) then
    begin
      FKeyExecutors.Remove(lKeyExecutor);
      lHasKeyExecutorRemoved := True;
      Break;
    end;
  end;
  if not lHasKeyExecutorRemoved then
  begin
    RefreshKeyBindings;
  end;
end;

class procedure TFreedomBindKeyBoard.RegisterKeyBindings;
begin
  RemoveKeyBindings;
  FRegisteredIndex := (BorlandIDEServices as IOTAKeyboardServices).AddKeyboardBinding(TFreedomBindKeyBoard.Create);
end;

class procedure TFreedomBindKeyBoard.RegisterKeyExecutor(pKeyExecutorClass: TKeyExecutorClass);
begin
  CreateExecutors;
  FKeyExecutors.Add(pKeyExecutorClass.Create);
end;

initialization

finalization
  TFreedomBindKeyBoard.RemoveKeyBindings;
  TFreedomBindKeyBoard.DestroyExecutors;

end.
