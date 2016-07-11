unit AM.Freedom.FileSearchWriter;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

type
  TFileSearch = class(TThread)
  private
    FThreadDirectorys: TList<String>;
    FListOfUnits: TList<String>;
    FAllDirectorysListed: Boolean;
    procedure ListInDirectory(aDirectory: string);
    function RemoveExtension(aFileName: string): string;
  protected
    procedure Execute; override;
  public
    constructor Create(aThreadDirectorys: TList<String>); reintroduce;
    destructor Destroy; override;
    property AllDirectorysListed: Boolean read FAllDirectorysListed;
    property Units: TList<String> read FListOfUnits;
  end;

  TFileSearchWriter = class sealed
  strict private
    class var FFileSearchWriter: TFileSearchWriter;
  private
    FThreads: TObjectList<TFileSearch>;
    FUnits: TList<TList<String>>;
    FDirectorys: TList<String>;
    FThreadDirectorys: TList<String>;
    function AddDirectoryToList(aDirectory: String): Boolean;
    procedure AddThreadDirectory(aDirectory: String);
    procedure CreateThread;
    procedure InternalStartAllThreads;
    procedure InternalAddDirectory(aDirectory: String);
    procedure RetrevieAllFinishedThreads;
    class procedure DestroyFileSearchWriter;
  protected
    constructor Create; virtual; final;
  public
    class procedure ClearThreadList;
    class procedure AddDirectory(aDirectory: String);
    class procedure StartAllThreads;
    class function ThreadsRunningCount: Integer;
    class function ThreadsCount: Integer;
    class function RetrieveUnits: TList<TList<String>>;
  end;

implementation

uses
  System.StrUtils;

{ TFileSearchWriter }

class procedure TFileSearchWriter.AddDirectory(aDirectory: String);
begin
  if (not Assigned(FFileSearchWriter)) then
  begin
    FFileSearchWriter := TFileSearchWriter.Create;
  end;
  FFileSearchWriter.InternalAddDirectory(aDirectory);
end;

function TFileSearchWriter.AddDirectoryToList(aDirectory: String): Boolean;
begin
  Result := not FDirectorys.Contains(aDirectory);
  if (Result) then
  begin
    FDirectorys.Add(aDirectory);
  end;
end;

procedure TFileSearchWriter.AddThreadDirectory(aDirectory: String);
begin
  FThreadDirectorys.Add(aDirectory);
  CreateThread;
end;

class procedure TFileSearchWriter.ClearThreadList;
begin
  if (not Assigned(FFileSearchWriter)) then
  begin
    FFileSearchWriter := TFileSearchWriter.Create;
  end;
  FFileSearchWriter.FThreads.Clear;
end;

constructor TFileSearchWriter.Create;
begin
  FThreads := TObjectList<TFileSearch>.Create;
  FUnits := TList<TList<String>>.Create;
  FDirectorys := TList<String>.Create;
  FThreadDirectorys := TList<String>.Create;
end;

procedure TFileSearchWriter.CreateThread;
var
  lThread: TFileSearch;
begin
  if (FThreadDirectorys.Count >= 120) then
  begin
    lThread := TFileSearch.Create(FThreadDirectorys);
    FThreads.Add(lThread);
    lThread.Start;
    FThreadDirectorys := TList<String>.Create;
  end;
end;

class procedure TFileSearchWriter.DestroyFileSearchWriter;
begin
  FreeAndNil(FFileSearchWriter);
end;

procedure TFileSearchWriter.InternalAddDirectory(aDirectory: String);
begin
  if AddDirectoryToList(aDirectory) then
  begin
    AddThreadDirectory(aDirectory);
  end;
end;

procedure TFileSearchWriter.RetrevieAllFinishedThreads;
var
  lThread: TFileSearch;
  lIndex: Integer;
begin
  lIndex := 0;
  while lIndex <= FThreads.Count -1 do
  begin
    lThread := FThreads.Items[lIndex];
    FUnits.Add(lThread.Units);
    Inc(lIndex);
  end;
end;

class function TFileSearchWriter.RetrieveUnits: TList<TList<String>>;
begin
  Result := nil;
  if (Assigned(FFileSearchWriter)) then
  begin
    if FFileSearchWriter.ThreadsRunningCount = 0 then
    begin
      FFileSearchWriter.RetrevieAllFinishedThreads;
      Result := FFileSearchWriter.FUnits;
    end;
  end;
end;

class procedure TFileSearchWriter.StartAllThreads;
begin
  FFileSearchWriter.InternalStartAllThreads;
end;

procedure TFileSearchWriter.InternalStartAllThreads;
var
  lThread: TFileSearch;
begin
  if (FThreadDirectorys.Count > 0) then
  begin
    lThread := TFileSearch.Create(FThreadDirectorys);
    FThreads.Add(lThread);
    lThread.Start;
    FThreadDirectorys := TList<String>.Create;
  end;
end;

class function TFileSearchWriter.ThreadsCount: Integer;
begin
  Result := FFileSearchWriter.FThreads.Count;
end;

class function TFileSearchWriter.ThreadsRunningCount: Integer;
var
  lThread: TFileSearch;
begin
  Result := 0;
  for lThread in FFileSearchWriter.FThreads do
  begin
    if (not lThread.AllDirectorysListed) then
    begin
      Inc(Result);
    end;
  end;
end;

{ TFileSearch }

constructor TFileSearch.Create(aThreadDirectorys: TList<String>);
begin
  inherited Create(True);
  FThreadDirectorys := aThreadDirectorys;
  Priority := tpHighest;
  FListOfUnits := TList<String>.Create;
  FreeOnTerminate := False;
  FAllDirectorysListed := False;
end;

destructor TFileSearch.Destroy;
begin
  FThreadDirectorys.Free;
  inherited;
end;

procedure TFileSearch.Execute;
var
  lDirectory: String;
begin
  inherited;
  for lDirectory in FThreadDirectorys do
  begin
    ListInDirectory(lDirectory + '*.pas');
    ListInDirectory(lDirectory + '*.dcu');
  end;
  FAllDirectorysListed := True;
end;

procedure TFileSearch.ListInDirectory(aDirectory: string);
var
  lFileName: string;
  lSearcher: TSearchRec;
  lFind: Integer;
begin
  lFind := FindFirst(aDirectory, faAnyFile, lSearcher);
  if lFind = 0 then
  begin
    while (lFind = 0) do
    begin
      if ((lSearcher.Name + ' ')[1] = '.') then
      begin
        lFind := FindNext(lSearcher);
        Continue;
      end;
      lFileName := RemoveExtension(lSearcher.Name);
      if (not FListOfUnits.Contains(lFileName)) then
      begin
        FListOfUnits.Add(lFileName);
      end;
      lFind := FindNext(lSearcher);
    end;
    FindClose(lSearcher);
  end;
end;

function TFileSearch.RemoveExtension(aFileName: string): string;
var
  lStrings: TStrings;
  lIndex: Integer;
begin
  lStrings := TStringList.Create;
  try
    ExtractStrings(['.'], [' '], PWideChar(aFileName), lStrings);
    Result := '';
    for lIndex := 0 to lStrings.Count - 2 do
    begin
      Result := Result + ifthen(Result <> '', '.') + lStrings.Strings[lIndex];
    end;
  finally
    lStrings.Free;
  end;
end;

initialization

finalization
  TFileSearchWriter.DestroyFileSearchWriter;

end.
