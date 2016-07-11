unit AM.Freedom.dclFreedomORMConfig;

interface

uses
  System.SysUtils,
  System.IniFiles,
  System.IOUtils,
  AM.Freedom.CustomFreedomConfig,
  System.Classes;

type
  TdclFreedomORMConfig = class sealed(TCustomFreedomConfig)
  strict private const
    cSection = 'FreedomORM';
    cRename = 'ActiveRename';
    cShortCuts = 'Shortcuts';
    cNewCriteria = 'NewCriteria';
    cNewGroupCriteria = 'NewGroupCriteria';
    cNewFreedomObject = 'NewFreedomObject';
    cNewFreedomDBObject = 'NewFreedomDBObject';
    cInverteSentence = 'InverteSentence';
    cMoveBlockUp = 'MoveBlockUp';
    cMoveBlockDown = 'MoveBlockDown';
    cDeclareVariable = 'DeclareVariable';
    cExtractVariable = 'ExtractVariable';
    cExtractConstant = 'ExtractConstant';
    cExtractMethod = 'ExtractMethod';
    cDeclareMethod = 'DeclareMethod';
    cFindMethod = 'FindMethod';
    cViewRefactorings = 'ViewRefactorings';
    cDeclareUnit = 'DeclareUnit';
    cNewUnit = 'NewUnit';
    cUseUnit = 'UseUnit';
    cFreedomORMOptions = 'FreedomORMOptions';
    cEmptyShortCut = 0;
  strict private
    class var FInstance: TdclFreedomORMConfig;
  strict private
    FIni: TIniFile;
    function GetActiveRename: Boolean;
    procedure SetActiveRename(const Value: Boolean);
    function GetNewFreedomObject: TShortCut;
    function GetNewCriteria: TShortCut;
    function GetNewFreedomDBObject: TShortCut;
    function GetNewGroupCriteria: TShortCut;
    procedure SetNewFreedomObject(const Value: TShortCut);
    procedure SetNewCriteria(const Value: TShortCut);
    procedure SetNewFreedomDBObject(const Value: TShortCut);
    procedure SetNewGroupCriteria(const Value: TShortCut);
    constructor Create;
    function GetReverseSentence: TShortCut;
    procedure SetReverseSentence(const Value: TShortCut);
    function GetMoveBlockUp: TShortCut;
    procedure SetMoveBlockUp(const Value: TShortCut);
    function GetMoveBlockDown: TShortCut;
    procedure SetMoveBlockDown(const Value: TShortCut);
    function GetDeclareVariable: TShortCut;
    procedure SetDeclareVariable(const Value: TShortCut);
    function GetExtractVariable: TShortCut;
    procedure SetExtractVariable(const Value: TShortCut);
    function GetExtractConstant: TShortCut;
    procedure SetExtractConstant(const Value: TShortCut);
    function GetFindMethod: TShortCut;
    procedure SetFindMethod(const Value: TShortCut);
    function GetExtractMethod: TShortCut;
    procedure SetExtractMethod(const Value: TShortCut);
    function GetDeclareMethod: TShortCut;
    procedure SetDeclareMethod(const Value: TShortCut);
    function GetViewRefactorings: TShortCut;
    procedure SetViewRefactorings(const Value: TShortCut);
    function GetDeclareUnit: TShortCut;
    procedure SetDeclareUnit(const Value: TShortCut);
    function GetNewUnit: TShortCut;
    procedure SetNewUnit(const Value: TShortCut);
    function GetUseUnit: TShortCut;
    procedure SetUseUnit(const Value: TShortCut);
    function GetFreedomORMOptions: TShortCut;
    procedure SetFreedomORMOptions(const Value: TShortCut);
  public
    destructor Destroy; override;
    function IniFileDirectory: String;
    property ActiveRename: Boolean read GetActiveRename write SetActiveRename;
    property NewFreedomObject: TShortCut read GetNewFreedomObject write SetNewFreedomObject;
    property NewFreedomDBObject: TShortCut read GetNewFreedomDBObject write SetNewFreedomDBObject;
    property NewGroupCriteria: TShortCut read GetNewGroupCriteria write SetNewGroupCriteria;
    property NewCriteria: TShortCut read GetNewCriteria write SetNewCriteria;
    property ReverseSentence: TShortCut read GetReverseSentence write SetReverseSentence;
    property MoveBlockUp: TShortCut read GetMoveBlockUp write SetMoveBlockUp;
    property MoveBlockDown: TShortCut read GetMoveBlockDown write SetMoveBlockDown;
    property DeclareVariable: TShortCut read GetDeclareVariable write SetDeclareVariable;
    property ExtractVariable: TShortCut read GetExtractVariable write SetExtractVariable;
    property ExtractConstant: TShortCut read GetExtractConstant write SetExtractConstant;
    property ExtractMethod: TShortCut read GetExtractMethod write SetExtractMethod;
    property DeclareMethod: TShortCut read GetDeclareMethod write SetDeclareMethod;
    property FindMethod: TShortCut read GetFindMethod write SetFindMethod;
    property ViewRefactorings: TShortCut read GetViewRefactorings write SetViewRefactorings;
    property DeclareUnit: TShortCut read GetDeclareUnit write SetDeclareUnit;
    property NewUnit: TShortCut read GetNewUnit write SetNewUnit;
    property UseUnit: TShortCut read GetUseUnit write SetUseUnit;
    property FreedomORMOptions: TShortCut read GetFreedomORMOptions write SetFreedomORMOptions;

    class function GetInstance: TdclFreedomORMConfig;
    class procedure DestroyInstance;
  end;


implementation

{ TdclFreedomORMConfig }

constructor TdclFreedomORMConfig.Create;
begin
  FIni := TIniFile.Create(IniFileDirectory + 'dclFreedomORM.ini');
end;

destructor TdclFreedomORMConfig.Destroy;
begin
  FreeAndNil(FIni);
  inherited;
end;

class procedure TdclFreedomORMConfig.DestroyInstance;
begin
  FreeAndNil(FInstance);
end;

function TdclFreedomORMConfig.GetActiveRename: Boolean;
begin
  Result := FIni.ReadBool(cSection, cRename, False);
end;

function TdclFreedomORMConfig.GetDeclareMethod: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cDeclareMethod, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetDeclareUnit: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cDeclareUnit, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetDeclareVariable: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cDeclareVariable, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetExtractConstant: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cExtractConstant, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetExtractMethod: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cExtractMethod, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetExtractVariable: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cExtractVariable, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetFindMethod: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cFindMethod, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetFreedomORMOptions: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cFreedomORMOptions, cEmptyShortCut);
end;

class function TdclFreedomORMConfig.GetInstance: TdclFreedomORMConfig;
begin
  if (not Assigned(FInstance)) then
  begin
    FInstance := TdclFreedomORMConfig.Create;
  end;
  Result := FInstance;
end;

function TdclFreedomORMConfig.GetMoveBlockDown: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cMoveBlockDown, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetMoveBlockUp: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cMoveBlockUp, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetReverseSentence: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cInverteSentence, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetUseUnit: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cUseUnit, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetViewRefactorings: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cViewRefactorings, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetNewCriteria: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cNewCriteria, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetNewFreedomDBObject: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cNewFreedomDBObject, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetNewFreedomObject: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cNewFreedomObject, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetNewGroupCriteria: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cNewGroupCriteria, cEmptyShortCut);
end;

function TdclFreedomORMConfig.GetNewUnit: TShortCut;
begin
  Result := FIni.ReadInteger(cShortCuts, cNewUnit, cEmptyShortCut);
end;

function TdclFreedomORMConfig.IniFileDirectory: String;
begin
  Result := GetIniFileDirectory;
end;

procedure TdclFreedomORMConfig.SetActiveRename(const Value: Boolean);
begin
  FIni.WriteBool(cSection, cRename, Value);
end;

procedure TdclFreedomORMConfig.SetDeclareMethod(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cDeclareMethod, Value);
end;

procedure TdclFreedomORMConfig.SetDeclareUnit(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cDeclareUnit, Value);
end;

procedure TdclFreedomORMConfig.SetDeclareVariable(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cDeclareVariable, Value);
end;

procedure TdclFreedomORMConfig.SetExtractConstant(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cExtractConstant, Value);
end;

procedure TdclFreedomORMConfig.SetExtractMethod(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cExtractMethod, Value);
end;

procedure TdclFreedomORMConfig.SetExtractVariable(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cExtractVariable, Value);
end;

procedure TdclFreedomORMConfig.SetFindMethod(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cFindMethod, Value);
end;

procedure TdclFreedomORMConfig.SetFreedomORMOptions(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cFreedomORMOptions, Value);
end;

procedure TdclFreedomORMConfig.SetMoveBlockDown(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cMoveBlockDown, Value);
end;

procedure TdclFreedomORMConfig.SetMoveBlockUp(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cMoveBlockUp, Value);
end;

procedure TdclFreedomORMConfig.SetReverseSentence(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cInverteSentence, Value);
end;

procedure TdclFreedomORMConfig.SetUseUnit(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cUseUnit, Value);
end;

procedure TdclFreedomORMConfig.SetViewRefactorings(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cViewRefactorings, Value);
end;

procedure TdclFreedomORMConfig.SetNewCriteria(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cNewCriteria, Value);
end;

procedure TdclFreedomORMConfig.SetNewFreedomDBObject(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cNewFreedomDBObject, Value);
end;

procedure TdclFreedomORMConfig.SetNewFreedomObject(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cNewFreedomObject, Value);
end;

procedure TdclFreedomORMConfig.SetNewGroupCriteria(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cNewGroupCriteria, Value);
end;

procedure TdclFreedomORMConfig.SetNewUnit(const Value: TShortCut);
begin
  FIni.WriteInteger(cShortCuts, cNewUnit, Value);
end;

initialization

finalization
  TdclFreedomORMConfig.DestroyInstance;

end.
