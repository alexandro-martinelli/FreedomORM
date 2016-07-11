unit AM.UnitReader.Members.MethodMember;

interface

uses
  AM.UnitReader.Members.CustomMember,
  AM.UnitReader.Members.ArgumentMember,
  AM.UnitReader.Enumerations;

type
  TMethodMember = class(TCustomMember)
  private
    FArguments: TArgumentList;
    FSourceCode: String;
    FMethodType: TMethodType;
    FIsClassMethod: Boolean;
  public
    constructor Create(pName: string; pVisibility: TVisibilityScope = vsStrictPrivate); override;
    destructor Destroy; override;
    property Arguments: TArgumentList read FArguments;
    property MethodType: TMethodType read FMethodType write FMethodType;
    property IsClassMethod: Boolean read FIsClassMethod write FIsClassMethod;
    property SourceCode: String read FSourceCode write FSourceCode;
  end;

  TMethodList = class(TCustomMemberList<TMethodMember>);

implementation

uses
  System.SysUtils,
  System.StrUtils;

{ TMethodMember }

constructor TMethodMember.Create(pName: string; pVisibility: TVisibilityScope = vsStrictPrivate);
begin
  inherited Create(pName, pVisibility);
  FArguments := TArgumentList.Create;
  FMethodType := mtProcedure;
  FIsClassMethod := False;
end;

destructor TMethodMember.Destroy;
begin
  FreeAndNil(FArguments);
  inherited;
end;

end.
