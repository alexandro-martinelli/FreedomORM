unit AM.UnitReader.Members.CustomMember;

interface

uses
  System.Generics.Defaults,
  System.Generics.Collections,
  AM.UnitReader.Enumerations,
  AM.UnitReader.Members.AttributeMember;

type
  TMemberClass = class of TCustomMember;

  TCustomMember = class abstract
  strict private
    FName: String;
    FVisibility: TVisibilityScope;
    FOwner: TCustomMember;
    FAttributes: TAttributeMemberList;
    FUnitClassName: String;
  public
    constructor Create(pName: String; pVisibility: TVisibilityScope = vsStrictPrivate); virtual;
    destructor Destroy; override;
    property Name: String read FName write FName;
    property UnitClassName: String read FUnitClassName write FUnitClassName;
    property Visibility: TVisibilityScope read FVisibility write FVisibility default vsStrictPrivate;
    property Owner: TCustomMember read FOwner write FOwner;
    property Attributes: TAttributeMemberList read FAttributes;
  end;

  TCustomMemberList<T: TCustomMember, constructor> = class abstract(TObjectList<T>)
  strict private type
    TMemberListComparer = class(TComparer<T>)
    public
      function Compare(const Left, Right: T): Integer; override;
    end;
  strict private
    FOwner: TCustomMember;
  protected
    procedure Notify(const Value: T; Action: TCollectionNotification); override;
  public
    constructor Create(pOwner: TCustomMember); reintroduce;
    function MemberByName(pName: String): T;
  end;

implementation

uses
  System.SysUtils;

{ TCustomMember }

constructor TCustomMember.Create(pName: String; pVisibility: TVisibilityScope);
begin
  FName := pName;
  FVisibility := pVisibility;
  FAttributes := TAttributeMemberList.Create;
end;

destructor TCustomMember.Destroy;
begin
  FAttributes.Free;
  inherited;
end;

{ TCustomMemberList<T> }

constructor TCustomMemberList<T>.Create(pOwner: TCustomMember);
begin
  inherited Create(TMemberListComparer.Create, True);
  FOwner := pOwner;
end;

function TCustomMemberList<T>.MemberByName(pName: String): T;
var
  lMember: T;
begin
  Result := nil;
  for lMember in Self do
  begin
    if SameText(lMember.Name, pName) then
    begin
      Result := lMember;
      Break
    end;
  end;
end;

procedure TCustomMemberList<T>.Notify(const Value: T; Action: TCollectionNotification);
begin
  inherited;
  if Action = cnAdded then
  begin
    Value.Owner := FOwner;
  end else
  begin
    Value.Owner := nil;
  end;
end;

{ TCustomMemberList<T>.TMemberListComparer }

function TCustomMemberList<T>.TMemberListComparer.Compare(const Left,
  Right: T): Integer;
begin
  Result := CompareText(Left.Name, Right.Name);
end;

end.
