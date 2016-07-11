unit AM.UnitReader.Members.ClassMember;

interface

uses
  System.Generics.Collections,
  AM.UnitReader.Members.CustomMember,
  AM.UnitReader.Members.FieldMember,
  AM.UnitReader.Members.MethodMember,
  AM.UnitReader.Members.PropertyMember,
  AM.UnitReader.Enumerations,
  AM.UnitReader.Members.ConstantMember;

type
  TClassList = class;

  TClassMember = class(TCustomMember)
  strict private
    FProperties: TPropertyList;
    FFields: TFieldList;
    FMethods: TMethodList;
    FClasses: TClassList;
    FConstants: TConstantList;
    FParentName: String;
  public
    constructor Create(pName: string; pVisibility: TVisibilityScope = vsStrictPrivate); override;
    destructor Destroy; override;
    property ParentName: String read FParentName write FParentName;
    property Constants: TConstantList read FConstants write FConstants;
    property Fields: TFieldList read FFields write FFields;
    property Methods: TMethodList read FMethods write FMethods;
    property Properties: TPropertyList read FProperties write FProperties;
    property Classes: TClassList read FClasses write FClasses;

  end;

  TClassList = class(TCustomMemberList<TClassMember>)
  public
    function FindClass(pClassName: String): TClassMember;
  end;

implementation

uses
  System.SysUtils,
  System.Classes,
  System.StrUtils;
{ TEntityClassFactory }

constructor TClassMember.Create(pName: string; pVisibility: TVisibilityScope);
begin
  inherited Create(pName, pVisibility);
  FProperties := TPropertyList.Create(Self);
  FFields := TFieldList.Create(Self);
  FMethods := TMethodList.Create(Self);
  FClasses := TClassList.Create(Self);
  FConstants := TConstantList.Create(Self);
end;

destructor TClassMember.Destroy;
begin
  FreeAndNil(FProperties);
  FreeAndNil(FFields);
  FreeAndNil(FMethods);
  FreeAndNil(FClasses);
  FreeAndNil(FConstants);
  inherited;
end;

{ TClassList }

function TClassList.FindClass(pClassName: String): TClassMember;
var
  lClass: TClassMember;
  lStrings: TStrings;
begin
  Result := nil;
  lStrings := TStringList.Create;
  try
    for lClass in Self do
    begin
      lStrings.Clear;
      ExtractStrings(['.'], [' '], PWideChar(pClassName), lStrings);
      pClassName := lStrings.Strings[lStrings.Count - 1];
      if SameText(lClass.Name, pClassName) then
      begin
        Result := lClass;
        Break;
      end;
    end;
  finally
    lStrings.Free;
  end;
end;

end.
