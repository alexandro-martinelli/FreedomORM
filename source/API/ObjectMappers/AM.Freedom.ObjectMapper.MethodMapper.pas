unit AM.Freedom.ObjectMapper.MethodMapper;

interface

uses
  System.Generics.Collections,
  AM.Freedom.SQLMappers.NamedObject,
  AM.Freedom.ObjectMapper.CustomMethodControl,
  AM.Freedom.EnumerationTypes;

type
  TMethodMapper = class(TNamedObject)
  private
    FMethodControlClass: TMethodControlClass;
    FOptions: TMethodOptions;
  public
    constructor Create(pName: String; pMethodControlClass: TMethodControlClass; pOptions: TMethodOptions); overload;
    function CreateNew: TMethodMapper;
    procedure Assign(pSource: TMethodMapper);
    property Name;
    property MethodControlClass: TMethodControlClass read FMethodControlClass write FMethodControlClass;
    property Options: TMethodOptions read FOptions write FOptions;
  end;

  TMethodMapperList = class(TObjectList<TMethodMapper>)
  public
    procedure Assign(pSource: TMethodMapperList);
    function CreateNew: TMethodMapperList;
  end;

implementation

{ TMethodMapper }

procedure TMethodMapper.Assign(pSource: TMethodMapper);
begin
  FMethodControlClass := pSource.MethodControlClass;
  FOptions   := pSource.Options;
  Name := pSource.Name;
end;

constructor TMethodMapper.Create(pName: String; pMethodControlClass: TMethodControlClass;
    pOptions: TMethodOptions);
begin
  Name := pName;
  FMethodControlClass := pMethodControlClass;
  FOptions   := pOptions;
end;

function TMethodMapper.CreateNew: TMethodMapper;
begin
  Result := TMethodMapper.Create;
  Result.Assign(Self);
end;

{ TMethodMapperList }

procedure TMethodMapperList.Assign(pSource: TMethodMapperList);
var
  AMethod: TMethodMapper;
begin
  Clear;
  for AMethod in pSource do
  begin
    Add(AMethod.CreateNew);
  end;
end;

function TMethodMapperList.CreateNew: TMethodMapperList;
begin
  Result := TMethodMapperList.Create;
  Result.Assign(Self);
end;

end.
