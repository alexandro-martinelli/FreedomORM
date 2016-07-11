unit AM.UnitReader.Members.AttributeMember;

interface

uses
  System.Generics.Collections,
  System.SysUtils;

type
  TArgumentValue = class sealed
  private
    FValue: String;
  public
    property Value: String read FValue write FValue;
  end;

  TArgumentValueList = class(TObjectList<TArgumentValue>);

  TAttributeMember = class sealed
  private
    FName: String;
    FArguments: TArgumentValueList;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: String read FName write FName;
    property Arguments: TArgumentValueList read FArguments write FArguments;
  end;

  TAttributeMemberList = class(TObjectList<TAttributeMember>);

implementation

{ TAttributeMember }

constructor TAttributeMember.Create;
begin
  FArguments := TArgumentValueList.Create;
end;

destructor TAttributeMember.Destroy;
begin
  FArguments.Free;
  inherited;
end;

end.