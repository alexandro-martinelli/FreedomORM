unit AM.Freedom.FindPropertyItem;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

type
  TFindPropertyItem = class
  private
    FPropertyNames: TStrings;
    FPropertyValue: Variant;
    FPropertyIndex: Integer;
    procedure SetPropertyNames(const Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    property PropertyNames: TStrings read FPropertyNames write SetPropertyNames;
    property PropertyIndex: Integer read FPropertyIndex write FPropertyIndex;
    property PropertyValue: Variant read FPropertyValue write FPropertyValue;
  end;

  TFindPropertyList = class(TObjectList<TFindPropertyItem>)
  public
    procedure AddItem(pPropertyNames: TStrings; pPropertyValue: Variant);
  end;

implementation

uses
  System.Variants;

{ TFindPropertyItem }

constructor TFindPropertyItem.Create;
begin
  FPropertyNames := TStringList.Create;
  FPropertyValue := Null;
end;

destructor TFindPropertyItem.Destroy;
begin
  FPropertyNames.Free;
  inherited;
end;

procedure TFindPropertyItem.SetPropertyNames(const Value: TStrings);
begin
  if (Assigned(FPropertyNames)) then
  begin
    FreeAndNil(FPropertyNames);
  end;
  FPropertyNames := Value;
end;

{ TFindPropertyList }

procedure TFindPropertyList.AddItem(pPropertyNames: TStrings; pPropertyValue: Variant);
var
  lItem: TFindPropertyItem;
begin
  lItem := TFindPropertyItem.Create;
  lItem.PropertyNames := pPropertyNames;
  lItem.PropertyValue := pPropertyValue;
  Add(lItem);
end;

end.