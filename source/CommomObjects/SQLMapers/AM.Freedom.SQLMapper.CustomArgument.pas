unit AM.Freedom.SQLMapper.CustomArgument;

interface

uses
  System.Generics.Collections,
  AM.Freedom.SQLMappers.AliasableObject,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.ArgumentDesignerRegister;

type
  TArgumentClass = class of TCustomArgument;

  TCustomArgument = class abstract(TAliasableObject);

  TCustomCommand = class;

  TCommandList = class(TList<TCustomCommand>)
  private
    procedure FreeNotification(pCommand: TCustomCommand);
  protected
    procedure Notify(const Item: TCustomCommand; Action: TCollectionNotification); override;
  end;

  TCustomCommand = class abstract(TCustomArgument)
  strict private
    FFreeNotifiers: TList<TCommandList>;
    FSchema: String;
  strict protected
    function GetCommandType: TCommandType; virtual; abstract;
  protected
    procedure AddFreeNotifier(pList: TCommandList);
  public
    constructor Create(pSchemaName: String = ''); virtual;
    destructor Destroy; override;
    property CommandType: TCommandType read GetCommandType;
    property Alias;
    property Schema: String read FSchema write FSchema;
  end;


  TSimpleArgument = class(TCustomArgument)
  protected
    class function AgumentDesigner: TArgumentDesigner; virtual;
  end;

  TComplexArgument = class(TSimpleArgument);

implementation

{ TCustomCommand }

procedure TCustomCommand.AddFreeNotifier(pList: TCommandList);
begin
  FFreeNotifiers.Add(pList);
end;

constructor TCustomCommand.Create(pSchemaName: String);
begin
  FFreeNotifiers := TList<TCommandList>.Create;
  FSchema := pSchemaName;
end;

destructor TCustomCommand.Destroy;
begin
  while FFreeNotifiers.Count > 0 do
  begin
    FFreeNotifiers.Items[0].FreeNotification(Self);
    FFreeNotifiers.Extract(FFreeNotifiers.Items[0]);
  end;
  FFreeNotifiers.Free;
  inherited;
end;

{ TCommandList }

procedure TCommandList.FreeNotification(pCommand: TCustomCommand);
begin
  Extract(pCommand);
end;

procedure TCommandList.Notify(const Item: TCustomCommand; Action: TCollectionNotification);
begin
  if Action = cnAdded then
  begin
    Item.AddFreeNotifier(Self);
  end;
  inherited;
end;

{ TSimpleArgument }

class function TSimpleArgument.AgumentDesigner: TArgumentDesigner;
begin
  Result := nil;
end;

end.
