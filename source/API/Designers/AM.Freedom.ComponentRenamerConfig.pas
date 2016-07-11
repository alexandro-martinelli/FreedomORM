unit AM.Freedom.ComponentRenamerConfig;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.IniFiles,
  AM.Freedom.CustomFreedomConfig;

type
  TComponentList = class(TComponent)
  strict private
    FComponents: TList<TComponent>;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ComponentList: TList<TComponent> read FComponents;
    procedure AddComponent(pComponent: TComponent);
    function Contains(pComponent: TComponent): Boolean;
    procedure RemoveComponent(pComponent: TComponent);
  end;

  TComponentRenamerConfig = class sealed(TCustomFreedomConfig)
  strict private const
    cIgnore = 'Ignored';
    cPrefix = 'Prefix';
  strict private
    class var FIni: TIniFile;
    class var FUnNamedComponents: TComponentList;
  private
    class procedure LoadIniFileConfiguration;
    class procedure DestroyIniFileConfiguration;
  public
    class function IniFileName: String;
    class procedure ReloadConfig;
    class procedure AddIgnoredClass(const pClassName: String);
    class procedure RemoveIgnoredClass(const pClassName: String);
    class procedure AddClassPrefix(const pClassName, pPrefix: String);
    class procedure RemoveClassPrefix(const pClassName: String);
    class function FindClassPrefix(const pClassName: String): String; overload;
    class function FindClassPrefix(const pClass: TClass): String; overload;
    class function IsIgnoredClass(const pClassName: String): Boolean; overload;
    class function IsIgnoredClass(const pClass: TClass): Boolean; overload;
    class function ConfigurePrefix(pClass: TClass): String;
    class procedure RemoveUnNamedComponent(pComponent: TComponent);
    class procedure AddUnNamedComponent(pComponent: TComponent);
    class function IsUnNamedComponent(pComponent: TComponent): Boolean;
  end;

implementation

uses
  ToolsAPI,
  DesignIntf,
  System.IOUtils,
  AM.Freedom.frmConfigurePrefix;

{ TComponentRenamerConfig }

class procedure TComponentRenamerConfig.AddClassPrefix(const pClassName, pPrefix: String);
begin
  FIni.WriteString(pClassName, cPrefix, pPrefix);
end;

class procedure TComponentRenamerConfig.AddIgnoredClass(const pClassName: String);
begin
  FIni.WriteBool(pClassName, cIgnore, True);
end;

class procedure TComponentRenamerConfig.AddUnNamedComponent(pComponent: TComponent);
begin
  if (not FUnNamedComponents.Contains(pComponent)) then
  begin
    FUnNamedComponents.AddComponent(pComponent);
  end;
end;

class function TComponentRenamerConfig.ConfigurePrefix(pClass: TClass): String;
begin
  Result := TfrmConfigurePrefix.ConfigurePrefix(pClass);
end;

class procedure TComponentRenamerConfig.DestroyIniFileConfiguration;
begin
  try
    FreeAndNil(FIni);
    FreeAndNil(FUnNamedComponents);
  except
  end;
end;

class function TComponentRenamerConfig.FindClassPrefix(const pClass: TClass): String;
var
  lClass: TClass;
  lClassName: string;
begin
  lClass := pClass;
  lClassName := lClass.ClassName;
  Result := '';
  while (Result = '') and (lClassName <> '') do
  begin
    Result := FindClassPrefix(lClassName);
    if (Result = '') then
    begin
      lClassName := '';
      lClass := lClass.ClassParent;
      if (Assigned(lClass)) then
      begin
        lClassName := lClass.ClassName;
      end;
    end;
  end;
end;

class function TComponentRenamerConfig.FindClassPrefix(const pClassName: String): String;
begin
  Result := FIni.ReadString(pClassName, cPrefix, '');
end;

class function TComponentRenamerConfig.IniFileName: String;
begin
  Result := GetIniFileDirectory + 'FreedomORMComponentRename.ini';
end;

class function TComponentRenamerConfig.IsIgnoredClass(const pClass: TClass): Boolean;
var
  lClass: TClass;
  lClassName: string;
begin
  lClass := pClass;
  lClassName := lClass.ClassName;
  Result := False;
  while not Result and (lClassName <> '') do
  begin
    Result := IsIgnoredClass(lClassName);
    if (not Result) then
    begin
      lClassName := '';
      lClass := lClass.ClassParent;
      if (Assigned(lClass)) then
      begin
        lClassName := lClass.ClassName;
      end;
    end;
  end;
end;

class function TComponentRenamerConfig.IsUnNamedComponent(pComponent: TComponent): Boolean;
begin
  Result := FUnNamedComponents.Contains(pComponent);
end;

class function TComponentRenamerConfig.IsIgnoredClass(const pClassName: String): Boolean;
begin
  Result := FIni.ReadBool(pClassName, cIgnore, False) or (pClassName = 'TCustomForm') or
      (pClassName = 'TField') or (pClassName = 'TCustomFrame') or (pClassName = 'TDataModule');
end;

class procedure TComponentRenamerConfig.LoadIniFileConfiguration;
begin
  if (not Assigned(FIni)) then
  begin
    FIni := TIniFile.Create(IniFileName);
  end;
  if (not Assigned(FUnNamedComponents)) then
  begin
    FUnNamedComponents := TComponentList.Create(nil);
  end;
end;

class procedure TComponentRenamerConfig.ReloadConfig;
begin
  FreeAndnil(FIni);
  LoadIniFileConfiguration;
end;

class procedure TComponentRenamerConfig.RemoveClassPrefix(const pClassName: String);
begin
  FIni.WriteString(pClassName, cPrefix, '');
end;

class procedure TComponentRenamerConfig.RemoveIgnoredClass(const pClassName: String);
begin
  FIni.WriteBool(pClassName, cIgnore, False);
end;

class procedure TComponentRenamerConfig.RemoveUnNamedComponent(pComponent: TComponent);
var
  lIndex: Integer;
begin
  if (FUnNamedComponents.Contains(pComponent)) then
  begin
    FUnNamedComponents.RemoveComponent(pComponent);
  end;
  for lIndex := 0 to pComponent.ComponentCount - 1 do
  begin
    RemoveUnNamedComponent(pComponent.Components[lIndex]);
  end;
end;

{ TComponentList }

procedure TComponentList.AddComponent(pComponent: TComponent);
begin
  if (not FComponents.Contains(pComponent)) then
  begin
    FComponents.Add(pComponent);
    pComponent.FreeNotification(Self);
  end;
end;

function TComponentList.Contains(pComponent: TComponent): Boolean;
begin
  Result := FComponents.Contains(pComponent);
end;

constructor TComponentList.Create(AOwner: TComponent);
begin
  inherited;
  FComponents := TList<TComponent>.Create;
end;

destructor TComponentList.Destroy;
begin
  FComponents.Free;
  inherited;
end;

procedure TComponentList.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    FComponents.Remove(AComponent);
  end;
end;

procedure TComponentList.RemoveComponent(pComponent: TComponent);
begin
  if (FComponents.Contains(pComponent)) then
  begin
    FComponents.Remove(pComponent);
    pComponent.RemoveFreeNotification(Self);
  end;
end;

initialization
  TComponentRenamerConfig.LoadIniFileConfiguration;

finalization
  TComponentRenamerConfig.DestroyIniFileConfiguration;

end.