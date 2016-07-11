unit AM.Freedom.MasterMethodControl;

interface

uses
  System.Rtti,
  System.Generics.Collections,
  AM.Freedom.ObjectMapper.MethodMapper;

type
  TMasterMethodControl = class sealed
  strict private type
    TVirtualMethod = class
    strict private
      FMethods: TMethodMapperList;
      FVMI: TVirtualMethodInterceptor;
      FMetaClass: TClass;
      procedure AssignMethodsForVMI;
      procedure AssignBeforeVMIMethod;
      procedure AssignAfterVMIMethod;
      procedure AssignOnErrorVMIMethod;
    public
      constructor Create(pMetaClass: TClass; pMethods: TMethodMapperList);
      destructor Destroy; override;
      property MetaClass: TClass read FMetaClass;
      property Methods: TMethodMapperList read FMethods;
      property VMI: TVirtualMethodInterceptor read FVMI;
    end;
    TVirtualMethods = class(TObjectList<TVirtualMethod>)
    public
      function ItemByClass(pMetaClass: TClass): TVirtualMethod;
    end;
  private
    class var FMasterList: TVirtualMethods;
    class procedure CreateMasterList;
    class procedure DestroyMasterList;
  public
    class procedure AddControler(pMetaClass: TClass; pMapperList: TMethodMapperList);
    class procedure RegisterObject(pObject: TObject);
    class procedure AddAndRegister(pObject: TObject; pMapperList: TMethodMapperList);
    class procedure UnregisterObject(pObject: TObject);
  end;

implementation

uses
  System.SysUtils, AM.Freedom.ObjectMapper.CustomMethodControl,
  AM.Freedom.EnumerationTypes;

{ TMasterMethodControl }

class procedure TMasterMethodControl.AddAndRegister(pObject: TObject;
  pMapperList: TMethodMapperList);
begin
  AddControler(pObject.ClassType, pMapperList);
  RegisterObject(pObject);
end;

class procedure TMasterMethodControl.AddControler(pMetaClass: TClass; pMapperList: TMethodMapperList);
begin
  if (FMasterList.ItemByClass(pMetaClass) = nil) and Assigned(pMapperList) then
  begin
    FMasterList.Add(TVirtualMethod.Create(pMetaClass, pMapperList));
  end else begin
    FReeAndNil(pMapperList);
  end;
end;

class procedure TMasterMethodControl.DestroyMasterList;
begin
  FreeAndNil(FMasterList);
end;

class procedure TMasterMethodControl.CreateMasterList;
begin
  FMasterList := TVirtualMethods.Create;
end;

class procedure TMasterMethodControl.RegisterObject(pObject: TObject);
begin
  if (FMasterList.ItemByClass(pObject.ClassType) <> nil) and Assigned(pObject) then
  begin
    FMasterList.ItemByClass(pObject.ClassType).VMI.Proxify(pObject);
  end;
end;

class procedure TMasterMethodControl.UnregisterObject(pObject: TObject);
begin
  if (FMasterList.ItemByClass(pObject.ClassType) <> nil) and Assigned(pObject) then
  begin
    FMasterList.ItemByClass(pObject.ClassType).VMI.Unproxify(pObject);
  end;
end;

{ TMasterMethodControl.TClassVirtualMethods }

procedure TMasterMethodControl.TVirtualMethod.AssignAfterVMIMethod;
begin
  FVMI.OnAfter :=
    procedure(Instance: TObject;
      Method: TRttiMethod; const Args: TArray<TValue>; var Result: TValue)
    var
      AMethod: TMethodMapper;
      AObject: TCustomMethodControl;
    begin
      for AMethod in FMethods do
      begin
        if (TMethodOption.After in AMethod.Options) and (AMethod.Name = Method.Name) then
        begin
          AObject := AMethod.MethodControlClass.Create;
          try
            AObject.AfterExecute(Instance);
          finally
            FreeAndNil(AObject);
          end;
        end;
      end;
    end;
end;

procedure TMasterMethodControl.TVirtualMethod.AssignBeforeVMIMethod;
begin
  FVMI.OnBefore :=
    procedure(Instance: TObject;
      Method: TRttiMethod; const Args: TArray<TValue>; out DoInvoke: Boolean;
      out Result: TValue)
    var
      AMethod: TMethodMapper;
      AObject: TCustomMethodControl;
    begin
      for AMethod in FMethods do
      begin
        if (TMethodOption.Before in AMethod.Options) and (AMethod.Name = Method.Name) then
        begin
          AObject := AMethod.MethodControlClass.Create;
          try
            AObject.BeforeExecute(Instance);
          finally
            FreeAndNil(AObject);
          end;
        end;
      end;
    end;
end;

procedure TMasterMethodControl.TVirtualMethod.AssignMethodsForVMI;
begin
  AssignBeforeVMIMethod;
  AssignAfterVMIMethod;
  AssignOnErrorVMIMethod;
end;

procedure TMasterMethodControl.TVirtualMethod.AssignOnErrorVMIMethod;
begin
  FVMI.OnException :=
    procedure(Instance: TObject;
      Method: TRttiMethod; const Args: TArray<TValue>; out RaiseException: Boolean;
      TheException: Exception; out Result: TValue)
    var
      AMethod: TMethodMapper;
      AObject: TCustomMethodControl;
    begin
      for AMethod in FMethods do
      begin
        if (TMethodOption.OnError in AMethod.Options) and (AMethod.Name = Method.Name) then
        begin
          AObject := AMethod.MethodControlClass.Create;
          try
            AObject.OnExecuteError(Instance, TheException);
          finally
            FreeAndNil(AObject);
          end;
        end;
      end;
    end;
end;

constructor TMasterMethodControl.TVirtualMethod.Create(pMetaClass: TClass;
    pMethods: TMethodMapperList);
begin
  FMetaClass := pMetaClass;
  FMethods := pMethods;
  FVMI := TVirtualMethodInterceptor.Create(pMetaClass);
  AssignMethodsForVMI;
end;

{ TMasterMethodControl.TVirtualMethods }

function TMasterMethodControl.TVirtualMethods.ItemByClass(
  pMetaClass: TClass): TVirtualMethod;
var
  AMethod: TVirtualMethod;
begin
  Result := nil;
  for AMethod in Self do
  begin
    if AMethod.MetaClass = pMetaClass then
    begin
      Result := AMethod;
      Break;
    end;
  end;
end;

destructor TMasterMethodControl.TVirtualMethod.Destroy;
begin
  FreeAndNil(FMethods);
  FreeAndNil(FVMI);
  inherited;
end;

initialization
  TMasterMethodControl.CreateMasterList;

finalization
  TMasterMethodControl.DestroyMasterList;

end.
