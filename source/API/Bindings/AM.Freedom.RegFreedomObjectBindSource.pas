unit AM.Freedom.RegFreedomObjectBindSource;

interface

uses
  System.Classes,
  ToolsAPI,
  DesignEditors,
  DesignIntf,
  AM.UnitReader.Members.ClassMember,
  AM.Freedom.ClassesFinder;

type
  TFreedomObjectBindSourcePropertyEditor = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

procedure Register;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  AM.Freedom.FreedomObjectBindSource,
  AM.UnitReader.Members.UnitMember,
  AM.UnitReader.Enumerations;

procedure Register;
begin
  RegisterComponents('LiveBindings Misc', [TFreedomObjectBindSource]);
  RegisterPropertyEditor(TypeInfo(String), TFreedomObjectBindSource, 'ObjectClassName', TFreedomObjectBindSourcePropertyEditor);
end;


{ TFreedomObjectBindSourcePropertyEditor }

function TFreedomObjectBindSourcePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TFreedomObjectBindSourcePropertyEditor.GetValues(Proc: TGetStrProc);
var
  lClasses: TClassList;
  lClass: TClassMember;
begin
  lClasses := TClassesFinder.GetClasses;
  if (Assigned(lClasses)) then
  begin
    for lClass in lClasses do
    begin
      if (lClass.Properties.Count > 0) and (lClass.ParentName <> 'TForm') and (lClass.ParentName <> 'TDataModule') then
      begin
        Proc(lClass.Name);
      end;
    end;
  end;
end;

procedure TFreedomObjectBindSourcePropertyEditor.SetValue(const Value: string);
var
  lClasses: TClassList;
  lClass: TClassMember;
begin
  inherited;
  lClasses := TClassesFinder.GetClasses;
  try
    lClass := lClasses.FindClass(Value);
    (GetComponent(0) as TFreedomObjectBindSource).GenerateFieldsWithClassMember(lClass);
  finally
    lClasses.Free;
  end;
end;

end.
