unit UnitReader.TokenTypeHelper;

interface

uses
  System.SysUtils,
  UnitReader.EnumerationTypes;

type
  TTokenTypeHelper = record Helper for TTokenType
  public
    function IsMethodDeclaration: Boolean;
    function IsMethodImplementation: Boolean;
    function IsBlock: Boolean;
  end;

implementation

{ TTokenTypeHelper }


{ TTokenTypeHelper }

function TTokenTypeHelper.IsBlock: Boolean;
begin
  Result := Self.IsMethodImplementation or (Self in [tkUsesSection, tkTypeSection, tkConstSection,
      tkVarSection, tkClassDeclaration, tkRecordDeclaration, tkVarDeclaration, tkInitializationSection,
      tkFinalizationSection]);
end;

function TTokenTypeHelper.IsMethodDeclaration: Boolean;
begin
  Result := Self in [tkProcedureDeclaration, tkFunctionDeclaration, tkClassProcedureDeclaration,
      tkClassFunctionDeclaration, tkConstructorDeclaration, tkClassConstructorDeclaration,
      tkDestrutorDeclaration, tkClassDestructorDeclaration];
end;

function TTokenTypeHelper.IsMethodImplementation: Boolean;
begin
  Result := Self in [tkProcedureImplementation, tkFunctionImplementation, tkClassProcedureImplementation,
      tkClassFunctionImplementation, tkConstructorImplementation, tkClassConstructorImplementation,
      tkDestructorImplementation, tkClassDestructorImplementation];
end;

end.