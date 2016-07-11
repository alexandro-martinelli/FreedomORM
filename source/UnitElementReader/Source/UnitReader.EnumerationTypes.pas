unit UnitReader.EnumerationTypes;

interface

uses
  System.SysUtils;

type
  TTokenType = (tkUnknow, tkComment, tkUnitSection, tkInterfaceSection, tkUsesSection, tkTypeSection, tkConstSection,
      tkVarSection, tkEnumerator, tkSet, tkClassDeclaration, tkVisibilityEscope, tkField, tkClassField, tkProcedureDeclaration,
      tkFunctionDeclaration, tkClassProcedureDeclaration, tkClassFunctionDeclaration, tkConstructorDeclaration,
      tkDestrutorDeclaration, tkClassConstructorDeclaration, tkClassDestructorDeclaration, tkRecordDeclaration, tkImplementationSection,
      tkProcedureImplementation, tkFunctionImplementation, tkConstructorImplementation, tkDestructorImplementation,
      tkClassProcedureImplementation, tkClassFunctionImplementation, tkClassConstructorImplementation, tkClassDestructorImplementation,
      tkVarDeclaration, tkTry, tkFinally, tkExcept, tkBegin, tkEnd, tkAttribute, tkClassOf, tkClassForward, tkProperty,
      tkBlankLine, tkInitializationSection, tkFinalizationSection, tkEndUnit);
  TElementType = (etLine, etBlock);

implementation

end.
