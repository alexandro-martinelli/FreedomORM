unit AM.Freedom.FreedomAttributes;

interface

uses
  System.Generics.Collections,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Consts;

type
  TAttributeClass = class of FreedomAttribute;

  FreedomAttribute = class(TCustomAttribute);

  Constraint = class(FreedomAttribute)
  strict private
    FColumns: TList<String>;
    FSchemas: TList<String>;
  protected
    procedure ExtractStringsToList(pColumnNames: String; pListOfString: TList<String>);
  public
    constructor Create(pColumnNames: String; pSchemaNames: String = ''); virtual;
    destructor Destroy; override;
    property Columns: TList<String> read FColumns;
    property Schemas: TList<String> read FSchemas;
  end;

  ColumnClass = class of CustomNamedColumn;

  CustomColumn = class abstract(FreedomAttribute)
  strict private
    FOptions: TColumnOptions;
  public
    constructor Create(pOptions: TColumnOptions = TConsts.DefaultColumnOptions); virtual;
    property Options: TColumnOptions read FOptions;
  end;

  CustomNamedColumn = class(CustomColumn)
  strict private
    FName: string;
    FAlias: String;
  public
    constructor Create(pName: string; pOptions: TColumnOptions = TConsts.DefaultColumnOptions; pAlias: String = ''); reintroduce; virtual;
    property Name: string read FName;
    property Alias: String read FAlias;
  end;

  ReferenceColumn = class(CustomNamedColumn)
  strict private
    FRefColumnName: string;
  public
    constructor Create(pName: string; pRefColumnName: string = ''; pOptions: TColumnOptions = TConsts.DefaultColumnOptions; pAlias: String = ''); reintroduce;
    property RefColumnName: string read FRefColumnName;
  end;

  CustomJoinedColumn = class(ReferenceColumn)
  strict private
    FJoinKind: TJoinKind;
    FRefClass: TClass;
    FRefTableName: string;
    FRefResultColumnName: string;
    FUpdateAction: TForeignOption;
    FDeleteAction: TForeignOption;
    FRefTableAlias: string;
  public
    constructor Create(pName: string; pRefColumnName: string = ''; pOptions: TColumnOptions = TConsts.DefaultColumnOptions;
        pJoinKind: TJoinKind = jkLeft; pAlias: String = '';
        pUpdateAction: TForeignOption = NoAction; pDeleteAction: TForeignOption = NoAction); reintroduce; overload;
    constructor Create(pRefResultColumnName: String; pName: string; pRefClass: TClass; pRefColumnName: string = '';
        pOptions: TColumnOptions = TConsts.DefaultColumnOptions;
        pJoinKind: TJoinKind = jkLeft; pAlias: String = '';
        pUpdateAction: TForeignOption = NoAction; pDeleteAction: TForeignOption = NoAction); reintroduce; overload;

    constructor Create(pRefResultColumnName: String; pName: string; pRefTableName: String; pRefTableAlias: String = '';
        pRefColumnName: string = '';
        pOptions: TColumnOptions = TConsts.DefaultColumnOptions;
        pJoinKind: TJoinKind = jkLeft; pAlias: String = '';
        pUpdateAction: TForeignOption = NoAction; pDeleteAction: TForeignOption = NoAction); reintroduce; overload;
    property JoinKind: TJoinKind read FJoinKind;
    property RefMetaClass: TClass read FRefClass;
    property RefTableName: string read FRefTableName;
    property RefTableAlias: string read FRefTableAlias;
    property RefResultColumnName: String read FRefResultColumnName;
    property UpdateAction: TForeignOption read FUpdateAction;
    property DeleteAction: TForeignOption read FDeleteAction;
  end;

implementation

uses
  System.Classes, System.SysUtils, AM.Freedom.Exceptions;

{ CustomColumn }

constructor CustomColumn.Create(pOptions: TColumnOptions);
begin
  FOptions := pOptions;
end;

{ TCustomNamedColumn }

constructor CustomNamedColumn.Create(pName: string; pOptions: TColumnOptions; pAlias: String);
begin
  inherited Create(pOptions);
  FName := pName;
  FAlias := pAlias;
end;

{ ReferenceColumn }

constructor ReferenceColumn.Create(pName: string; pRefColumnName: string; pOptions: TColumnOptions; pAlias: String);
begin
  inherited Create(pName, pOptions, pAlias);
  if (pRefColumnName <> '') then
  begin
    FRefColumnName := pRefColumnName;
  end
  else
  begin
    FRefColumnName := pName;
  end;
end;

{ CustomJoinedColumn }

constructor CustomJoinedColumn.Create(pName, pRefColumnName: string; pOptions: TColumnOptions; pJoinKind: TJoinKind;
    pAlias: String; pUpdateAction, pDeleteAction: TForeignOption);
begin
  Inherited Create(pName, pRefColumnName, pOptions, pAlias);
  FJoinKind := pJoinKind;
  FUpdateAction := pUpdateAction;
  FDeleteAction := pDeleteAction;
end;

constructor CustomJoinedColumn.Create(pRefResultColumnName: String; pName: string; pRefClass: TClass; pRefColumnName: string;
    pOptions: TColumnOptions; pJoinKind: TJoinKind; pAlias: String;
    pUpdateAction, pDeleteAction: TForeignOption);
begin
  if (pRefColumnName = '') then
  begin
    pRefColumnName := pName;
  end
  else if (pName = '') then
  begin
    pName := pRefColumnName;
  end;
  Inherited Create(pName, pRefColumnName, pOptions, pAlias);
  FJoinKind := pJoinKind;
  FRefClass := pRefClass;
  FRefResultColumnName := pRefResultColumnName;
  FUpdateAction := pUpdateAction;
  FDeleteAction := pDeleteAction;
  FRefTableName := '';
  FRefTableAlias := '';
end;

constructor CustomJoinedColumn.Create(pRefResultColumnName: String; pName, pRefTableName, pRefTableAlias, pRefColumnName: String;
    pOptions: TColumnOptions; pJoinKind: TJoinKind; pAlias: String; pUpdateAction, pDeleteAction: TForeignOption);
begin
  Inherited Create(pName, pRefColumnName, pOptions, pAlias);
  FRefClass := nil;
  FJoinKind := pJoinKind;
  FRefTableName := pRefTableName;
  FRefTableAlias := pRefTableAlias;
  FRefResultColumnName := pRefResultColumnName;
  FUpdateAction := pUpdateAction;
  FDeleteAction := pDeleteAction;
end;

{ Constraint }

constructor Constraint.Create(pColumnNames: String; pSchemaNames: String = '');
begin
  FColumns := TList<String>.Create;
  FSchemas := TList<String>.Create;
  ExtractStringsToList(pColumnNames, FColumns);
  if FColumns.Count = 0 then
  begin
    raise EInvalidColumnsInConstraintClass.Create(ClassName);
  end;
  ExtractStringsToList(pSchemaNames, FSchemas);
end;

destructor Constraint.Destroy;
begin
  FColumns.Free;
  FSchemas.Free;
  inherited;
end;

procedure Constraint.ExtractStringsToList(pColumnNames: String; pListOfString: TList<String>);
var
  lColumnNames: TStrings;
  lName: String;
begin
  lColumnNames := TStringList.Create;
  try
    ExtractStrings([',', ';'], [' '], PWideChar(LowerCase(pColumnNames)), lColumnNames);
    for lName in lColumnNames do
    begin
      if not pListOfString.Contains(lName) then
      begin
        pListOfString.Add(lName);
      end;
    end;
  finally
    lColumnNames.Free;
  end;
end;

end.
