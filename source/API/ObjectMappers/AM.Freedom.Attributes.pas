unit AM.Freedom.Attributes;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  AM.Freedom.EnumerationTypes,
  AM.Freedom.Consts,
  AM.Freedom.ObjectMapper.CustomTrigger,
  AM.Freedom.ObjectMapper.CustomMethodControl,
  AM.Freedom.FreedomAttributes,
  AM.Freedom.ObjectMapper.CustomObjectCursor;

type
  TColumnOptions = AM.Freedom.EnumerationTypes.TColumnOptions;
  TTriggerOptions = AM.Freedom.EnumerationTypes.TTriggerOptions;
  TIdOption = AM.Freedom.EnumerationTypes.TIdOption;
  TOrderType = AM.Freedom.EnumerationTypes.TOrderType;
  TForeignOption = AM.Freedom.EnumerationTypes.TForeignOption;

  AutoMapping = class(FreedomAttribute);

  NoMapping = class(FreedomAttribute);

  Unique = class(Constraint);

  Primary = class(Constraint);

  Foreign = class(Constraint)
  strict private
    FOnUpdate: TForeignOption;
    FReferenceColumns: TList<String>;
    FReferencesTo: String;
    FOnDelete: TForeignOption;
  public
    constructor Create(pColumnNames, pReferencesTo: String; pReferenceColumns: String = '';
        pOnUpdate: TForeignOption = NoAction; pOnDelete: TForeignOption = NoAction;
        pSchemaNames: string = ''); reintroduce;
    destructor Destroy; override;
    property ReferenceColumns: TList<String> read FReferenceColumns;
    property ReferencesTo: String read FReferencesTo;
    property OnUpdate: TForeignOption read FOnUpdate;
    property OnDelete: TForeignOption read FOnDelete;
  end;

  Schema = class(FreedomAttribute)
  private
    FName: String;
    FDefault: Boolean;
  public
    constructor Create(pName: String; pDefault: Boolean = False);
    property Name: String read FName;
    property Default: Boolean read FDefault;
  end;

  Entity = class(FreedomAttribute)
  protected
    FName: string;
    FAlias: string;
  public
    constructor Create(pName: string; pAlias: string = '');
    property Name: string read FName;
    property Alias: string read FAlias;
  end;

  Cursor = class(FreedomAttribute)
  protected
    FMetaClass: TObjectCursorClass;
  public
    constructor Create(pMetaClass: TObjectCursorClass);
    property MetaClass: TObjectCursorClass read FMetaClass;
  end;

  Trigger = class(FreedomAttribute)
  private
    FMetaClass: TTriggerClass;
    FOptions: TTriggerOptions;
  public
    constructor Create(pMetaClass: TTriggerClass; pOptions: TTriggerOptions);
    property MetaClass: TTriggerClass read FMetaClass;
    property Options: TTriggerOptions read FOptions;
  end;

  VirtualMethodControl = class(FreedomAttribute)
  private
    FMetaClass: TMethodControlClass;
    FOptions: TMethodOptions;
  public
    constructor Create(pMetaClass: TMethodControlClass; pOptions: TMethodOptions);
    property MetaClass: TMethodControlClass read FMetaClass;
    property Options: TMethodOptions read FOptions;
  end;

  Domain = class(FreedomAttribute)
  private
    FName: String;
  public
    constructor Create(pName: String);
    property Name: String read FName;
  end;

  Column = class(CustomNamedColumn)
  strict private
    FSize: UInt32;
    FScale: Byte;
  public
    constructor Create(pName: string; pOptions: TColumnOptions = TConsts.DefaultColumnOptions; pSize: UInt32 = 0; pScale: Byte = 0; pAlias: String = ''); reintroduce;
    property Size: UInt32 read FSize;
    property Scale: Byte read FScale;
  end;

  BooleanColumn = class(CustomNamedColumn)
  strict private
    FValueTrue: Variant;
    FValueFalse: Variant;
    FInternalColumnType: TColumnType;
  public
    constructor Create(pName: string; pOptions: TColumnOptions = TConsts.DefaultColumnOptions; pAlias: String = ''); overload; override;
    constructor Create(pName: string; pValueTrue: Integer; pValueFalse: Integer;
        pOptions: TColumnOptions = TConsts.DefaultColumnOptions; pAlias: String = ''); reintroduce; overload;
    constructor Create(pName: string; pValueTrue, pValueFalse: string;
      pOptions: TColumnOptions = TConsts.DefaultColumnOptions; pAlias: String = ''); reintroduce; overload;
    property ValueTrue: Variant read FValueTrue;
    property ValueFalse: Variant read FValueFalse;
    property InternalColumnType: TColumnType read FInternalColumnType;
  end;

  EnumerationColumn = class(CustomNamedColumn)
  strict private
    FEnumType: TEnumerationType;
    FEnumCharOf: TArray<Char>;
  public
    constructor Create(pName: string; pOptions: TColumnOptions = []; pAlias: string = ''); overload; override;
    constructor Create(pName: string; pEnumCharArray: String; pOptions: TColumnOptions = TConsts.DefaultColumnOptions; pAlias: String = ''); reintroduce; overload;
    property EnumType: TEnumerationType read FEnumType;
    property EnumCharOf: TArray<Char> read FEnumCharOf;
  end;

  BlobColumn = class(CustomNamedColumn)
  private
    FBlobType: TBlobType;
    FSize: UInt32;
  public
    constructor Create(pName: string; pOptions: TColumnOptions = []; pSize: UInt32 = 0; pBlobType: TBlobType = Binary; pAlias: string = ''); reintroduce;
    property BlobType: TBlobType read FBlobType write FBlobType;
    property Size: UInt32 read FSize write FSize;
  end;

  JoinedColumn = class(CustomJoinedColumn);

  DetailColumn = class(ReferenceColumn);

  Extension = class(FreedomAttribute);

  Rounded = class(FreedomAttribute)
  private
    FRoundDecimals: Byte;
    FRoundDecimalsMode: TRoundDecimalsMode;
    FCanBeModified: Boolean;
  public
    constructor Create(pRoundDecimals: Byte = 0; pRoundDecimalsMode: TRoundDecimalsMode = rdmApproach; pCanBeModified: Boolean = True);
    property RoundDecimals: Byte read FRoundDecimals;
    property RoundDecimalsMode: TRoundDecimalsMode read FRoundDecimalsMode;
    property CanBeModified: Boolean read FCanBeModified write FCanBeModified;
  end;

  Id = class(FreedomAttribute)
  private
    FSequenceName: string;
    FIdOption: TIdOption;
    FSchemas: TList<String>;
  public
    constructor Create(pIdOption: TIdOption = TIdOption.Identity; pSequenceName: string = ''; pSchemas: String = ''); reintroduce;
    destructor Destroy; override;
    property SequenceName: string read FSequenceName;
    property IdOption: TIdOption read FIdOption;
    property Schemas: TList<String> read FSchemas;
  end;

  Order = class(FreedomAttribute)
  private
    FIndex: Integer;
    FOrderType: TOrderType;
    FDirective: String;
  public
    constructor Create(pIndex: Integer; pOrderType: TOrderType = TOrderType.Asc; pDirective: String = '');
    property Index: Integer read FIndex;
    property OrderType: TOrderType read FOrderType;
    property Directive: String read FDirective;
  end;

  DefaultValue = class(DefaultAttribute);

  DefaultNowValue = class(DefaultValue)
  public
    constructor Create; reintroduce;
  end;

  Bind = class(FreedomAttribute)
  private
    FDisplayFormat: String;
    FDisplayLabel: String;
  public
    constructor Create(pDisplayFormat: String; pDisplayLabel: String = '');
    property DisplayFormat: String read FDisplayFormat;
    property DisplayLabel: String read FDisplayLabel;
  end;

implementation

uses
  System.Variants, AM.Freedom.Exceptions;

{ Entity }

constructor Entity.Create(pName: string; pAlias: string);
begin
  FName  := pName;
  FAlias := pAlias;
end;

{ BooleanColumn }

constructor BooleanColumn.Create(pName: string; pValueTrue, pValueFalse: Integer;
  pOptions: TColumnOptions; pAlias: String);
begin
  inherited Create(pName, pOptions, pAlias);
  FValueTrue  := pValueTrue;
  FValueFalse := pValueFalse;
  FInternalColumnType := ctyByte;
end;

constructor BooleanColumn.Create(pName, pValueTrue, pValueFalse: string;
  pOptions: TColumnOptions; pAlias: String);
begin
  inherited Create(pName, pOptions, pAlias);
  FValueTrue  := pValueTrue;
  FValueFalse := pValueFalse;
  FInternalColumnType := ctyString;
end;

constructor BooleanColumn.Create(pName: string; pOptions: TColumnOptions; pAlias: String);
begin
  inherited Create(pName, pOptions, pAlias);
  FValueTrue  := True;
  FValueFalse := False;
  FInternalColumnType := ctyBoolean;
end;

{ EnumerationColumn }

constructor EnumerationColumn.Create(pName: string; pEnumCharArray: String; pOptions: TColumnOptions; pAlias: String);
var
  lCounter: Integer;
  lStrings: TStrings;
begin
  Create(pName, pOptions, pAlias);
  FEnumType := emChar;
  lStrings := TStringList.Create;
  try
    ExtractStrings([',', ';'], [' '], pWideChar(pEnumCharArray), lStrings);
    SetLength(FEnumCharOf, lStrings.Count);
    for lCounter := 0 to lStrings.Count - 1 do
    begin
      FEnumCharOf[lCounter] := lStrings.Strings[lCounter][1];
    end;
  finally
    lStrings.Free;
  end;
end;

constructor EnumerationColumn.Create(pName: string; pOptions: TColumnOptions; pAlias: string);
begin
  inherited Create(pName, pOptions, pAlias);
  FEnumType := emByte;
  SetLength(FEnumCharOf, 0);
end;

{ Id }

constructor Id.Create(pIdOption: TIdOption; pSequenceName: String; pSchemas: String);
var
  lSchemas: TStrings;
  lSchema: String;
begin
  FSequenceName    := pSequenceName;
  FIdOption := pIdOption;
  FSchemas := TList<String>.Create;
  lSchemas := TStringList.Create;
  try
    ExtractStrings([',', ';'], [' '], PWideChar(LowerCase(pSchemas)), lSchemas);
    for lSchema in lSchemas do
    begin
      if not FSchemas.Contains(lSchema) then
      begin
        FSchemas.Add(lSchema);
      end;
    end;
  finally
    lSchemas.Free;
  end;
end;

destructor Id.Destroy;
begin
  FSchemas.Free;
  inherited;
end;

{ Order }

constructor Order.Create(pIndex: Integer; pOrderType: TOrderType; pDirective: String);
begin
  FIndex := pIndex;
  FOrderType := pOrderType;
  FDirective := pDirective;
end;

{ TNowValue }

constructor DefaultNowValue.Create;
begin
  inherited Create(Now);
end;

{ Trigger }

constructor Trigger.Create(pMetaClass: TTriggerClass; pOptions: TTriggerOptions);
begin
  FMetaClass := pMetaClass;
  FOptions   := pOptions;
end;

{ VirtualMethodControl }

constructor VirtualMethodControl.Create(pMetaClass: TMethodControlClass; pOptions: TMethodOptions);
begin
  FMetaClass := pMetaClass;
  FOptions   := pOptions;
end;

{ TColumn }

constructor Column.Create(pName: string; pOptions: TColumnOptions; pSize: UInt32; pScale: Byte; pAlias: String);
begin
  inherited Create(pName, pOptions, pAlias);
  FSize := pSize;
  FScale := pScale;
end;

{ Schema }

constructor Schema.Create(pName: String; pDefault: Boolean);
begin
  FName := LowerCase(pName);
  FDefault := pDefault;
end;

{ Domain }

constructor Domain.Create(pName: String);
begin
  FName := pName;
end;

{ Bind }

constructor Bind.Create(pDisplayFormat, pDisplayLabel: String);
begin
  FDisplayFormat := pDisplayFormat;
  FDisplayLabel := pDisplayLabel;
end;

{ Foreign }

constructor Foreign.Create(pColumnNames, pReferencesTo, pReferenceColumns: String; pOnUpdate, pOnDelete: TForeignOption;
  pSchemaNames: string);
var
  lName: String;
begin
  inherited Create(pColumnNames, pSchemaNames);
  FOnUpdate := pOnUpdate;
  FOnDelete := pOnDelete;
  FReferenceColumns := TList<String>.Create;
  ExtractStringsToList(pReferenceColumns, FReferenceColumns);
  if FReferenceColumns.Count = 0 then
  begin
    for lName in Columns do
    begin
      FReferenceColumns.Add(lName)
    end;
  end;
  if Columns.Count <> FReferenceColumns.Count then
  begin
    raise EDifferentColumnsInConstraintClass.Create(Columns.Count, FReferenceColumns.Count, ClassName, 'ReferenceColumns');
  end;
  FReferencesTo := pReferencesTo;
end;

destructor Foreign.Destroy;
begin
  FReferenceColumns.Free;
  inherited;
end;

{ Rounder }

constructor Rounded.Create(pRoundDecimals: Byte; pRoundDecimalsMode: TRoundDecimalsMode; pCanBeModified: Boolean);
begin
  FRoundDecimals := pRoundDecimals;
  FRoundDecimalsMode := pRoundDecimalsMode;
  FCanBeModified := pCanBeModified;
end;

{ Cursor }

constructor Cursor.Create(pMetaClass: TObjectCursorClass);
begin
  FMetaClass := pMetaClass;
end;

{ BlobColumn }

constructor BlobColumn.Create(pName: string; pOptions: TColumnOptions; pSize: UInt32; pBlobType: TBlobType; pAlias: string);
begin
  inherited Create(pName, pOptions, pAlias);
  FBlobType := pBlobType;
  FSize := pSize;
end;

end.
