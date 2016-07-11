unit AM.Freedom.Exceptions;

interface

uses
  System.SysUtils;

type
  EFreedomException = class(Exception);

  EInvalidArgument = class(EFreedomException)
  public
    constructor Create(const pArgumentClassName, pClassName: String); reintroduce;
  end;

  EInvalidArgumentValue = class(EFreedomException)
  public
    constructor Create(const pArgumentValueName: String); reintroduce;
  end;

  EInvalidReaderClass = class(EFreedomException)
  public
    constructor Create(const pAttributeClassName: String); reintroduce;
  end;

  EInvalidFieldAttribute = class(EFreedomException)
  public
    constructor Create(pAttributeClassName: String; pFieldName: String; pExpectedAttributeClassName: String = ''); reintroduce;
  end;

  EUnmappedClass = class(EFreedomException)
  public
    constructor Create(pClassName: String); reintroduce;
  end;

  ERequiredColumnValue = class(EFreedomException)
  public
    constructor Create(pColumnName, pObjectName: String); reintroduce;
  end;

  ERequiredListColumnValue = class(EFreedomException)
  public
    constructor Create(pColumnName, pObjectName: String); reintroduce;
  end;

  EInvalidReferencedColumnColumnType = class(EFreedomException)
  public
    constructor Create(pColumnName, pColumnType, pClassName: String); reintroduce;
  end;

  EInvalidMethodCallOnClass = class(EFreedomException)
  public
    constructor Create(pMethodName, pClassName: String); reintroduce;
  end;

  EInvalidLazyClass = class(EFreedomException)
  public
    constructor Create(pClassName, pRequiredClassName: String);
  end;

  EPersistError = class(EFreedomException)
  public
    constructor Create; reintroduce;
  end;

  ECannotPersistWithoutIDColumn = class(EFreedomException)
  public
    constructor Create(pClassName: String); reintroduce;
  end;

  ECannotLoadWithoutIDColumn = class(EFreedomException)
  public
    constructor Create(pClassName: String); reintroduce;
  end;

  ECannotPersistWithoutIDColumnValue = class(EFreedomException)
  public
    constructor Create(pClassName,  pIdColumnName: String); reintroduce;
  end;

  EInvalidConversion = class(EFreedomException)
  public
    constructor Create(pColumnName: string; pColumnValue: Variant); reintroduce;
  end;

  EInvalidReferenceClass = class(EFreedomException)
  public
    constructor Create(pColumnName, pClassName, pFieldName: String); reintroduce;
  end;

  EInvalidCursorColumnName = class(EFreedomException)
  public
    constructor Create(pColumnName: String); reintroduce;
  end;

  EInvalidCursorColumnIndex = class(EFreedomException)
  public
    constructor Create(pColumnIndex: Integer); reintroduce;
  end;

  EInvalidSQLText = class(EFreedomException)
  public
    constructor Create(pSQLText, pErrorMessage: String); reintroduce;
  end;

  EInvalidColumnsForJoin = class(EFreedomException)
  public
    constructor Create(pOnClassName, pToClassName: String); reintroduce;
  end;

  EInvalidBlobFieldTypeToStream = class(EFreedomException)
  public
    constructor Create(pDataType, pColumnName: String); reintroduce;
  end;

  EInvalidColumnsInConstraintClass = class(EFreedomException)
  public
    constructor Create(pConstraintClassName: String); reintroduce;
  end;

  EDifferentColumnsInConstraintClass = class(EFreedomException)
  public
    constructor Create(pExpectedNumberOfColumns, pReceivedNumberOfColumns: Byte; pConstraintClassName, pPropertyName: String); reintroduce;
  end;

  EInvalidSourceList = class(EFreedomException)
  public
    constructor Create(pObjectName, pOwnerName: String); reintroduce;
  end;

  EInvalidObjectClass = class(EFreedomException)
  public
    constructor Create(pObjectName, pOwnerName: String); reintroduce;
  end;

  EInvalidPropertyNameForFilterExpression = class(EFreedomException)
  public
    constructor Create(pPropertyName, pColumnTypeName: String); reintroduce;
  end;

  ESortPropertyNotFound = class(EFreedomException)
  public
    constructor Create(pPropertyName, pClassName: String); reintroduce;
  end;

  EInvalidFieldExtensionAttribute = class(EFreedomException)
  public
    constructor Create(pFieldName, pClassName: String); reintroduce;
  end;

  EFieldValueCannotBeAssignedOnClass = class(EFreedomException)
  public
    constructor Create(pFieldName, pFieldClassName, pOnClassName: String); reintroduce;
  end;

  EInvalidFieldCommandForPersistentClass = class(EFreedomException)
  public
    constructor Create(pFieldCommandClassName, pPersistentClassName: String); reintroduce;
  end;

  EInvalidFieldColumnTypeDBConversion = class(EFreedomException)
  public
    constructor Create(pColumnTypeName, pSQLMapperClassName: String); reintroduce;
  end;

implementation

uses
  System.Variants;

{ EInvalidArgument }

constructor EInvalidArgument.Create(const pArgumentClassName, pClassName: String);
begin
  inherited CreateFmt('Invalid argument class(%s) for class %s', [pArgumentClassName, pClassName]);
end;

{ EInvalidReaderClass }

constructor EInvalidReaderClass.Create(const pAttributeClassName: String);
begin
  inherited CreateFmt('Unregistered column reader for attribute: %s', [pAttributeClassName]);
end;

{ EInvalidFieldAttribute }

constructor EInvalidFieldAttribute.Create(pAttributeClassName, pFieldName, pExpectedAttributeClassName: String);
begin
  inherited CreateFmt('Invalid attribute class %s for field %s.' + sLineBreak +
      'Expected attribute class is %s', [pAttributeClassName, pFieldName, pExpectedAttributeClassName]);
end;

{ EUnmappedClass }

constructor EUnmappedClass.Create(pClassName: String);
begin
  inherited CreateFmt('Class named %s is not mapped with attributes([AutoMapping] or [Entity(TableName) or [Cursor(TObjectCursorClass)]).', [pClassName])
end;

{ ERequiredColumnValue }

constructor ERequiredColumnValue.Create(pColumnName, pObjectName: String);
begin
  inherited CreateFmt('The value for column %s is required on object %s', [pColumnName, pObjectName]);
end;

{ ERequiredListColumnValue }

constructor ERequiredListColumnValue.Create(pColumnName, pObjectName: String);
begin
  inherited CreateFmt('The column %s require one or more rows in object %s', [pColumnName, pObjectName]);
end;

{ EInvalidReferencedColumnColumnType }

constructor EInvalidReferencedColumnColumnType.Create(pColumnName, pColumnType, pClassName: String);
begin
  inherited CreateFmt('Invalid column type(%s) for referenced columns(JoinColumn, DetailColumn, EntityColumn) on column %s at class %s',
      [pColumnType, pColumnName, pClassName]);
end;

{ EInvalidMethodClass }

constructor EInvalidMethodCallOnClass.Create(pMethodName, pClassName: String);
begin
  inherited CreateFmt('Invalid call at method %s on class %s', [pMethodName, pClassName]);
end;

{ EInvalidLazyClass }

constructor EInvalidLazyClass.Create(pClassName, pRequiredClassName: String);
begin
  inherited CreateFmt('%s does not inherits from %s, this type of class can''t be used in Lazy Load.', [pClassName, pRequiredClassName]);
end;

{ EPersistError }

constructor EPersistError.Create;
begin
  inherited Create('Error on persist Cursor, verify internal exception for more details');
end;

{ ECannotPersistWithoutIDColumn }

constructor ECannotPersistWithoutIDColumn.Create(pClassName: String);
begin
  inherited CreateFmt('Can''t persist class %s without ID column specified', [pClassName]);
end;

{ EInvalidArgumentValue }

constructor EInvalidArgumentValue.Create(const pArgumentValueName: String);
begin
  inherited CreateFmt('TValueArgument does not support values of type "%s"', [pArgumentValueName]);
end;

{ EInvalidConversion }

constructor EInvalidConversion.Create(pColumnName: string; pColumnValue: Variant);
begin
  Inherited CreateFmt('Invalid Conversion from column %s at value %s', [pColumnName, VarToStr(pColumnValue)]);
end;

{ EInvalidReferenceClass }

constructor EInvalidReferenceClass.Create(pColumnName, pClassName, pFieldName: String);
begin
  inherited CreateFmt('Invalid reference class on %s at column %s on mapped field %s', [pClassName, pColumnName, pFieldName]);
end;

{ EInvalidCursorColumnName }

constructor EInvalidCursorColumnName.Create(pColumnName: String);
begin
  inherited CreateFmt('Invalid persistent cursor column name %s', [pColumnName]);
end;

{ EInvalidSQLText }

constructor EInvalidSQLText.Create(pSQLText, pErrorMessage: String);
begin
  inherited CreateFmt('Invalid SQL Text:%s%s%s%s%s at message:%s%s%s', [sLineBreak, sLineBreak, pSQLText, sLineBreak, sLineBreak,  pErrorMessage]);
end;

{ EInvalidColumnsForJoin }

constructor EInvalidColumnsForJoin.Create(pOnClassName, pToClassName: String);
begin
  inherited CreateFmt('Invalid columns for join on Class %s to class %s', [pOnClassName, pToClassName]);
end;

{ EInvalidCursorColumnIndex }

constructor EInvalidCursorColumnIndex.Create(pColumnIndex: Integer);
begin
  inherited CreateFmt('Invalid cursor column at index %d', [pColumnIndex]);
end;

{ EInvalidBlobFieldTypeToStream }

constructor EInvalidBlobFieldTypeToStream.Create(pDataType, pColumnName: String);
begin
  inherited CreateFmt('Invalid conversion from field %s(%s) to data type "Blob"', [pDataType, pColumnName]);
end;

{ EDifferentColumnsInConstraintClass }

constructor EDifferentColumnsInConstraintClass.Create(pExpectedNumberOfColumns, pReceivedNumberOfColumns: Byte;
  pConstraintClassName, pPropertyName: String);
begin
  inherited CreateFmt('The constraint type %s expect %d columns in %s but receive %d columns',
      [pConstraintClassName, pExpectedNumberOfColumns, pPropertyName, pReceivedNumberOfColumns]);
end;

{ EInvalidColumnsInConstraintClass }

constructor EInvalidColumnsInConstraintClass.Create(pConstraintClassName: String);
begin
  inherited CreateFmt('The constraint type %s required one or more columns', [pConstraintClassName]);
end;

{ EInvalidSourceList }

constructor EInvalidSourceList.Create(pObjectName, pOwnerName: String);
begin
  inherited CreateFmt('Invalid List Source at Object %s on %s', [pObjectName, pOwnerName]);
end;

{ EInvalidObjectClass }

constructor EInvalidObjectClass.Create(pObjectName, pOwnerName: String);
begin
  inherited CreateFmt('Invalid object class at Object %s on %s', [pObjectName, pOwnerName]);
end;

{ EInvalidPropertyNameForFilterExpression }

constructor EInvalidPropertyNameForFilterExpression.Create(pPropertyName, pColumnTypeName: String);
begin
  inherited CreateFmt('Invalid property %s at type %s for filter expression evaluation', [pPropertyName, pColumnTypeName]);
end;

{ ESortPropertyNotFound }

constructor ESortPropertyNotFound.Create(pPropertyName, pClassName: String);
begin
  inherited CreateFmt('Invalid property name %s.%s for sort on list', [pClassName, pPropertyName]);
end;

{ EInvalidFieldExtensionAttribute }

constructor EInvalidFieldExtensionAttribute.Create(pFieldName, pClassName: String);
begin
  inherited CreateFmt('Invalid extension field %s on class %s, extension fields needs to be a class type', [pFieldName, pClassName]);
end;

{ ECannotPersistWithoutIDColumnValue }

constructor ECannotPersistWithoutIDColumnValue.Create(pClassName, pIdColumnName: String);
begin
  inherited CreateFmt('The value for id column "%s" must be specified to persist class "%s"', [pIdColumnName, pClassName]);
end;

{ EFieldValueCannotBeAssignedOnClass }

constructor EFieldValueCannotBeAssignedOnClass.Create(pFieldName, pFieldClassName, pOnClassName: String);
begin
  inherited CreateFmt('The value for field "%s: %s" cannot be assigned on class "%s"(the field is not created)',
      [pFieldName, pFieldClassName, pOnClassName]);
end;

{ ECannotLoadWithoutIDColumn }

constructor ECannotLoadWithoutIDColumn.Create(pClassName: String);
begin
  inherited CreateFmt('Can''t load by ID in class %s without ID column specified', [pClassName]);
end;

{ EInvalidFieldCommandForPersistentClass }

constructor EInvalidFieldCommandForPersistentClass.Create(pFieldCommandClassName, pPersistentClassName: String);
begin
  inherited CreateFmt('The field command class %s cannot be mapped on Persistent %s', [pFieldCommandClassName, pPersistentClassName]);
end;

{ EInvalidFieldColumnTypeDBConversion }

constructor EInvalidFieldColumnTypeDBConversion.Create(pColumnTypeName, pSQLMapperClassName: String);
begin
  inherited CreateFmt('The column type %s cannot be converted for SQLMapper %s.', [pColumnTypeName, pSQLMapperClassName]);
end;

end.

