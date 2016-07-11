unit AM.Freedom.ClassFactory;

interface

uses
  AM.Freedom.SQLMappers.Arguments;

type
  TTableFactoryClass = class of TTableFactory;

  TTableFactory = class sealed
  strict private
    strict private
    class var FTable: TTableArgument;
    class procedure CreateTable;
  public
    class function Name(pName: String): TTableFactoryClass;
    class function Alias(pAlias: String): TTableFactoryClass;
    class function Return: TTableArgument;
  end;

  TFieldFactoryClass = class of TFieldFactory;

  TFieldFactory = class sealed
  strict private
    class var FField: TFieldArgument;
    class procedure CreateField;
  public
    class function Name(pName: String): TFieldFactoryClass;
    class function TableAlias(pTableAlias: String): TFieldFactoryClass;
    class function Alias(pAlias: String): TFieldFactoryClass;
    class function Return: TFieldArgument;
  end;

  TClassFactory = class sealed
  public
    class function Field(pName: string): TFieldFactoryClass;
    class function Table(pName: String): TTableFactoryClass;
    class function LiteralArgument(pLiteralValue: String): TLiteralArgument;
    class function NullArgument: TNullArgument;
    class function Value<T>(pValue: T): TValueArgument;
    class function ValueString(pValue: String): TValueArgument;
    class function ValueSmallint(pValue: Smallint): TValueArgument;
    class function ValueInteger(pValue: Integer): TValueArgument;
    class function ValueInt64(pValue: Int64): TValueArgument;
    class function ValueDouble(pValue: Double): TValueArgument;
    class function ValueExtended(pValue: Extended): TValueArgument;
    class function ValueCurrency(pValue: Currency): TValueArgument;
    class function ValueDate(pValue: TDate): TValueArgument;
    class function ValueTime(pValue: TTime): TValueArgument;
    class function ValueDateTime(pValue: TDateTime): TValueArgument;


  end;

implementation

uses
  System.SysUtils;

{ TFieldFactory }

class function TFieldFactory.Name(pName: String): TFieldFactoryClass;
begin
  CreateField;
  FField.Name := pName;
  Result := Self;
end;

class procedure TFieldFactory.CreateField;
begin
  if Assigned(FField) then
  begin
    FreeAndNil(FField);
  end;
  FField := TFieldArgument.Create;
end;

class function TFieldFactory.Return: TFieldArgument;
begin
  Result := FField;
  FField := nil;
end;

class function TFieldFactory.Alias(pAlias: String): TFieldFactoryClass;
begin
  Result := Self;
  FField.Alias := pAlias;
end;

class function TFieldFactory.TableAlias(pTableAlias: String): TFieldFactoryClass;
begin
  Result := Self;
  FField.TableAlias := pTableAlias;
end;


class function TTableFactory.Name(pName: String): TTableFactoryClass;
begin
  CreateTable;
  Result := Self;
  FTable.Name := pName;
end;

class procedure TTableFactory.CreateTable;
begin
  if Assigned(FTable) then
  begin
    FreeAndNil(FTable);
  end;
  FTable := TTableArgument.Create;
end;

class function TTableFactory.Return: TTableArgument;
begin
  Result := FTable;
  FTable := nil;
end;

class function TTableFactory.Alias(pAlias: String): TTableFactoryClass;
begin
  FTable.Alias := pAlias;
  Result := Self;
end;

{ TClassFactory }

class function TClassFactory.ValueCurrency(pValue: Currency): TValueArgument;
begin
  Result := TValueArgument.CreateAsCurrency(pValue);
end;

class function TClassFactory.ValueDateTime(pValue: TDateTime): TValueArgument;
begin
  Result := TValueArgument.CreateAsDateTime(pValue);
end;

class function TClassFactory.ValueDate(pValue: TDate): TValueArgument;
begin
  Result := TValueArgument.CreateAsDate(pValue);
end;

class function TClassFactory.ValueDouble(pValue: Double): TValueArgument;
begin
  Result := TValueArgument.CreateAsDouble(pValue);
end;

class function TClassFactory.ValueExtended(pValue: Extended): TValueArgument;
begin
  Result := TValueArgument.CreateAsExtended(pValue);
end;

class function TClassFactory.Field(pName: string): TFieldFactoryClass;
begin
  Result := TFieldFactory.Name(pName);
end;

class function TClassFactory.ValueInt64(pValue: Int64): TValueArgument;
begin
  Result := TValueArgument.CreateAsInt64(pValue);
end;

class function TClassFactory.ValueInteger(pValue: Integer): TValueArgument;
begin
  Result := TValueArgument.CreateAsInteger(pValue);
end;

class function TClassFactory.LiteralArgument(pLiteralValue: String): TLiteralArgument;
begin
  Result := TLiteralArgument.Create(pLiteralValue);
end;

class function TClassFactory.NullArgument: TNullArgument;
begin
  Result := TNullArgument.Create;
end;

class function TClassFactory.ValueSmallint(pValue: Smallint): TValueArgument;
begin
  Result := TValueArgument.CreateAsSmallint(pValue);
end;

class function TClassFactory.ValueString(pValue: String): TValueArgument;
begin
  Result := TValueArgument.CreateAsString(pValue);
end;

class function TClassFactory.Table(pName: string): TTableFactoryClass;
begin
  Result := TTableFactory.Name(pName);
end;

class function TClassFactory.ValueTime(pValue: TTime): TValueArgument;
begin
  Result := TValueArgument.CreateAsTime(pValue);
end;

class function TClassFactory.Value<T>(pValue: T): TValueArgument;
begin
  Result := TValueArgument.CreateAs<T>(pValue);
end;

end.
