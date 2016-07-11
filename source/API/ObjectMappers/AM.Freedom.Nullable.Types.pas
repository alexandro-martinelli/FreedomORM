unit AM.Freedom.Nullable.Types;

interface

uses
  System.SysUtils,
  AM.Freedom.Nullable;

type
  TBooleanNullable = class(TNullable<Boolean>)
  public
    property BooleanValueOptions;
  end;
  TByteNullable = class(TNullable<Byte>);
  TSmallIntNullable = class(TNullable<SmallInt>);
  TIntegerNullable = class(TNullable<Integer>);
  TInt64Nullable = class(TNullable<Int64>);
  TDateNullable = class(TNullable<TDate>);
  TTimeNullable = class(TNullable<TTime>);
  TDateTimeNullable = class(TNullable<TDateTime>);
  TStringNullable = class(TNullable<String>);
  TSingleNullable = class(TNullable<Single>);
  TDoubleNullable = class(TNullable<Double>);
  TCurrencyNullable = class(TNullable<Currency>);
  TExtendedNullable = class(TNullable<Extended>);

implementation

end.