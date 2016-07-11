unit AM.UnitReader.Enumerations;

interface

type
  TOnLogMessage = procedure(pLogMessage: String) of object;

  TWordType = (wtMethod, wtParameter, wtVariable, wtConstant, wtClass, wtRecord);
  TVerifyResult = (OK, TooShort, NotFound);
  TVisibilityScope = (vsStrictPrivate, vsPrivate, vsStrictProtected, vsProtected, vsPublic, vsPublished, vsUnknow);
  TMethodType = (mtProcedure, mtFunction, mtConstructor, mtDestructor);
  TMemberType = (mtUnknow, mtByte, mtSmallInt, mtInteger, mtInt64, mtShortInt, mtWord, mtCardinal, mtUInt8, mtInt8,
      mtUInt16, mtInt16, mtUInt32, mtInt32, mtUInt64, mtSingle, mtReal, mtDouble, mtExtended, mtCurrency,
      mtBoolean, mtDate, mtTime, mtDateTime, mtTStrings, mtString, mtChar);

implementation

end.
