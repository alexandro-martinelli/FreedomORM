unit AM.Freedom.EnumerationTypes;

interface

type
  TComparatorType = (cpEqual, cpDifferent, cpGreaterThan, cpLessThan, cpGreaterThanOrEqualTo, cpLessThanOrEqualTo, cpNull,
      cpNotNull, cpContaining, cpStartingWith, cpLikeLeft, cpLikeMidle, cpLikeRigth, cpBetween, cpIn, cpExists);
  TPolicy = (poAnd, poOr, poAndNot, poOrNot);
  TCharCase = (Normal, Upper, Lower);
  TCriteriaColumnType = (cvtUnknow, cvtSmallint, cvtInteger, cvtInt64, cvtString, cvtDouble, cvtCurrency, cvtExtended, cvtDate,
      cvtTime, cvtDateTime, cvtBoolean, cvtEnumerator, cvtSet);
  TCriteriaType = (ctSQL, ctProperty);
  TOrderType = (Asc, Desc);
  TJoinKind = (jkNone, jkJoin, jkLeft, jkRigth, jkInner, jkCross, jkOuter, jkLeftOuter, jkRigthOuter, jkCrossOuter);
  TColumnType = (ctyUnknow, ctyByte, ctySmallint, ctyInteger, ctyInt64, ctyChar, ctyString, ctySingle, ctyDouble, ctyCurrency, ctyExtended,
      ctyDate, ctyTime, ctyDateTime, ctyBoolean, ctyEnumerator, ctyDetail, ctyJoin, ctyBlob, ctyMemo, ctyGuid, ctyExtension, ctyXML);
  TColumnTypes = set of TColumnType;
  TCalcExpressionType = (Addition, Subtract, Multiply, Divide);
  TForeignOption = (NoAction, Cascade, SetNull, SetDefault);
  TCommandType = (Select, Add, Alter, Drop, Create);
  TConstraintType = (PrimaryKey, ForeignKey, UniqueKey);
  TColumnOption = (Required, NoInsert, NoUpdate, NoDelete);
  TColumnOptions = set of TColumnOption;
  TTriggerOption = (BeforeInsert, BeforeUpdate, BeforeDelete, AfterInsert, AfterUpdate, AfterDelete);
  TTriggerOptions = set of TTriggerOption;
  TMethodOption = (Before, After, OnError);
  TMethodOptions = set of TMethodOption;
  TIdOption = (None, Identity, Sequence);
  TDescriptionTextType = (dttField, dttTable, dttDomain);
  TLazyType = (Simple, List, Blob);
  TObjectState = (Unknown, Clean, Inserted, Deleted);
  TSortType = TOrderType;
  TStrSortOptions = set of (ssoCaseInsensitive);
  TFindStrOption = (fsoCaseInsensitive, fsoContaining);
  TFindStrOptions = set of TFindStrOption;
  TCompareStrOptions = set of (cssCaseInsensitive);
  TSchemaState = TObjectState;
  TDDLOption = (Fields, Constraints, Sequences);
  TDDLOptions = set of TDDLOption;
  TBlobType = (Text, Binary, XML, Image);
  TObjectToMapperOption = (SubLevels, Properties);
  TObjectToMapperOptions = set of TObjectToMapperOption;
  TEnumerationType = (emChar, emByte);
  TUpdateObjectMode = (uomManual, uomOnPersist);
  TUpdateObjectOption = (uooJoins, uooDetails);
  TUpdateObjectOptions = set of TUpdateObjectOption;
  TConnectorType = (Firebird, SQLServer, Oracle, PostGree, MySQL, Interbase, DataSnap);
  TConnectorTypes = set of TConnectorType;
  TUnionType = (Union, UnionAll);
  TAddAllFieldsMode = (Asterisk, FieldByField);
  TChangedProperties = set of (DataType, Nullable, DataSize);
  TAlterFieldCommandParam = (DontChangeNullable, UseSetOnNullabeChanged, UseTypeOnDataTypeChanged);
  TAlterFieldCommandParams = set of TAlterFieldCommandParam;
  TChangeAliasMode = (cmEmptys, cmAll);
  TNullOption = (noNone, noFirst, noLast);
  TIndexOption = (ioNone, ioClustered, ioConcurrently, ioDescending);
  TNullableChange = (nNull, nNotNull, nDontChange);
  TRoundDecimalsMode = (rdmApproach, rdmDown, rdmUp, rdmTrunc);
  TPersistObjectMode = (pomUpdate, pomDrop);
  TDBLogType = (ltDDL, ltDML, ltSelect);
  TDBLogTypes = set of TDBLogType;

implementation

end.
