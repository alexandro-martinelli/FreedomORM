unit AM.Freedom.Consts;

interface

uses
  AM.Freedom.EnumerationTypes,
  System.TypInfo;

type
  TConsts = class sealed
  public const
    cSQLTokens: Array [0..11] of String = ('.', '(', ')', ',', '+', '-', '*', '/', '>', '<', '=', '|');
    cStringDelimiters: array [0 .. 11] of string = ('.', ';', ')', ' ', ']', '[', '(', ',', ':', '=', #13, #10);
    cReadOnly   = [NoUpdate, NoDelete, NoInsert];
    cTriggerAll = [BeforeInsert, BeforeUpdate, BeforeDelete, AfterInsert, AfterUpdate, AfterDelete];
    cTriggerBeforeAll = [BeforeInsert, BeforeUpdate, BeforeDelete];
    cTriggerAfterAll = [AfterInsert, AfterUpdate, AfterDelete];
    cMethodAll  = [Before, After, OnError];
    cDDLAll     = [Fields, Constraints, Sequences];
    cLastUpdateTimeColumn = 'LAST_UPDATE_TIME';
    cNull                = 'NULL';
    DefaultColumnOptions = [];
    cDBLogTypeAll = [ltSelect, ltDDL, ltDML];
  end;

implementation

end.
