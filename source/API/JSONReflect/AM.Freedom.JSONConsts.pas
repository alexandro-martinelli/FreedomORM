unit AM.Freedom.JSONConsts;

interface

uses
  System.SysUtils;

type
  TJSONConsts = class sealed
  public const
    cFieldSeparator = ', ';
    cArrayValueSeparator = ', ';
    cArrayInitialization = '[';
    cArrayFinalization = ']';
    cClassInitialization = '{';
    cClassFinalization = '}';
    cIdentifierSeparator = ':';
    cFieldsInitialization = '{';
    cFieldsFinalization = '}';
    cNameValueSeparator = ':';
    cSimpleChars: Array of Char = [#13, #10, #9];
    cJSONChar: Array of String = ['/n', '/r', '/t'];
  end;

implementation

end.