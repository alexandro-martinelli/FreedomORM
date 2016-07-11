unit AM.Freedom.IJSONObject;

interface

uses
  System.SysUtils,
  System.JSON,
  Data.DBXJSONReflect;

type
  IJSONObject = interface
  ['{DC84070B-2626-4E18-82CB-D9C353EEC0E3}']
    procedure DoBeforeMarshal(pJSONMarshal: TJSONMarshal);
    procedure DoBeforeUnMarshal(pJSONUnMarshal: TJSONUnMarshal);
    procedure DoAfterMarshal(pJSONMarshal: TJSONMarshal);
    procedure DoAfterUnMarshal(pJSONUnMarshal: TJSONUnMarshal);
    procedure DoGetConverters(pJSONMarshal: TJSONMarshal);
    procedure DoGetReverters(pJSONUnMarshal: TJSONUnMarshal);

    function ToJSON: TJSONValue;
    function FromJSON(pJSONValue: TJSONValue): TObject;
  end;


implementation

end.