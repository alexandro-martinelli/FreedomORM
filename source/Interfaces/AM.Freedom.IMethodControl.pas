unit AM.Freedom.IMethodControl;

interface

uses
  System.SysUtils;

type
  IMethodControl = interface
  ['{6EDC4B2A-35E7-42D3-BF14-D8664657F9F6}']
    procedure BeforeExecute(Sender: TObject);
    procedure OnExecuteError(Sender: TObject; const E: Exception);
    procedure AfterExecute(Sender: TObject);
  end;

implementation

end.
