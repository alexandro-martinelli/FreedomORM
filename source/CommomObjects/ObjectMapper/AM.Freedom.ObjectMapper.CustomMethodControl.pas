unit AM.Freedom.ObjectMapper.CustomMethodControl;

interface

uses
  AM.Freedom.IMethodControl,
  System.SysUtils;

type
  TMethodControlClass = class of TCustomMethodControl;

  TCustomMethodControl = class(TInterfacedObject, IMethodControl)
  public
    procedure BeforeExecute(Sender: TObject); virtual; abstract;
    procedure OnExecuteError(Sender: TObject; const E: Exception); virtual; abstract;
    procedure AfterExecute(Sender: TObject); virtual; abstract;
  end;

implementation

end.
