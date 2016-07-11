unit AM.Freedom.INotificator;

interface

uses
  System.SysUtils;

type
  INotificator = interface
  ['{08B2A74F-9B4C-4F5A-AF30-54538FD54BA6}']
    procedure SendFreeNotification;
    procedure RegisterFreeNotification(pNotificator: INotificator);
    procedure ReceiveFreeNotification(pNotificator: INotificator);
    procedure RemoveNotificator(pNotificator: INotificator);
  end;

implementation

end.