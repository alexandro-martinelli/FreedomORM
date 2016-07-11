unit AM.Freedom.Persistent.IDSConnector;

interface

uses
  System.SysUtils,
  AM.Freedom.GroupCriteria,
  AM.Freedom.ICursor;

type
  IDSCursor = interface(ICursor)
    ['{82BE5170-0318-4CEA-85C9-CBC83D0107E3}']
  end;

  IDSStatement = interface
    ['{E4449394-13E4-44B5-9E22-958584E678A2}']
    function GetCursor(pClass: TClass; pGroupCriteria: TGroupCriteria): IDSCursor;
  end;

  IDSConnector = interface
    ['{1FEF65EB-9323-47BB-A946-0F9CF78EF14D}']
    function NewStatement: IDSStatement;
  end;


implementation

end.