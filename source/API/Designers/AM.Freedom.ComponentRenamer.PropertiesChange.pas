unit AM.Freedom.ComponentRenamer.PropertiesChange;

interface

uses
  System.SysUtils;

type
  TPropertiesChange = class
  private
    FName: String;
    FCaption: String;
  public
    property Name: String read FName write FName;
    property Caption: String read FCaption write FCaption;
  end;

implementation

end.