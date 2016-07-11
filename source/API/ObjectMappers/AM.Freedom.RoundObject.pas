unit AM.Freedom.RoundObject;

interface

uses
  System.SysUtils,
  AM.Freedom.EnumerationTypes;

type
  TRoundObject = class sealed
  private
    FRoundValue: Extended;
    FRoundDecimals: Byte;
    FRoundDecimalsMode: TRoundDecimalsMode;
    FHandled: Boolean;
    FColumnName: String;
    FObjectClass: TClass;
  public
    property RoundValue: Extended read FRoundValue write FRoundValue;
    property RoundDecimals: Byte read FRoundDecimals write FRoundDecimals;
    property RoundDecimalsMode: TRoundDecimalsMode read FRoundDecimalsMode write FRoundDecimalsMode default rdmApproach;
    property Handled: Boolean read FHandled write FHandled default False;
    property ColumnName: String read FColumnName write FColumnName;
    property ObjectClass: TClass read FObjectClass write FObjectClass;
  end;

implementation

end.
