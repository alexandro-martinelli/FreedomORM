unit AM.UnitReader.PrefixOptions;

interface

type
  TPrefixOptions = class sealed
  strict private
    FFields: Char;
    FConstants: Char;
    FParameters: Char;
    FVariables: Char;
  private
    FClasses: Char;
    FRecords: Char;
  public
    constructor Create;
    procedure Assign(pSource: TPrefixOptions);
    property Constants: Char read FConstants write FConstants;
    property Variables: Char read FVariables write FVariables;
    property Parameters: Char read FParameters write FParameters;
    property Fields: Char read FFields write FFields;
    property Classes: Char read FClasses write FClasses;
    property Records: Char read FRecords write FRecords;
  end;

implementation

{ TPrefixOptions }

procedure TPrefixOptions.Assign(pSource: TPrefixOptions);
begin
  FFields := pSource.Fields;
  FConstants := pSource.Constants;
  FParameters := pSource.Parameters;
  FVariables := pSource.Variables;
  FClasses := pSource.Classes;
  FRecords := pSource.Records;
end;

constructor TPrefixOptions.Create;
begin
  FFields := 'F';
  FConstants := 'c';
  FParameters := 'p';
  FVariables := 'l';
  FClasses := 'T';
  FRecords := '.';

end;

end.
