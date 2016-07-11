unit AM.Freedom.Helper.ForeignOption;

interface

uses
  AM.Freedom.EnumerationTypes;

type
  TForeignOptionHelper = record helper for TForeignOption
  public
    function ToString: String;
    function ToDescription: String;
  end;

implementation

{ TForeignOptionHelper }

function TForeignOptionHelper.ToDescription: String;
begin
  case Self of
    Cascade: Result := 'Cascade';
    SetNull: Result := 'SetNull';
    SetDefault: Result := 'SetDefault';
    else
     Result := 'NoAction';
  end;
end;

function TForeignOptionHelper.ToString: String;
begin
  case Self of
    Cascade: Result := 'cascade';
    SetNull: Result := 'set null';
    SetDefault: Result := 'set default';
    else
      Result := 'no action';
  end;
end;

end.
