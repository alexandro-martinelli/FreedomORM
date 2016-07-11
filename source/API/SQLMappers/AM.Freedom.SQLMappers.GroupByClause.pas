unit AM.Freedom.SQLMappers.GroupByClause;

interface

uses
  System.Generics.Collections,
  AM.Freedom.SQLMapper.CustomArgument;

type
  TGroupByClause = class(TObjectList<TCustomArgument>)
  public
    constructor Create(pFields: Array of TCustomArgument); overload;
  end;

implementation

{ TGroupByClause }

constructor TGroupByClause.Create(pFields: array of TCustomArgument);
var
  I: Integer;
begin
  inherited Create;
  for I := Low(pFields) to High(pFields) do
  begin
    Add(pFields[I]);
  end;
end;

end.
