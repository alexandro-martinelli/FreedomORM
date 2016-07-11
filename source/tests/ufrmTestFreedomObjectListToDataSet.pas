unit ufrmTestFreedomObjectListToDataSet;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Grids,
  Vcl.DBGrids,
  Data.DB;

type
  TfrmTestFreedomObjectListToDataSet = class(TForm)
    dtsData: TDataSource;
    DBGrid1: TDBGrid;
  public
    class procedure ShowData(pDataSet: TDataSet);
  end;

implementation

{$R *.dfm}

{ TfrmTestFreedomObjectListToDataSet }

class procedure TfrmTestFreedomObjectListToDataSet.ShowData(pDataSet: TDataSet);
var
  lfrmTestFreedomObjectListToDataSet: TfrmTestFreedomObjectListToDataSet;
begin
  lfrmTestFreedomObjectListToDataSet := TfrmTestFreedomObjectListToDataSet.Create(nil);
  try
    lfrmTestFreedomObjectListToDataSet.dtsData.DataSet := pDataSet;
    lfrmTestFreedomObjectListToDataSet.ShowModal;
  finally
    lfrmTestFreedomObjectListToDataSet.Free;
  end;
end;

end.
