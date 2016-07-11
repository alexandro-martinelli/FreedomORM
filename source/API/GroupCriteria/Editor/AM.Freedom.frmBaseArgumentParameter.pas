unit AM.Freedom.frmBaseArgumentParameter;

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
  AM.Freedom.frmBaseCriteria,
  Vcl.ImgList,
  Vcl.ExtCtrls,
  AM.Freedom.ArgumentDesignerRegister,
  Vcl.StdCtrls;

type
  TfrmBaseArgumentParameter = class(TfrmBaseCriteria)
    lblParameterName: TLabel;
    cbxParameterValue: TComboBox;
  private
    FParamType: TDesignerParamType;
    FParamName: String;
    FRequired: Boolean;
    FParameter: TParameterDesigner;
    procedure SetParamName(const Value: String);
    procedure SetParamType(const Value: TDesignerParamType);
    procedure SetParameter(const Value: TParameterDesigner);
  protected
    function GetDescription: String; override;
  public
    procedure Validate; override;
    property Parameter: TParameterDesigner read FParameter write SetParameter;
  end;

implementation

{$R *.dfm}

{ TfrmBaseArgumentParameter }

function TfrmBaseArgumentParameter.GetDescription: String;
begin
  Result := FParameter.Description(cbxParameterValue.Text);
end;

procedure TfrmBaseArgumentParameter.SetParameter(const Value: TParameterDesigner);
begin
  FParameter := Value;
  SetParamName(FParameter.ParamName);
  SetParamType(FParameter.ParamType);
  FRequired := FParameter.Required;
end;

procedure TfrmBaseArgumentParameter.SetParamName(const Value: String);
begin
  FParamName := Value;
  lblParameterName.Caption := FParamName + ': ';
end;

procedure TfrmBaseArgumentParameter.SetParamType(const Value: TDesignerParamType);
begin
  FParamType := Value;
end;

procedure TfrmBaseArgumentParameter.Validate;
begin
  if (Trim(cbxParameterValue.Text) = '') and FRequired then
  begin
    ShowBalloHintForControl(cbxParameterValue, Format('The parameter value for %s is required!', [FParamName]));
    Abort;
  end;
end;

end.
