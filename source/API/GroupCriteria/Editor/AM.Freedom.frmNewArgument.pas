unit AM.Freedom.frmNewArgument;

interface

uses
  System.Generics.Collections,
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
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  AM.Freedom.frmBaseArgumentParameter,
  AM.Freedom.ArgumentDesignerRegister;

type
  TfrmNewArgument = class(TfrmBaseCriteria)
    pnlArgument: TPanel;
    lblArgument: TLabel;
    cbxArgument: TComboBox;
    lblCreate: TLabel;
    cbxCreateAs: TComboBox;
    procedure cbxArgumentChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FListParameters: TObjectList<TfrmBaseCriteria>;
    procedure DoCreateParams;
    function GetArgumentClassName: String;
    function GetCreateAs: String;
  protected
    function GetAlignToParent: TAlign; override;
    function GetDescription: String; override;
  public
    class function NewArgument(Aowner: TComponent; pArgumentName: String): TfrmNewArgument;
    procedure Validate; override;
    constructor Create(AOwner: TComponent; pArgumentName: String); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses
  System.StrUtils,
  System.Math,
  Vcl.ComCtrls;

procedure TfrmNewArgument.cbxArgumentChange(Sender: TObject);
begin
  inherited;
  DoCreateParams;
end;

constructor TfrmNewArgument.Create(AOwner: TComponent; pArgumentName: String);
var
  lArgument: TArgumentDesigner;
begin
  inherited Create(AOwner);
  FListParameters := TObjectList<TfrmBaseCriteria>.Create;
  pnlName.Visible := Trim(pArgumentName) <> '';
  pnlName.Caption := Format(pnlName.Caption, [pArgumentName]);
  for lArgument in TArgumentDesignerRegister.ArgumentDesignerList do
  begin
    cbxArgument.Items.Add(Copy(lArgument.ArgumentClassName, 2, Length(lArgument.ArgumentClassName)));
  end;
end;

destructor TfrmNewArgument.Destroy;
begin
  FListParameters.Free;
  inherited;
end;

procedure TfrmNewArgument.DoCreateParams;
var
  lArgument: TArgumentDesigner;
  lParameter: TParameterDesigner;
  lfrmParameter: TfrmBaseCriteria;
begin
  FListParameters.Clear;
  lArgument := TArgumentDesignerRegister.ArgumentDesignerList.FindArgumentClassName(GetArgumentClassName);
  if (Assigned(lArgument)) then
  begin
    for lParameter in lArgument.ParameterDesignerList do
    begin
      if (lParameter.ParamType <> dptArgument) then
      begin
        lfrmParameter := TfrmBaseArgumentParameter.Create(Self);
        TfrmBaseArgumentParameter(lfrmParameter).Parameter := lParameter;
      end
      else
      begin
        lfrmParameter := TfrmNewArgument.Create(Self, lParameter.ParamName);
      end;
      lfrmParameter.JumpTo(Self);
      if (lfrmParameter.ClassType = TfrmNewArgument) then
      begin
        lfrmParameter.Align := TAlign.alTop;
        lfrmParameter.AdjustHeigth;
      end;
      FListParameters.Add(lfrmParameter);
    end;
  end;
  lblCreate.Visible := SameText(cbxArgument.Text, 'ValueArgument');
  cbxCreateAs.Visible := lblCreate.Visible;
  if (Align <> TAlign.alClient) then
  begin
    AdjustHeigth;
  end;
end;

procedure TfrmNewArgument.FormShow(Sender: TObject);
begin
  inherited;
  if (Owner <> nil) and (Owner.ClassType = TTabSheet) then
  begin
    cbxArgument.SetFocus;
  end;
end;

function TfrmNewArgument.GetAlignToParent: TAlign;
begin
  Result := TAlign.alClient;
end;

function TfrmNewArgument.GetArgumentClassName: String;
begin
  Result := 'T' + cbxArgument.Text;
end;

function TfrmNewArgument.GetCreateAs: String;
begin
  if (not cbxCreateAs.Visible) then
  begin
    Result := 'Create';
  end
  else
  begin
    Result := 'CreateAs' + cbxCreateAs.Text;
  end;
end;

function TfrmNewArgument.GetDescription: String;
var
  lfrmBaseArgumentParameter: TfrmBaseCriteria;
begin
  Result := '';
  for lfrmBaseArgumentParameter in FListParameters do
  begin
    if lfrmBaseArgumentParameter.Description <> '' then
    begin
      Result := Result + Ifthen(Result <> '', ', ') +
          lfrmBaseArgumentParameter.Description;
    end;
  end;
  Result := GetArgumentClassName + '.' + GetCreateAs + '(' + Result + ')';
end;

class function TfrmNewArgument.NewArgument(Aowner: TComponent; pArgumentName: String): TfrmNewArgument;
begin
  Result := TfrmNewArgument.Create(Aowner, pArgumentName);
end;

procedure TfrmNewArgument.Validate;
var
  lfrmParameter: TfrmBaseCriteria;
begin
  if (cbxArgument.ItemIndex = -1) then
  begin
    ShowBalloHintForControl(cbxArgument, 'The argument class type is required!');
    Abort;
  end;
  if (cbxCreateAs.Visible) and (cbxCreateAs.ItemIndex = -1) then
  begin
    ShowBalloHintForControl(cbxCreateAs, 'The create type is required!');
    Abort;
  end;
  for lfrmParameter in FListParameters do
  begin
    lfrmParameter.Validate;
  end;

end;

end.



