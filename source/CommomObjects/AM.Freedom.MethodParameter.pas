unit AM.Freedom.MethodParameter;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  TParameterDirective = (pdValue, pdConst, pdVar, pdOut);

  TMethodParameter = class
  private
    FParameterName: String;
    FParameterType: String;
    FDirective: TParameterDirective;
    function GetDeclaration: String;
  public
    procedure Assign(pSource: TMethodParameter);
    property ParameterName: String read FParameterName write FParameterName;
    property ParameterType: String read FParameterType write FParameterType;
    property Directive: TParameterDirective read FDirective write FDirective;
    property Declaration: String read GetDeclaration;
  end;

  TMethodParameterList = class(TObjectList<TMethodParameter>)
  public
    function FindParameter(pParameterName: String): TMethodParameter;
  end;

  TParameterDirectiveHelper = record Helper for TParameterDirective
  public
    function ToString: String;
    function ToDeclaration: String;
  end;


implementation

uses
  System.StrUtils;

{ TParameterDirectiveHelper }

function TParameterDirectiveHelper.ToDeclaration: String;
begin
  case Self of
    pdConst: Result := 'const ';
    pdVar: Result := 'var ';
    pdOut: Result := 'out ';
    else
      Result := '';
  end;
end;

function TParameterDirectiveHelper.ToString: String;
begin
  case Self of
    pdConst: Result := 'Const';
    pdVar: Result := 'Var';
    pdOut: Result := 'Out';
    else
      Result := 'Value';
  end;
end;

{ TMethodParameter }

procedure TMethodParameter.Assign(pSource: TMethodParameter);
begin
  FParameterName := pSource.ParameterName;
  FParameterType := pSource.ParameterType;
  FDirective := pSource.Directive;
end;

function TMethodParameter.GetDeclaration: String;
begin
  Result := FDirective.ToDeclaration;
  Result := Result + ifthen(Result <> '', ' ') + FParameterName + ': ' + FParameterType;
end;

{ TMethodParameterList }

function TMethodParameterList.FindParameter(pParameterName: String): TMethodParameter;
var
  lParameter: TMethodParameter;
begin
  Result := nil;
  for lParameter in Self do
  begin
    if SameText(lParameter.ParameterName, pParameterName) then
    begin
      Result := lParameter;
      Break;
    end;
  end;
end;

end.