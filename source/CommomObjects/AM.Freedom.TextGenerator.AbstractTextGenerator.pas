unit AM.Freedom.TextGenerator.AbstractTextGenerator;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  AM.Freedom.TextGenerator.GenerateTextParams,
  AM.Freedom.SQLMappers.ISQLMapper, AM.Freedom.DefaultsClassRegister;

type
  EUnregisteredClassTextGenerator = class(Exception)
  public
    constructor Create(pClassName: String); reintroduce;
  end;

  TAbstractTextGenerator = class abstract(TInterfacedObject)
  strict private
    FISQLMapper: ISQLMapper;
  strict protected
    function GetTextFromGenerator(pObject: TObject; pParams: TGenerateTextParams = nil): String;
    procedure FormatSQLText(var pStr: String);
    function GetSQLMapper: ISQLMapper;
  public
    constructor Create(pISQLMapper: ISQLMapper = nil); virtual;
  end;

implementation

uses
  AM.Freedom.TextGeneratorRegister,
  AM.Freedom.TextGenerator.CustomTextGenerator;

{ TCustomTextGenerator }

constructor TAbstractTextGenerator.Create(pISQLMapper: ISQLMapper);
begin
  FISQLMapper := pISQLMapper;
end;

function TAbstractTextGenerator.GetTextFromGenerator(pObject: TObject; pParams: TGenerateTextParams): String;
var
  lClass: TTextGeneratorClass;
  lGenerator: TCustomTextGenerator;
begin
  Result := EmptyStr;
  if Assigned(pObject) then
  begin
    lClass := TTextGeneratorRegister.FindTextGenerator(pObject.ClassType);
    if Assigned(lClass) then
    begin
      lGenerator := lClass.Create(FISQLMapper);
      try
        Result := lGenerator.GenerateText(pObject, pParams);
      finally
        FreeAndNil(lGenerator);
        FreeAndNil(pParams);
      end;
    end else
    begin
      raise EUnregisteredClassTextGenerator.Create(pObject.ClassName);
    end;
  end;
end;

procedure TAbstractTextGenerator.FormatSQLText(var pStr: String);
begin
  if GetSQLMapper <> nil then
  begin
    GetSQLMapper.FormatSQLText(pStr);
  end;
end;

function TAbstractTextGenerator.GetSQLMapper: ISQLMapper;
begin
  Result := FISQLMapper;
  if not Assigned(Result) then
  begin
    Result := TDefaultsClassRegister.DefaultSQLMapper;
  end;
end;

{ EUnregisteredClassTextGenerator }

constructor EUnregisteredClassTextGenerator.Create(pClassName: String);
begin
  inherited Create('Unregistered class text generator for class: ' + pClassName);
end;

end.
