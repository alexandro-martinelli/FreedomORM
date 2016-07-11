unit AM.Freedom.GeneratorTextList;

interface

uses
  System.Generics.Collections,
  AM.Freedom.TextGenerator.CustomTextGenerator,
  AM.Freedom.SQLMappers.CustomSQLMapper;

type
  TGeneratorTextItem = class sealed
  strict private
    FMetaClass: TClass;
    FTextGeneratorClass: TTextGeneratorClass;
    FSQLMapperClass: TSQLMapperClass;
  public
    constructor Create(const pMetaClass: TClass; const pTextGeneratorClass: TTextGeneratorClass;
        const pSQLMapperClass: TSQLMapperClass);
    property MetaClass: TClass read FMetaClass;
    property TextGeneratorClass: TTextGeneratorClass read FTextGeneratorClass;
    property SQLMapperClass: TSQLMapperClass read FSQLMapperClass;
  end;

  TGeneratorTextList = class(TObjectList<TGeneratorTextItem>)
  public
    function FindTextGeneratorClass(const pMetaClass: TClass; const pSQLMapperClass: TSQLMapperClass): TTextGeneratorClass;

  end;

implementation

{ TGeneratorTextItem }

constructor TGeneratorTextItem.Create(const pMetaClass: TClass; const pTextGeneratorClass: TTextGeneratorClass;
  const pSQLMapperClass: TSQLMapperClass);
begin
  FMetaClass := pMetaClass;
  FTextGeneratorClass := pTextGeneratorClass;
  FSQLMapperClass := pSQLMapperClass;
end;

{ TGeneratorTextList }

function TGeneratorTextList.FindTextGeneratorClass(const pMetaClass: TClass;
  const pSQLMapperClass: TSQLMapperClass): TTextGeneratorClass;
var
  lItem: TGeneratorTextItem;
begin
  Result := nil;
  for lItem in Self do
  begin
    if (lItem.MetaClass = pMetaClass) then
    begin
      if (lItem.SQLMapperClass = pSQLMapperClass) then
      begin
        Result := lItem.TextGeneratorClass;
        Break;
      end else if (lItem.SQLMapperClass = nil) then
      begin
        Result := lItem.TextGeneratorClass;
      end;
    end;
  end;
end;

end.
