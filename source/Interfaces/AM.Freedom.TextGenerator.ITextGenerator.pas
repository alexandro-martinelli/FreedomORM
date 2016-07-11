unit AM.Freedom.TextGenerator.ITextGenerator;

interface

uses
  AM.Freedom.TextGenerator.GenerateTextParams;

type
  ITextGenerator = interface
  ['{AA439E56-7507-44E4-B503-E9800D19D2E3}']
    function GenerateText(pObject: TObject; pParams: TGenerateTextParams = nil): String;
  end;

implementation

end.
