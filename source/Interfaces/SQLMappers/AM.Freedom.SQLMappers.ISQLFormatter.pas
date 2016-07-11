unit AM.Freedom.SQLMappers.ISQLFormatter;

interface

uses
  System.Classes,
  AM.Freedom.EnumerationTypes;

type
  ISQLFormatter = interface
  ['{E685D96C-5CA9-4E3B-B786-92C57FF36F9F}']
    function GetCodeWordCharCase: TCharCase;
    function GetKeyWordCharCase: TCharCase;
    procedure SetCodeWordCharCase(const pCharCase: TCharCase);
    procedure SetKeyWordCharCase(const pCharCase: TCharCase);

    function FormatSQLText(const pSQL: String; pKeyWords: TStrings): string;

    property CodeWordCharCase: TCharCase read GetCodeWordCharCase write SetCodeWordCharCase;
    property KeyWordCharCase: TCharCase read GetKeyWordCharCase write SetKeyWordCharCase;
  end;

implementation

end.
