unit AM.UnitReader.WordVerificator;

interface

uses
  AM.UnitReader.Enumerations,
  AM.UnitReader.PrefixOptions;

type
  TWordVerificator = class
  strict private
    FWordType: TWordType;
    FWord: String;
    FPrefixOptions: TPrefixOptions;
    FOnLogMessage: TOnLogMessage;
    FMinimunLength: Byte;

    procedure DoLogMessage(pLogMessage: string);
    function IsReservedWord: Boolean;
    function VerifyLength: Boolean;
    function VerifyPrefix: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Verify;
    property WordType: TWordType read FWordType write FWordType;
    property Word: String read FWord write FWord;
    property PrefixOptions: TPrefixOptions read FPrefixOptions write FPrefixOptions;
    property MinimunLength: Byte read FMinimunLength write FMinimunLength;
    property OnLogMessage: TOnLogMessage read FOnLogMessage write FOnLogMessage;
  end;

implementation

uses
  System.StrUtils,
  AM.UnitReader.Helper.WordType,
  System.SysUtils;

{TWordVerificator}

constructor TWordVerificator.Create;
begin
  FPrefixOptions := TPrefixOptions.Create;
end;

destructor TWordVerificator.Destroy;
begin
  FPrefixOptions.Free;
  inherited;
end;

procedure TWordVerificator.DoLogMessage(pLogMessage: string);
begin
  if Assigned(FOnLogMessage) then
  begin
    FOnLogMessage(pLogMessage);
  end;
end;

function TWordVerificator.VerifyLength: Boolean;
begin
  Result := Length(FWord) < FMinimunLength;
  if not Result then
  begin
    DoLogMessage(Format('Name ''%s'' too short for %s name', [FWord, FWordType.ToString]));
  end;
end;

function TWordVerificator.VerifyPrefix: Boolean;
var
  lPrefix: Char;
begin
  Result := True;
  lPrefix := '.';
  case FWordType of
    wtParameter: lPrefix := FPrefixOptions.Parameters;
    wtVariable: lPrefix := FPrefixOptions.Variables;
    wtConstant: lPrefix := FPrefixOptions.Constants;
  end;
  if lPrefix <> '.' then
  begin
    Result := StartsStr(FWord, lPrefix);
    if not Result then
    begin
      DoLogMessage(Format('Prefix %s not found in %s: %s', [lPrefix, FWordType.ToString, FWord]));
    end;
  end;
end;

procedure TWordVerificator.Verify;
begin
  if not IsReservedWord then
  begin
    if VerifyLength then
    begin
      VerifyPrefix;
    end;
  end;
end;

function TWordVerificator.IsReservedWord: Boolean;
const
  cReservedWords: string = 'Array;of;function;procedure;if;then;begin;end;case;to;downto;do;while;for;const;var;not;and;class;' +
      'record;repeat;until;unit;uses;implementation;interface;in;is;as;or;type;strict;private;protected;public;published' +
      'initialization;finalization;object';
begin
  Result := ContainsText(cReservedWords, FWord);
end;


end.
