unit UnitReader.Elements;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  UnitReader.EnumerationTypes;

type
  TElementClass = class of TLineElement;

  TLines = class;

  TLineElement = class
  private
    FTokenType: TTokenType;
    FLineNumber: Integer;
    FText: String;
    FParent: TLineElement;
  protected
    function GetElementType: TElementType; virtual;
  public
    constructor Create; virtual;
    procedure Assign(pSource: TLineElement); virtual;
    function CreateNew: TLineElement;
    property TokenType: TTokenType read FTokenType write FTokenType default tkUnknow;
    property LineNumber: Integer read FLineNumber write FLineNumber default -1;
    property Text: String read FText write FText;
    property Parent: TLineElement read FParent write FParent;
    property ElementType: TElementType read GetElementType;
  end;

  TLines = class(TObjectList<TLineElement>)
  public
    function LineByLineNumber(pLineNumber: Integer): TLineElement;
    procedure AddLine(pLine: TLineElement);
  end;

  TBlockElement = class(TLineElement)
  private
    FBlock: TStrings;
    FFinalized: Boolean;
    FLines: TLines;
    function GetBlock: TStrings;
    procedure UpdateBlock;
    function GetCompleteDeclaration: TStrings;
    function GetFinalized: Boolean;
  protected
    function GetElementType: TElementType; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    function AddLine: TLineElement; overload;
    procedure AddLine(pLine: TLineElement); overload;
    function AddBlock: TBlockElement; overload;
    procedure AddBlock(pBlock: TBlockElement); overload;

    property Block: TStrings read GetBlock;
    property Finalized: Boolean read GetFinalized write FFinalized;
    property Lines: TLines read FLines;
    property CompleteDeclaration: TStrings read GetCompleteDeclaration;
  end;

const
  TTokenTypeElement: array [TTokenType] of TElementClass = (TLineElement, TBlockElement, TBlockElement, TBlockElement,
      TBlockElement, TBlockElement, TBlockElement, TBlockElement, TLineElement, TLineElement, TBlockElement,
      TBlockElement, TLineElement, TLineElement, TBlockElement, TBlockElement, TBlockElement, TBlockElement, TBlockElement,
      TBlockElement, TBlockElement, TBlockElement, TBlockElement, TBlockElement, TBlockElement, TBlockElement,
      TBlockElement, TBlockElement, TBlockElement, TBlockElement, TBlockElement, TBlockElement,
      TBlockElement, TLineElement, TLineElement, TLineElement, TLineElement, TLineElement, TLineElement, TLineElement,
      TLineElement, TLineElement, TLineElement, TBlockElement, TBlockElement, TLineElement);

implementation

uses
  System.IOUtils,
  System.StrUtils;


{ TLineElement }

procedure TLineElement.Assign(pSource: TLineElement);
begin
  FTokenType := pSource.TokenType;
  FLineNumber := pSource.LineNumber;
  FText := pSource.Text;
  FParent := pSource.Parent;
end;

constructor TLineElement.Create;
begin
  FLineNumber := -1;
  FTokenType := tkUnknow;
end;

function TLineElement.CreateNew: TLineElement;
begin
  Result := TLineElement.Create;
  Result.Assign(Self);
end;

function TLineElement.GetElementType: TElementType;
begin
  Result := etLine;
end;

{ TBlockElement }

function TBlockElement.AddLine: TLineElement;
begin
  Result := TLineElement.Create;
  AddLine(Result);
end;

function TBlockElement.AddBlock: TBlockElement;
begin
  Result := TBlockElement.Create;
  AddBlock(Result);
end;

procedure TBlockElement.AddBlock(pBlock: TBlockElement);
begin
  FLines.AddLine(pBlock);
end;

procedure TBlockElement.AddLine(pLine: TLineElement);
begin
  FLines.AddLine(pLine);
end;

procedure TBlockElement.UpdateBlock;
var
  lLine: TLineElement;
begin
  FBlock.Clear;
  for lLine in FLines do
  begin
    if (lLine is TLineElement) then
    begin
      FBlock.Add(lLine.Text);
    end
    else
    begin
      FBlock.Add(TBlockElement(lLine).Block.Text);
    end;
  end;
end;

procedure TBlockElement.Clear;
begin
  FLines.Clear;
  FBlock.Clear;
end;

constructor TBlockElement.Create;
begin
  inherited;
  FBlock := TStringList.Create;
  FLines := TLines.Create(False);
end;

destructor TBlockElement.Destroy;
begin
  FreeAndNil(FBlock);
  FreeAndNil(FLines);
  inherited;
end;

function TBlockElement.GetBlock: TStrings;
begin
  UpdateBlock;
  Result := FBlock;
end;

function TBlockElement.GetCompleteDeclaration: TStrings;
begin
  Result := TStringList.Create;
  Result.Add(Text);
  Result.AddStrings(GetBlock);
end;

function TBlockElement.GetElementType: TElementType;
begin
  Result := etBlock;
end;

function TBlockElement.GetFinalized: Boolean;
var
  lIndex: Integer;
begin
  Result := FFinalized;

  if (not Result) and (TokenType = tkClassDeclaration) then
  begin
    if (FLines.Count > 0) then
    begin
      Result := FLines.Items[FLines.Count - 1].TokenType = tkEnd;
      if (not Result) then
      begin
        lIndex := FLines.Count - 1;
        while (lIndex >= 0) and (not Result) do
        begin
          Result := FLines.Items[lIndex].TokenType = tkEnd;
          Dec(lIndex);
        end;
      end;
    end
    else
    begin
      Result := EndsText(';', Text);
    end;
  end;
  FFinalized := Result;
end;

{ TLines }

procedure TLines.AddLine(pLine: TLineElement);
begin
  Add(pLine);
end;

function TLines.LineByLineNumber(pLineNumber: Integer): TLineElement;
var
  lLine: TLineElement;
begin
  Result := nil;
  for lLine in Self do
  begin
    if lLine.LineNumber = pLineNumber then
    begin
      Result := lLine;
      Break;
    end;
  end;
end;

end.
