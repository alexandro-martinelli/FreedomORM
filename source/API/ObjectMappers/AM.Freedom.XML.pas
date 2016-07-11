unit AM.Freedom.XML;

interface

uses
  Xml.xmldom,
  Xml.XMLIntf,
  Xml.XMLDoc,
  System.SysUtils,
  System.Classes,
  AM.Freedom.IXML;


type
  TXML = class(TInterfacedObject, IXML)
  strict private
    FXMLDocument: TXMLDocument;
    function GetRoot: IXMLNode;
    procedure SetRoot(const pNode: IXMLNode);
    function GetXMLText: String;
    procedure ActivateXML;
  public
    constructor Create;
    destructor Destroy; override;
    function AddChild(const pTagName: String): IXMLNode; overload;
    function AddChild(const pTagName, pNamespaceURI: String): IXMLNode; overload;
    function IsEmpty: Boolean;
    procedure LoadFromFile(const pFileName: String = ''); overload;
    procedure LoadFromStream(const pStream: TStream); overload;
    procedure LoadFromXML(const pXML: String); overload;
    procedure SaveToFile(const pFileName: String); overload;
    procedure SaveToStream(const pStream: TStream); overload;
    procedure SaveToXML(var pXML: String); overload;
    property Root: IXMLNode read GetRoot write SetRoot;
    property XMLText: String read GetXMLText;
  end;

implementation

{ TXML }

procedure TXML.ActivateXML;
begin
  if (not FXMLDocument.Active) then
  begin
    FXMLDocument.Active := True;
  end;
end;

function TXML.AddChild(const pTagName, pNamespaceURI: String): IXMLNode;
begin
  ActivateXML;
  Result := FXMLDocument.AddChild(pTagName, pNamespaceURI);
end;

constructor TXML.Create;
begin
  FXMLDocument := TXMLDocument.Create(nil);
end;

destructor TXML.Destroy;
begin
  FXMLDocument.Free;
  inherited;
end;

function TXML.AddChild(const pTagName: String): IXMLNode;
begin
  ActivateXML;
  Result := FXMLDocument.AddChild(pTagName);
end;

function TXML.GetRoot: IXMLNode;
begin
  ActivateXML;
  Result := FXMLDocument.DocumentElement;
end;

function TXML.GetXMLText: String;
begin
  ActivateXML;
  Result := FXMLDocument.XML.Text;
end;

function TXML.IsEmpty: Boolean;
begin
  ActivateXML;
  Result := FXMLDocument.IsEmptyDoc;
end;

procedure TXML.LoadFromFile(const pFileName: String);
begin
  ActivateXML;
  FXMLDocument.LoadFromFile(pFileName);
end;

procedure TXML.LoadFromStream(const pStream: TStream);
begin
  ActivateXML;
  FXMLDocument.LoadFromStream(pStream);
end;

procedure TXML.LoadFromXML(const pXML: String);
begin
  ActivateXML;
  FXMLDocument.LoadFromXML(pXML);
end;

procedure TXML.SaveToFile(const pFileName: String);
begin
  ActivateXML;
  FXMLDocument.SaveToFile(pFileName);
end;

procedure TXML.SaveToStream(const pStream: TStream);
begin
  ActivateXML;
  FXMLDocument.SaveToStream(pStream);
end;

procedure TXML.SaveToXML(var pXML: String);
begin
  ActivateXML;
  FXMLDocument.SaveToXML(pXML);
end;

procedure TXML.SetRoot(const pNode: IXMLNode);
begin
  ActivateXML;
  FXMLDocument.DocumentElement := pNode;
end;

end.
