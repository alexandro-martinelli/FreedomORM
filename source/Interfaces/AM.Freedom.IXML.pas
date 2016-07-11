unit AM.Freedom.IXML;

interface

uses
  Xml.XMLIntf,
  System.Classes,
  System.SysUtils;

type
  IXML = interface
  ['{CA535BA3-0EA3-471B-B5D5-24E430A51D06}']
    function AddChild(const pTagName: String): IXMLNode; overload;
    function AddChild(const pTagName, pNamespaceURI: String): IXMLNode; overload;
    function IsEmpty: Boolean;
    procedure LoadFromFile(const pFileName: String = ''); overload;
    procedure LoadFromStream(const pStream: TStream); overload;
    procedure LoadFromXML(const pXML: String); overload;
    procedure SaveToFile(const pFileName: String); overload;
    procedure SaveToStream(const pStream: TStream); overload;
    procedure SaveToXML(var pXML: String); overload;
  end;

implementation

end.