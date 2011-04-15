(*******************************************************************************
** File Name: gtxml.pas
** Last update: 2009-10-21
This file is part of gtcore of gametoolsetpkg.

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms
of the GNU General Public license (the  "GPL License"), in which case the
provisions of GPL License are applicable instead of those
above.

For feedback and questions about this file or the project in its whole
please mail one of its authors:
  j.wielicki@sotecware.net (Horazont)
  jakob@yazo-games.de (Wilson)
*******************************************************************************)
unit GTXML;

{$mode objfpc}{$H+}
{$PIC ON}

interface

uses
  Classes, SysUtils, libxml2, fgl;
type
  TxmlString = {$IFNDEF Unicode}String{$ELSE}WideString{$ENDIF};

  TxmlNodeType = (ntElement, ntText, ntComment, ntCDATA, ntOther);

  TxmlParserFlag = (pfValidateDTD);
  TxmlParserFlags = set of TxmlParserFlag;

  TxmlAttribute = class;
  TxmlNode = class;
  TxmlDocument = class;

  { ExmlParserError }

  ExmlParserError = class (Exception)
  public
    constructor Create(const ParserError: String; ParserErrno, Line: Integer);
  private
    FParserError: String;
    FLine: Integer;
  public
    property ParserError: String read FParserError;
    property Line: Integer read FLine;
  end;

  ExmlValidityError = class (Exception);

  { TxmlAttribute }

  TxmlAttribute = class (TObject)
    constructor Create(AParent: TxmlNode;AOrigAttr:xmlAttrPtr);
    destructor Destroy; override;
  private
    FParent: TxmlNode;
    FOrigAttr: xmlAttrPtr;
    function GetContent: TxmlString;
    function GetName: TxmlString;
    procedure SetContent(const AValue: TxmlString);
  public
    property Parent: TxmlNode read FParent;
    property Name: TxmlString read GetName;
    property Content: TxmlString read GetContent write SetContent;
  end;

  TxmlAttribList = specialize TFPGList<TxmlAttribute>;
  TxmlNodeList = specialize TFPGList<TxmlNode>;
  TxmlNodeArray = array of TxmlNode;

  { TxmlNode }

  TxmlNode = class (TObject)
    constructor Create(ADocument: TxmlDocument;AParent: TxmlNode; AOrigNode: xmlNodePtr);
    destructor Destroy; override;
  private
    FDocument: TxmlDocument;
    FOrigNode: xmlNodePtr;
    FParent: TxmlNode;
    FOrigNodeOwned: Boolean;
    FType: TxmlNodeType;
    function GetHasCDATA: Boolean;
    function getName: TxmlString;
    function GetNode(Name: TxmlString): TxmlNode;
    function GetNodeAC(Name: TxmlString): TxmlNode;
    function GetNodes(Name: TxmlString): TxmlNodeArray;
    procedure SetAttribute(Name : TxmlString; const AValue: TxmlString);
    procedure SetAttributeBoolean(Name : TxmlString; const AValue: Boolean);
    procedure SetAttributeDouble(Name : TxmlString; const AValue: Double);
    procedure SetAttributeInteger(Name : TxmlString; const AValue: Integer);
    procedure SetContent(const AValue: TxmlString);
    procedure SetContentBoolean(const AValue: Boolean);
    procedure SetContentDouble(const AValue: Double);
    procedure SetContentInteger(const AValue: Integer);
    procedure SetHasCDATA(const AValue: Boolean);
    procedure setName(const AValue: TxmlString);
    procedure ReadChildren;
    procedure DeleteChildren;
    procedure SetNodeValue(Name: TxmlString; const AValue: TxmlString);
    procedure SetNodeValueBoolean(Name: TxmlString; const AValue: Boolean);
    procedure SetNodeValueDouble(Name: TxmlString; const AValue: Double);
    procedure SetNodeValueInteger(Name: TxmlString; const AValue: Integer);
  protected
    FChildren : TXmlNodeList;
    FAttributes : TxmlAttribList;
    FShallHaveCDATA: Boolean; //True if currently empty content, but new content shall use CDATA
  public
    function getContent: TxmlString;
    function getContentBoolean(Default:Boolean): Boolean;overload;
    function getContentDouble(Default:Double): Double;overload;
    function getContentInteger(Default:Integer): Integer; overload;
    function getContentBoolean: Boolean; overload;
    function getContentDouble: Double; overload;
    function getContentInteger: Integer;overload;

    function GetNodeValue(Name: TxmlString): TxmlString;
    function GetNodeValueBoolean(Name: TxmlString;Default:Boolean): Boolean;overload;
    function GetNodeValueDouble(Name: TxmlString;Default:Double): Double;overload;
    function GetNodeValueInteger(Name: TxmlString;Default:Integer): Integer;overload;
    function GetNodeValueBoolean(Name: TxmlString): Boolean; overload;
    function GetNodeValueDouble(Name: TxmlString): Double;overload;
    function GetNodeValueInteger(Name: TxmlString): Integer;overload;

    function GetAttribute(Name : TxmlString): TxmlString;
    function GetAttributeBoolean(Name : TxmlString;Default:Boolean): Boolean;overload;
    function GetAttributeDouble(Name : TxmlString;Default:Double): Double; overload;
    function GetAttributeInteger(Name : TxmlString;Default:Integer): Integer; overload;
    function GetAttributeBoolean(Name : TxmlString): Boolean; overload;
    function GetAttributeDouble(Name : TxmlString): Double; overload;
    function GetAttributeInteger(Name : TxmlString): Integer; overload;
  public
    property Parent: TxmlNode read FParent;
    property Name: TxmlString read getName write setName;
    property Node[Name:TxmlString]: TxmlNode read GetNode; default; //Gets only first with given name
    property NodeAC[Name:TxmlString]: TxmlNode read GetNodeAC; //Gets only first with given name, Autocreates if not exists
    property Nodes[Name:TxmlString]: TxmlNodeArray read GetNodes; //Gets all subnodes with given name
    property Children : TxmlNodeList read FChildren;
    property Document: TxmlDocument read FDocument;
    property Attribute[Name : TxmlString]: TxmlString read GetAttribute write SetAttribute;
    property AttributeDouble[Name : TxmlString]: Double read GetAttributeDouble write SetAttributeDouble;
    property AttributeInteger[Name : TxmlString]: Integer read GetAttributeInteger write SetAttributeInteger;
    property AttributeBoolean[Name : TxmlString]: Boolean read GetAttributeBoolean write SetAttributeBoolean;
    property NodeValue[Name: TxmlString]: TxmlString read GetNodeValue write SetNodeValue;
    property NodeValueDouble[Name: TxmlString]: Double read GetNodeValueDouble write SetNodeValueDouble;
    property NodeValueInteger[Name: TxmlString]: Integer read GetNodeValueInteger write SetNodeValueInteger;
    property NodeValueBoolean[Name: TxmlString]: Boolean read GetNodeValueBoolean write SetNodeValueBoolean;
    property Attributes : TxmlAttribList read FAttributes;
    property NodeType: TxmlNodeType read FType;
    property HasCData: Boolean read GetHasCDATA write SetHasCDATA;


    property OrigNode: xmlNodePtr read FOrigNode;

    property Content: TxmlString read getContent write SetContent;
    property ContentDouble:Double read getContentDouble write SetContentDouble;
    property ContentInteger:Integer read getContentInteger write SetContentInteger;
    property ContentBoolean:Boolean read getContentBoolean write SetContentBoolean;

    function AddNode(AName:TxmlString):TxmlNode;
    procedure Delete;

  end;

  { TxmlDocument }

  TxmlDocument = class
  private
    FRootNode: TxmlNode;
    FOrigDoc: xmlDocPtr;
    function GetAsString: String;
    procedure SetAsString(const AValue: String);
  public
    property RootNode:TxmlNode read FRootNode;
    procedure LoadFromStream(Stream:TStream; ParserFlags: TxmlParserFlags = []);
    procedure LoadFromFile(Filename:String; ParserFlags: TxmlParserFlags = []);
    procedure SaveToStream(Stream:TStream);
    procedure SaveToFile(Filename:String);
    property AsString:String read GetAsString write SetAsString;
    constructor Create;
    destructor Destroy; override;
  end;

function xmlNodeTypeToString(nodeType:TxmlNodeType):String;


implementation

uses Math;

{ ExmlParserError }

constructor ExmlParserError.Create(const ParserError: String; ParserErrno,
  Line: Integer);
begin
  inherited CreateFmt('XML parser error %d: %s at line %d.', [ParserErrno, ParserError, Line]);
end;

{ TxmlAttribute }

constructor TxmlAttribute.Create(AParent: TxmlNode;AOrigAttr:xmlAttrPtr);
begin
  FParent:=AParent;
  FOrigAttr:=AOrigAttr;
end;

destructor TxmlAttribute.Destroy;
begin
  inherited Destroy;
end;

function TxmlAttribute.GetContent: TxmlString;
begin
  result:=FOrigAttr^.children^.content;
end;

function TxmlAttribute.GetName: TxmlString;
begin
  result:= FOrigAttr^.name;
end;

procedure TxmlAttribute.SetContent(const AValue: TxmlString);
begin
  xmlFree(FOrigAttr^.children^.content);
  FOrigAttr^.children^.content := xmlStrdup(PChar(AValue));
end;

{ TxmlNode }

constructor TxmlNode.Create(ADocument: TxmlDocument; AParent: TxmlNode; AOrigNode: xmlNodePtr);
var
  curAttr:xmlAttrPtr;
begin
  if AOrigNode = nil then raise Exception.Create('nil node!');
  FShallHaveCDATA:=false;
  FDocument:=ADocument;
  FOrigNode:=AOrigNode;
  FOrigNodeOwned := False;
  FParent:=AParent;
  FAttributes:=TxmlAttribList.Create;
  FChildren:=TxmlNodeList.Create;
  case FOrigNode^.type_ of
    XML_ELEMENT_NODE: FType := ntElement;
    XML_TEXT_NODE: FType := ntText;
    XML_CDATA_SECTION_NODE: FType := ntCDATA;
    XML_COMMENT_NODE: FType := ntComment;
  else
    FType := ntOther;
    raise Exception.Create(format('Unknown node type for node %s (%d).',[FOrigNode^.name, FOrigNode^.type_]));
  end;
  //Read Attributes
  curAttr:=FOrigNode^.properties;
  while assigned(curAttr) do
  begin
    if assigned(curAttr^.children) then
      FAttributes.Add(TxmlAttribute.Create(self,curAttr));

    CurAttr := CurAttr^.next;
  end;
  ReadChildren;
end;

destructor TxmlNode.Destroy;
var
  AAttribute: TxmlAttribute;
  AChild: TxmlNode;
begin
(*  if FOrigNode <> nil then
  begin
    xmlUnlinkNode(FOrigNode);
    xmlFreeNode(FOrigNode);
  end; *)
  for AAttribute in FAttributes do
  begin
    AAttribute.Free;
  end;
  for AChild in FChildren do
  begin
    AChild.Free;
  end;
  FAttributes.Free;
  FChildren.Free;
  inherited Destroy;
end;

function TxmlNode.GetAttribute(Name : TxmlString): TxmlString;
var
  lowerName:TxmlString;
  i:Integer;
begin
  lowerName:=lowerCase(name);
  result:='';
  for i:=0 to Attributes.Count-1 do
  begin
    if lowercase(Attributes[i].Name)=lowerName then
    begin
      result:=Attributes[i].Content;
      Break;
    end;
  end;
end;



function TxmlNode.GetAttributeBoolean(Name : TxmlString;Default: Boolean): Boolean;
var
  cont:String;
begin
  cont:=GetAttribute(name);
  if cont='' then
    result:=default
  else
    result:=StrToBool(cont);
end;

function TxmlNode.GetAttributeDouble(Name: TxmlString; Default: Double
  ): Double;
var
  sep:Char;
begin
  sep:=DecimalSeparator;
  DecimalSeparator:='.';
  if not TryStrToFloat(GetAttribute(Name), Result) then
    Result := Default;
  decimalSeparator:=sep;
end;

function TxmlNode.GetAttributeInteger(Name: TxmlString; Default: Integer
  ): Integer;
begin
  if not TryStrToInt(GetAttribute(Name), Result) then
    Result := Default;
end;

function TxmlNode.GetAttributeBoolean(Name: TxmlString): Boolean;
begin
  result:=GetAttributeBoolean(Name,False);
end;

function TxmlNode.GetAttributeDouble(Name : TxmlString): Double;
begin
  result:=GetAttributeDouble(Name,Nan);
end;

function TxmlNode.GetAttributeInteger(Name : TxmlString): Integer;
begin
  result:=GetAttributeInteger(Name,0);
end;

function TxmlNode.getContent: TxmlString;
var
  Tmp: xmlCharPtr;
begin
  Tmp := xmlNodeGetContent(FOrigNode);
  result := Tmp;
  xmlFree(Tmp);
end;

function TxmlNode.getContentBoolean: Boolean;
begin
  result:=GetContentBoolean(false);
end;

function TxmlNode.getContentDouble: Double;
begin
  result:=GetContentDouble(Nan);
end;

function TxmlNode.getContentInteger: Integer;
begin
  result:=GetContentInteger(0);
end;

function TxmlNode.getContentBoolean(Default: Boolean): Boolean;
var
  cont:String;
begin
  cont:=GetContent;
  if cont='' then
    result:=default
  else
    result:=StrToBool(cont);
end;

function TxmlNode.getContentDouble(Default: Double): Double;
var
  sep:Char;
begin
  sep:=DecimalSeparator;
  DecimalSeparator:='.';
  if not TryStrToFloat(GetContent, Result) then
    result:=NAN;
  decimalSeparator:=sep;
end;

function TxmlNode.getContentInteger(Default: Integer): Integer;
begin
  if not TryStrToInt(GetContent, Result) then
    result:=0;
end;

function TxmlNode.GetHasCDATA: Boolean;
var
  c:Integer;
begin
  Result:= FShallHaveCDATA;
  c:=children.count;
  if (not result) and (c = 1) then
    result:=children[0].nodetype = ntCDATA;
end;

function TxmlNode.getName: TxmlString;
begin
  result:=FOrigNode^.name;
end;

function TxmlNode.GetNode(Name: TxmlString): TxmlNode;
var
  lowerName:TxmlString;
  i:Integer;
begin
  lowerName:=lowerCase(name);
  result:=nil;
  for i:=0 to FChildren.Count-1 do
  begin
    if lowercase(FChildren[i].Name)=lowerName then
    begin
      result:=FChildren[i];
      Break;
    end;
  end;
end;

function TxmlNode.GetNodeAC(Name: TxmlString): TxmlNode;
begin
  result:=GetNode(Name);
  if not assigned(result) then
    result:=AddNode(Name);
end;

function TxmlNode.GetNodes(Name: TxmlString): TxmlNodeArray;
var
  lowerName:TxmlString;
  i,count:Integer;
begin
  lowerName:=lowerCase(name);
  count:=0;
  SetLength(result,1);
  for i:=0 to FChildren.Count-1 do
  begin
    if lowercase(FChildren[i].Name)=lowerName then
    begin
      if length(result)<=count then
        setlength(result,length(result)*2);
      result[count]:=FChildren[i];
      inc(count);
    end;
  end;
  SetLength(result,count);
end;

function TxmlNode.GetNodeValue(Name: TxmlString): TxmlString;
var
  TmpNode: TxmlNode;
begin
  TmpNode := GetNode(Name);
  if TmpNode = nil then
    Result := ''
  else
    Result := TmpNode.Content;
end;

function TxmlNode.GetNodeValueBoolean(Name: TxmlString; Default: Boolean
  ): Boolean;
var
  TmpNode: TxmlNode;
begin
  TmpNode := GetNode(Name);
  if TmpNode = nil then
    Result := Default
  else
    Result := TmpNode.getContentBoolean(Default);
end;

function TxmlNode.GetNodeValueDouble(Name: TxmlString; Default: Double
  ): Double;
var
  TmpNode: TxmlNode;
begin
  TmpNode := GetNode(Name);
  if TmpNode = nil then
    Result := Default
  else
    Result := TmpNode.getContentDouble(Default);
end;

function TxmlNode.GetNodeValueInteger(Name: TxmlString; Default: Integer
  ): Integer;
var
  TmpNode: TxmlNode;
begin
  TmpNode := GetNode(Name);
  if TmpNode = nil then
    Result := Default
  else
    Result := TmpNode.getContentInteger(Default);
end;

function TxmlNode.GetNodeValueBoolean(Name: TxmlString): Boolean;
begin
  result:=GetNodeValueBoolean(Name,False);
end;

function TxmlNode.GetNodeValueDouble(Name: TxmlString): Double;
begin
  result:=GetNodeValueDouble(Name,Nan);
end;

function TxmlNode.GetNodeValueInteger(Name: TxmlString): Integer;
begin
  result:=GetNodeValueInteger(Name,0);
end;


procedure TxmlNode.SetAttribute(Name : TxmlString; const AValue: TxmlString);
var
  lowerName:TxmlString;
  found:Boolean;
  newattr:xmlAttrPtr;
  i:Integer;
begin
  found:=false;
  lowerName:=lowerCase(name);
  assert(assigned(attributes));
  for i:=0 to Attributes.Count-1 do
  begin
    if lowercase(Attributes[i].Name)=lowerName then
    begin
      Attributes[i].Content:=AValue;
      found:=true;
      Break;
    end;
  end;
  if not found then
  begin
    newattr := xmlNewProp (FOrigNode, PChar(Name), PChar(AValue));
    Attributes.Add(TxmlAttribute.Create(self,newattr));
  end;
end;

procedure TxmlNode.SetAttributeBoolean(Name : TxmlString; const AValue: Boolean);
begin
  SetAttribute(Name,BoolToStr(AValue));
end;

procedure TxmlNode.SetAttributeDouble(Name : TxmlString; const AValue: Double);
begin
  SetAttribute(Name,FloatToStr(AValue));
end;

procedure TxmlNode.SetAttributeInteger(Name : TxmlString; const AValue: Integer);
begin
  SetAttribute(Name,IntToStr(AValue));
end;

procedure TxmlNode.SetContent(const AValue: TxmlString);
var
  n:xmlNodePtr;
  i:Integer;
begin
  if HasCdata then
  begin
    if FShallHaveCDATA then
    begin
      n:=xmlNewCDataBlock(Document.FOrigDoc,xmlStrdup(PChar(AValue)),length(AValue));
      xmlAddChild(FOrigNode,n);
      ReadChildren;
    end else
    begin
      xmlNodeSetContentLen(FOrigNode^.children,xmlStrdup(PChar(AValue)),length(AValue));
    end;
    FShallHaveCDATA:=length(AValue)=0;
    if FShallHaveCDATA then
    begin
      DeleteChildren;
    end;
  end
  else
    xmlNodeSetContentLen(FOrigNode,PChar(AValue),length(AValue));
end;

procedure TxmlNode.SetContentBoolean(const AValue: Boolean);
begin
  SetContent(BoolToStr(AValue));
end;

procedure TxmlNode.SetContentDouble(const AValue: Double);
var
  sep:Char;
begin
  sep:=DecimalSeparator;
  DecimalSeparator:='.';
  SetContent(FloatToStr(AValue));
  DecimalSeparator:=sep;
end;

procedure TxmlNode.SetContentInteger(const AValue: Integer);
begin
  SetContent(IntToStr(AValue));
end;

procedure TxmlNode.SetHasCDATA(const AValue: Boolean);
var
  c:String;
  n,curChild:xmlNodePtr;
  i:Integer;
begin
  c:=xmlNodeGetContent(FOrigNode);
  if AValue then
  begin
    if length(c)>0 then
    begin
      xmlNodeSetContent(FOrigNode,nil);
      DeleteChildren;
      n:=xmlNewCDataBlock(Document.FOrigDoc,xmlStrdup(PChar(c)),length(c));
      xmlAddChild(FOrigNode,n);
      ReadChildren;
    end else begin
      DeleteChildren;
      FShallHaveCDATA:=true;
    end;
  end else begin
    FShallHaveCData:=false;
    DeleteChildren;
    xmlNodeSetContent(FOrigNode,PChar(c));
    ReadChildren;
  end;
end;

procedure TxmlNode.setName(const AValue: TxmlString);
begin
  xmlFree(FOrigNode^.name);
  FOrigNode^.name:=xmlStrdup(PChar(AValue));
end;

procedure TxmlNode.ReadChildren;
var
  curChild:xmlNodePtr;
begin
  curChild:=FOrigNode^.Children;
  while assigned(curChild) do
  begin
    FChildren.Add(TxmlNode.Create(FDocument,self,curChild));
    curChild:=curChild^.next;
  end;
end;

procedure TxmlNode.DeleteChildren;
var
  i:Integer;
begin
  for i:=0 to children.count-1 do
    children[i].delete;
  children.clear;
end;

procedure TxmlNode.SetNodeValue(Name: TxmlString; const AValue: TxmlString);
begin
  NodeAC[Name].Content := AValue;
end;

procedure TxmlNode.SetNodeValueBoolean(Name: TxmlString; const AValue: Boolean);
begin
  NodeAC[Name].ContentBoolean := AValue;
end;

procedure TxmlNode.SetNodeValueDouble(Name: TxmlString; const AValue: Double);
begin
  NodeAC[Name].ContentDouble := AValue;
end;

procedure TxmlNode.SetNodeValueInteger(Name: TxmlString; const AValue: Integer);
begin
  NodeAC[Name].ContentInteger := AValue;
end;

function TxmlNode.AddNode(AName: TxmlString): TxmlNode;
var
  n:xmlNodePtr;
begin
  n:=xmlNewNode(nil,PChar(AName));
  xmlAddChild(FOrigNode,n);
  result:=TxmlNode.Create(Document,self,n);
  FChildren.Add(result);
end;

procedure TxmlNode.Delete;
var
  i:Integer;
begin
  for i:=0 to Children.count-1 do
    children[i].delete;
  xmlUnlinkNode(FOrigNode);
  xmlFreeNode(FOrigNode);
  Free;
end;

{ TxmlDocument }

function TxmlDocument.GetAsString: String;
var
  buff:xmlCharPtr;
  len:Integer;
begin
  xmlDocDumpFormatMemory(FOrigDoc,@buff,@len,1);
  result:=buff;
end;

procedure TxmlDocument.SetAsString(const AValue: String);
var
  stream:TStringStream;
begin
  stream:=TStringStream.Create(AValue);
  try
  LoadFromStream(stream);
  finally
    stream.free;
  end;
end;

procedure TxmlDocument.LoadFromStream(Stream: TStream; ParserFlags: TxmlParserFlags);
var
  Data: Pointer;
  Ctxt: xmlParserCtxtPtr;
  Read: Integer;
  Flags: Integer;
  Validator: xmlValidCtxtPtr;
begin
  if Stream = nil then
    Exit;

  if FRootNode <> nil then
    FRootNode.Free;
  xmlFreeDoc(FOrigDoc); //Free the old Doc, since we create a new one

  GetMem(Data, Stream.Size+1);
  try
    Read := Stream.Read(Data^, Stream.Size);
    FillByte((Data+Read)^, 1, 0);

    Ctxt := xmlNewParserCtxt;
    try
      FOrigDoc := nil;

      Flags := Integer(XML_PARSE_NONET) or Integer(XML_PARSE_RECOVER);
      FOrigDoc := xmlCtxtReadMemory(Ctxt, Data, Read+1, nil, nil, Flags);

      if (Ctxt^.errNo = 0) and (FOrigDoc <> nil) then
      begin
        if (pfValidateDTD in ParserFlags) then
        begin
          Validator := xmlNewValidCtxt();
          try
            if xmlValidateDocument(Validator, FOrigDoc) = 0 then
              raise ExmlValidityError.Create('XML not valid.');
          finally
            xmlFreeValidCtxt(Validator);
          end;
        end;
        FRootNode:=TxmlNode.Create(self,nil,xmlDocGetRootElement(FOrigDoc));
      end
      else
      begin
        raise ExmlParserError.Create(Trim(StrPas(Ctxt^.lastError.message)), Ctxt^.errNo, Ctxt^.lastError.line);
      end;
    finally
      xmlFreeParserCtxt(Ctxt);
    end;
  finally
    FreeMem(Data);
  end;
end;

procedure TxmlDocument.LoadFromFile(Filename: String; ParserFlags: TxmlParserFlags);
var
  fs:TFileStream;
begin
  fs:=TFileStream.Create(Utf8ToAnsi(filename),fmOpenRead);
  try
    LoadFromStream(fs, ParserFlags);
  finally
    fs.Free;
  end;
end;

procedure TxmlDocument.SaveToStream(Stream: TStream);
var
  buff:xmlCharPtr;
  len:Integer;
begin
  assert(assigned(FOrigDoc));
  assert(assigned(stream));
  xmlDocDumpFormatMemory(FOrigDoc,@buff,@len,1);
  Stream.Write(buff^,len);
  xmlFree(buff);
end;

procedure TxmlDocument.SaveToFile(Filename: String);
var
  fs:TFileStream;
begin
  fs:=TFileStream.Create(filename, fmCreate or fmOpenWrite);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

constructor TxmlDocument.Create;
var
  node:xmlNodePtr;
  error:xmlErrorPtr;
begin
  FOrigDoc:=xmlNewDoc('1.0');
  node:=xmlNewNode(nil,'root');
  xmlDocSetRootElement(FOrigDoc,node);
  FRootNode:=TxmlNode.Create(self,nil,node);
end;

destructor TxmlDocument.Destroy;
begin
  FRootNode.Free;
  xmlFreeDoc(FOrigDoc);
  inherited Destroy;
end;

function xmlNodeTypeToString(nodeType:TxmlNodeType):String;
begin
  case nodeType of
  ntElement: result:='Element';
  ntText: result:='Text';
  ntComment: result:='Comment';
  ntCDATA: result:='CDATA';
  ntOther: result:='Other';
  else result:='unknown';
  end;
end;

end.

