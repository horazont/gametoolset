(*******************************************************************************
** File Name: gtbase.pas
This file is part of the Game Toolset Package.

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

For feedback and questions about Game Toolset please mail me,
Jonas Wielicki:
  j.wielicki@sotecware.net

CREDITS:
  the gtxml.pas is adapted from xml.pas by Wilson and me, see that file for
  further information.
*******************************************************************************)
unit GTBase;

{$mode objfpc}{$H+}
{$PIC ON}

interface

uses
  Classes, SysUtils, GTXML, typinfo, variants, contnrs, fgl, math, GTURI;

const
  GTACTION_RESET = 0;
  GTACTION_IMPORT = 1;
  GTACTION_LOAD = 2;
  GTACTION_SAVE = 3;
  GTACTION_EXPORT = 4;

  GTXML_NOPROP_KEY = 'noprop';

  NODEFAULT_VALUE = 2147483648; // took from ref.pdf

  XMLNumberFormatSettings : TFormatSettings = (
    CurrencyFormat: 1;
    NegCurrFormat: 5;
    ThousandSeparator: ',';
    DecimalSeparator: '.';
    CurrencyDecimals: 2;
    DateSeparator: '-';
    TimeSeparator: ':';
    ListSeparator: ',';
    CurrencyString: '$';
    ShortDateFormat: 'd/m/y';
    LongDateFormat: 'dd" "mmmm" "yyyy';
    TimeAMString: 'AM';
    TimePMString: 'PM';
    ShortTimeFormat: 'hh:nn';
    LongTimeFormat: 'hh:nn:ss';
    ShortMonthNames: ('Jan','Feb','Mar','Apr','May','Jun',
                      'Jul','Aug','Sep','Oct','Nov','Dec');
    LongMonthNames: ('January','February','March','April','May','June',
                     'July','August','September','October','November','December');
    ShortDayNames: ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
    LongDayNames:  ('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
    TwoDigitYearCenturyWindow: 50;
  );

const
  ReferenceBlockExp = 12;
  ReferenceBlockSize = 1 shl ReferenceBlockExp;

  NULL = 0;

type
  EGTError = class (Exception);

  EGTCancelProgress = class (Exception);

  EGTXMLError = class (EGTError);
    EGTXMLInvalidProperty = class (EGTXMLError);
  EGTContextError = class (EGTError);
  EGTReferenceError = class (EGTError);
  EGTCoreError = class (EGTError);

  TGTBaseEditable = class;
  TGTBaseObject = class;
  TGTBaseClass = class of TGTBaseObject;
  TGTTemplate = class;

  TGTAction = Integer;

  TGTNodeEvent = procedure (Sender: TObject; ANode: TGTBaseObject) of object;
  TGTPropChangeEvent = procedure (Sender: TObject; const PropertyName: String;
    PropInfo: PPropInfo; const OldValue, NewValue: Variant) of object;
  TGTOnProgressEvent = procedure (Sender: TObject; AAction: TGTAction;
    Current, Max: Integer) of object;
  TGTEvent = procedure (Sender: TObject) of object;
  TGTEditObject = procedure (Sender: TObject; ANode: TGTBaseEditable) of object;
  TGTInspectObject = procedure (Sender: TObject; ANode: TGTBaseObject) of object;

  IGTSpecialProgressCaption = interface ['{FBA8EFE8-2B1D-493A-91B5-23C52BDDE9D8}']
    function GetActionDescription: String;
  end;

  { TGTGenericEventList }

  generic TGTGenericEventList<EventType> = class (TObject)
  private type
    TGTGenericEventListEntry = record
      AHandler: EventType;
    end;
    PGTGenericEventListEntry = ^TGTGenericEventListEntry;
  var public
    constructor Create;
    destructor Destroy; override;
  protected
    FCalling: Integer;
    FDeferredDeletes: TFPList;
    FList: TFPList;
    FLocked: Integer;
  protected
    procedure BeginCall;
    procedure EndCall;
    function EqualMethodPointers(const Method1, Method2: TMethod): Boolean;
    function LookupHandler(AHandler: EventType): Integer;
  public
    procedure BeginLock;
    procedure Clear;
    procedure EndLock;
    procedure RegisterHandler(AHandler: EventType);
    procedure UnRegisterHandler(AHandler: EventType);
  end;

  { TGTEventList }

  TGTEventListSpecialized = specialize TGTGenericEventList<TGTEvent>;
  TGTEventList = class (TGTEventListSpecialized)
  public
    procedure Call(Sender: TObject);
  end;

  { TGTPropChangeEventList }

  TGTPropChangeEventListSpecialized = specialize TGTGenericEventList<TGTPropChangeEvent>;
  TGTPropChangeEventList = class (TGTPropChangeEventListSpecialized)
  public
    procedure Call(Sender: TObject; PropertyName: String; PropInfo: PPropInfo;
      OldValue, NewValue: Variant);
  end;

  { TGTNodeEventList }

  TGTNodeEventList = class (TObject)
  public
    constructor Create;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
  private
    FCalling: Integer;
    FDeferredDeletes, FDeferredDeleteAlls: TFPList;
    FDeferredAdds: TFPList;
    FList: TFPList;
    FLocked: Integer;
  protected
    procedure BeginCall;
    procedure EndCall;
    function LookupHandler(AHandler: TGTNodeEvent; AClassMask: TGTBaseClass = nil): Integer;
  public
    procedure BeginLock;
    procedure Call(Sender: TObject; ANode: TGTBaseObject);
    procedure Clear;
    procedure EndLock;
    procedure RegisterHandler(AHandler: TGTNodeEvent; AClassMask: TGTBaseClass = nil);
    procedure UnRegisterHandler(AHandler: TGTNodeEvent; AClassMask: TGTBaseClass = nil);
    procedure UnRegisterAll(AHandler: TGTNodeEvent);
  end;

  TGTReferenceID = Ptruint;

  IGTLoaderContext = interface ['{6B33E981-9190-465F-ADCC-54F2516901CA}']
    procedure RegisterFinalizationHandler(AHandler: TGTEvent);
    procedure RegisterFinalization(Instance: TObject; Prop: PPropInfo; ID: TGTReferenceID);
    function SolveTemplateReference(const AReferenceID: TGTReferenceID): TGTTemplate;
    procedure DeclareTemplateReference(const AReferenceID: TGTReferenceID; Instance: TGTTemplate);
  end;

  IGTLoaderContext2 = interface ['{BDDE8994-D0AF-4AA1-9AE6-073FFEE5B1AA}']
    function SolveReference(const AClass: TGTBaseClass; const AReferenceID: TGTReferenceID): TGTBaseObject;
    procedure DeclareReference(const AInstance: TGTBaseObject; const AReferenceID: TGTReferenceID);
  end;

  ENullReferenceException = class (Exception);

  generic IGTReferenceManagerLoadingObserver<_Type> = interface ['{B5C37FF7-72FA-4642-BFDB-B2AAAE8458BB}']
    procedure LoadedObject(const AInstance: _Type);
  end;

  { TGTReferenceManager }

  generic TGTReferenceManager<_Type, IGTObserver> = class (TObject)
  private type
    TGTReferenceBlock = array [0..ReferenceBlockSize-1] of _Type;
    PGTReferenceBlock = ^TGTReferenceBlock;

    PGTReferenceFreeBlock = ^TGTReferenceFreeBlock;
    TGTReferenceFreeBlock = record
      Start: TGTReferenceID;
      Length: TGTReferenceID;
      Next: PGTReferenceFreeBlock;
      Prev: PGTReferenceFreeBlocK;
    end;
  var public
    constructor Create;
    destructor Destroy; override;
  private
    FAllocatedBlocks: Integer;
    FBlocks: array of PGTReferenceBlock;
    FFirstFreeBlock: PGTReferenceFreeBlock;
    FIDCapacity: TGTReferenceID;
    FLastFreeBlock: PGTReferenceFreeBlock;
  protected
    procedure Expand;
    procedure SetIDCapacity(const AValue: TGTReferenceID);
  public
    function AllocateReference(const AObject: _Type): TGTReferenceID; virtual;
    procedure Clear;
    procedure DeclareReference(const AObject: _Type; const ReferenceID: TGTReferenceID); virtual;
    procedure LoadFromXML(const XMLNode: TxmlNode; Context: IGTLoaderContext = nil);
    procedure ReleaseReference(const ReferenceID: TGTReferenceID);
    procedure SaveToXML(const XMLNode: TxmlNode);
    function SolveReference(const ReferenceID: TGTReferenceID): _Type;
  published
    property Capacity: TGTReferenceID read FIDCapacity write SetIDCapacity;
  end;

  { TGTBaseEditable }

  TGTBaseEditable = class (TPersistent, IUnknown)
  private
    FData: Pointer;
  protected // IUnknown
    function _AddRef: LongInt; stdcall;
    function _Release: LongInt; stdcall;
    function QueryInterface(const iid : tguid;out obj) : longint;stdcall;
  public
    class function ClassToString: String; virtual;
    function ToString: String; override;
  public
    property Data: Pointer read FData write FData;
  end;
  TGTBaseEditableClass = class of TGTBaseEditable;

  { TGTBaseObject }

  TGTBaseObject = class (TGTBaseEditable)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  private
    FOnChange: TGTEventList;
    FOnDestruction: TGTEventList;
    FOnPropChange: TGTPropChangeEventList;
    FTemplate: TGTTemplate;
  protected
    function DefaultNodeName: String; virtual;
    procedure DoChange; virtual;
    procedure DoPropChange(PropName: String;
      OldValue, NewValue: Variant); virtual;
    procedure OnTemplateChange(Sender: TObject; const PropertyName: String;
      PropInfo: PPropInfo; const OldValue, NewValue: Variant);
    procedure OnTemplateDestruction(Sender: TObject);
    procedure RaiseMissingContext(AContextName: String);
    procedure SetTemplate(ATemplate: TGTTemplate);
  public
    procedure BeforeDestruction; override;
    // NOTE that these methods should *not* be used to export to the final game
    // data. These methods are for editor internal purposes. For exporting
    // one should define own methods in an own base class based on this one.
    procedure LoadFromFile(const AFileName: String); virtual;
    procedure LoadFromStream(const AStream: TStream); virtual;
    procedure LoadFromURL(const AURL: String); virtual;
    procedure LoadFromXML(const XMLNode: TxmlNode; Context: IGTLoaderContext = nil); virtual;
    procedure SaveToFile(const AFileName: String); virtual;
    procedure SaveToStream(const AStream: TStream); virtual;
    procedure SaveToURL(const AURL: String); virtual;
    procedure SaveToXML(const XMLNode: TxmlNode); virtual;
  public
    property OnChange: TGTEventList read FOnChange;
    property OnDestruction: TGTEventList read FOnDestruction;
    property OnPropChange: TGTPropChangeEventList read FOnPropChange;
  published
    property Template: TGTTemplate read FTemplate write SetTemplate stored false;
  end;

  TGTBaseObjectList = specialize TFPGList<TGTBaseObject>;

  { TGTMultiRefObject }

  TGTMultiRefObject = class (TGTBaseObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FID: TGTReferenceID;
    FOnRefChange: TGTEventList;
    FReferences: TGTBaseObjectList;
    function GetReferencer(Index: Integer): TGTBaseObject;
  protected
    procedure DoRefChange; virtual;
    function GetReferenceCount: Integer;
    procedure RemoveAllReferences;
  protected
    property Referencer[Index: Integer]: TGTBaseObject read GetReferencer;
  public
    procedure AddReference(const AObject: TGTBaseObject); virtual;
    procedure Debug_PrintUsers;
    procedure RemoveReference(const AObject: TGTBaseObject); virtual;
    procedure RemoveAllReferences(const AObject: TGTBaseObject);
  public
    property ID: TGTReferenceID read FID write FID;
    property OnRefChange: TGTEventList read FOnRefChange;
    property ReferenceCount: Integer read GetReferenceCount stored false;
  end;

  generic TGTReference<TGTClass> = class (TGTBaseObject)
  public
    constructor Create; override;
    destructor Destroy; override;
  private
    FObj: TGTClass;
    FTmpID: TGTReferenceID;
    FOnAssigned: TGTEventList;
  protected
    procedure FinalizeLoading(Sender: TObject);
    procedure LinkObject(const AObj: TGTClass);
    procedure ObjDeleting(Sender: TObject);
    procedure SetObj(const AObj: TGTClass);
    procedure UnlinkObject(const AObj: TGTClass);
  public
    procedure BeforeDestruction; override;
    procedure LoadFromXML(const XMLNode: TxmlNode; Context: IGTLoaderContext =
       nil); override;
    procedure SaveToXML(const XMLNode: TxmlNode); override;
  public
    property Obj: TGTClass read FObj write SetObj;
    property OnAssigned: TGTEventList read FOnAssigned;
  end;

  { TGTTemplate }

  TGTTemplate = class (TPersistent)
  public
    constructor Create(AClass: TGTBaseClass);
    destructor Destroy; override;
  private
    FID: TGTReferenceID;
    FInstance: TGTBaseObject;
    FName: String;
    FOnDestruction: TGTEventList;
    function GetOnPropUpdate: TGTPropChangeEventList;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromXML(const XMLNode: TxmlNode; Context: IGTLoaderContext = nil);
    procedure SaveToXML(const XMLNode: TxmlNode);
  published
    property Instance: TGTBaseObject read FInstance;
    property Name: String read FName write FName;
    property OnDestruction: TGTEventList read FOnDestruction;
    property OnPropUpdate: TGTPropChangeEventList read GetOnPropUpdate;
  end;

  { TGTObjectFactory }

  TGTObjectFactory = class (TObject)
  private
    constructor Create;
  public
    class function CreateGTClass(AClassName: String): TGTBaseObject;
  end;

  IGTTemplateManagerLoadingObserver = specialize IGTReferenceManagerLoadingObserver<TGTTemplate>;

  TGTTemplateManager_ = specialize TGTReferenceManager<TGTTemplate, IGTTemplateManagerLoadingObserver>;

  { TGTTemplateManager }

  TGTTemplateManager = class (TGTTemplateManager_)
  public
    function AllocateReference(const AObject: TGTTemplate): TGTReferenceID; override;
    procedure DeclareReference(const AObject: TGTTemplate;
       const ReferenceID: TGTReferenceID); override;
  end;

type
  TGTBaseClassList = specialize TFPGList<TGTBaseClass>;

function FindGTClass(AClassName: String): TGTBaseClass;
procedure EnumerateGTClasses(ABaseClass: TGTBaseClass; AOutput: TFPList);
procedure RegisterGTClass(AClassType: TGTBaseClass; AAlias: String = '');

function TryStrToID(const AStr: String; out ID: TGTReferenceID): Boolean;
function StrToID(const AStr: String): TGTReferenceID;
function IDToStr(const ID: TGTReferenceID): String;

implementation

var
  ClassHashTable: TFPDataHashTable = nil;
  ClassList: TGTBaseClassList = nil;
const
  ClassListHTSize = 193;

procedure EnumerateGTClasses(ABaseClass: TGTBaseClass; AOutput: TFPList);
var
  I: Integer;
begin
  if ClassList = nil then
    Exit;
  for I := 0 to ClassList.Count - 1 do
  begin
    if ClassList[I].InheritsFrom(ABaseClass) then
      AOutput.Add(ClassList[I]);
  end;
end;

procedure RegisterGTClass(AClassType: TGTBaseClass; AAlias: String);
begin
  if ClassHashTable = nil then
    ClassHashTable := TFPDataHashTable.CreateWith(ClassListHTSize, @RSHash);
  if ClassList = nil then
    ClassList := TGTBaseClassList.Create;
  if AAlias <> '' then
  begin
    ClassHashTable.Add(AAlias, AClassType);
  end
  else
  begin
    ClassHashTable.Add(AClassType.ClassName, AClassType);
    ClassList.Add(AClassType);
  end;
end;

function TryStrToID(const AStr: String; out ID: TGTReferenceID): Boolean;
{$ifdef CPU32}
var
  TmpID: QWord;
{$endif}
begin
  {$ifdef CPU32}
  Result := TryStrToQWord(AStr, TmpID);
  ID := TmpID;
  {$else}
  Result := TryStrToQWord(AStr, ID);
  {$endif}
end;

function StrToID(const AStr: String): TGTReferenceID;
begin
  Result := StrToQWord(AStr);
end;

function IDToStr(const ID: TGTReferenceID): String;
begin
  Result := IntToStr(ID);
end;

function FindGTClass(AClassName: String): TGTBaseClass;
begin
  if ClassHashTable = nil then
  begin
    Result := nil;
    Exit;
  end;
  Result := TGTBaseClass(ClassHashTable.Items[AClassName]);
end;

{ TGTGenericEventList }

constructor TGTGenericEventList.Create;
begin
  FList := TFPList.Create;
  FDeferredDeletes := TFPList.Create;
  FCalling := 0;
  FLocked := 0;
end;

destructor TGTGenericEventList.Destroy;
begin
  FDeferredDeletes.Free;
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TGTGenericEventList.BeginCall;
begin
  Inc(FCalling);
end;

procedure TGTGenericEventList.EndCall;
var
  I: Integer;
  Item: PGTGenericEventListEntry;
begin
  Dec(FCalling);
  if FCalling = 0 then
  begin
    for I := 0 to FDeferredDeletes.Count - 1 do
    begin
      Item := PGTGenericEventListEntry(FDeferredDeletes[I]);
      UnRegisterHandler(Item^.AHandler);
      FreeMem(Item);
    end;
    FDeferredDeletes.Clear;
  end;
  Assert(FCalling >= 0);
end;

function TGTGenericEventList.EqualMethodPointers(const Method1, Method2: TMethod
  ): Boolean;
begin
  Result :=  (Method1.Code = Method2.Code) and (Method1.Data = Method2.Data);

end;

function TGTGenericEventList.LookupHandler(AHandler: EventType): Integer;
var
  I: Integer;
  List: PPointerList;
begin
  List := FList.List;
  for I := 0 to FList.Count - 1 do
    if EqualMethodPointers(TMethod(PGTGenericEventListEntry(List^[I])^.AHandler), TMethod(AHandler)) then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

procedure TGTGenericEventList.BeginLock;
begin
  Inc(FLocked);
end;

procedure TGTGenericEventList.Clear;
var
  I: Integer;
  List: PPointerList;
begin
  List := FList.List;
  for I := 0 to FList.Count - 1 do
    FreeMem(List^[I]);
  FList.Clear;
end;

procedure TGTGenericEventList.EndLock;
begin
  Dec(FLocked);
end;

procedure TGTGenericEventList.RegisterHandler(AHandler: EventType);
var
  Item: PGTGenericEventListEntry;
begin
  if FCalling > 0 then
    raise EInvalidOperation.Create('Cannot register handler in event list while it is being called.');
  if LookupHandler(AHandler) >= 0 then
    Exit;
  GetMem(Item, SizeOf(TGTGenericEventListEntry));
  Item^.AHandler := AHandler;
  FList.Add(Item);
end;

procedure TGTGenericEventList.UnRegisterHandler(AHandler: EventType);
var
  I: Integer;
  Item: PGTGenericEventListEntry;
begin
  if FCalling > 0 then
  begin
    GetMem(Item, SizeOf(TGTGenericEventListEntry));
    Item^.AHandler := AHandler;
    FDeferredDeletes.Add(Item);
    Exit;
  end;
  I := LookupHandler(AHandler);
  if (I < 0) then
    Exit;
  FreeMem(FList[I]);
  FList.Delete(I);
end;

{ TGTEventList }

procedure TGTEventList.Call(Sender: TObject);
var
  I: Integer;
  List: PPointerList;
begin
  if FLocked > 0 then
    Exit;
  BeginCall;
    List := FList.List;
    for I := 0 to FList.Count - 1 do
      PGTGenericEventListEntry(List^[I])^.AHandler(Sender);
  EndCall;
end;

{ TGTPropChangeEventList }

procedure TGTPropChangeEventList.Call(Sender: TObject; PropertyName: String;
  PropInfo: PPropInfo; OldValue, NewValue: Variant);
var
  I: Integer;
  List: PPointerList;
begin
  if FLocked > 0 then
    Exit;
  BeginCall;
    List := FList.List;
    for I := 0 to FList.Count - 1 do
      PGTGenericEventListEntry(List^[I])^.AHandler(Sender, PropertyName,
        PropInfo, OldValue, NewValue);
  EndCall;
end;

{ TGTNodeEventList }

type
  TGTEventListEntry = record
    AHandler: TGTNodeEvent;
  end;
  PGTEventListEntry = ^TGTEventListEntry;
  TGTNodeEventListEntry = record
    AHandler: TGTNodeEvent;
    AClassMask: TGTBaseClass;
  end;
  PGTNodeEventListEntry = ^TGTNodeEventListEntry;

constructor TGTNodeEventList.Create;
begin
  FCalling := 0;
  FDeferredDeletes := TFPList.Create;
  FDeferredDeleteAlls := TFPList.Create;
  FDeferredAdds := TFPList.Create;
  FList := TFPList.Create;
  FLocked := 0;
end;

procedure TGTNodeEventList.BeforeDestruction;
begin
  if FCalling > 0 then
    raise EGTCoreError.Create('Cannot destroy a event list while it is being called.');
  inherited BeforeDestruction;
end;

destructor TGTNodeEventList.Destroy;
begin
  FDeferredAdds.Free;
  FDeferredDeleteAlls.Free;
  FDeferredDeletes.Free;
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TGTNodeEventList.BeginCall;
begin
  Inc(FCalling);
end;

procedure TGTNodeEventList.EndCall;
var
  I: Integer;
  ItemAll: PGTEventListEntry;
  Item: PGTNodeEventListEntry;
begin
  Dec(FCalling);
  if FCalling = 0 then
  begin
    for I := 0 to FDeferredDeleteAlls.Count - 1 do
    begin
      ItemAll := PGTEventListEntry(FDeferredDeleteAlls[I]);
      UnRegisterAll(TGTNodeEvent(ItemAll^.AHandler));
      FreeMem(ItemAll);
    end;
    FDeferredDeleteAlls.Clear;
    for I := 0 to FDeferredDeletes.Count - 1 do
    begin
      Item := PGTNodeEventListEntry(FDeferredDeletes[I]);
      UnRegisterHandler(Item^.AHandler, Item^.AClassMask);
      FreeMem(Item);
    end;
    FDeferredDeletes.Clear;

    for I := 0 to FDeferredAdds.Count - 1 do
    begin
      Item := PGTNodeEventListEntry(FDeferredAdds[I]);
      RegisterHandler(Item^.AHandler, Item^.AClassMask);
      FreeMem(Item);
    end;
    FDeferredAdds.Clear;
  end;
  Assert(FCalling >= 0);
end;

function TGTNodeEventList.LookupHandler(AHandler: TGTNodeEvent;
  AClassMask: TGTBaseClass): Integer;
var
  I: Integer;
  List: PPointerList;
  Item: PGTNodeEventListEntry;
begin
  List := FList.List;
  for I := 0 to FList.Count - 1 do
  begin
    Item := PGTNodeEventListEntry(List^[I]);
    if (Item^.AClassMask = AClassMask) and (Item^.AHandler = AHandler) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

procedure TGTNodeEventList.BeginLock;
begin
  Inc(FLocked);
end;

procedure TGTNodeEventList.Call(Sender: TObject; ANode: TGTBaseObject);
var
  I: Integer;
  List: PPointerList;
  Item: PGTNodeEventListEntry;
begin
  if FLocked > 0 then
    Exit;
  BeginCall;
    List := FList.List;
    for I := 0 to FList.Count - 1 do
    begin
      Item := PGTNodeEventListEntry(List^[I]);
      if (Item^.AClassMask = nil) or (ANode.ClassType = Item^.AClassMask) or (ANode.InheritsFrom(Item^.AClassMask)) then
        Item^.AHandler(Sender, ANode);
    end;
  EndCall;
end;

procedure TGTNodeEventList.Clear;
var
  I: Integer;
  List: PPointerList;
begin
  List := FList.List;
  for I := 0 to FList.Count - 1 do
    FreeMem(List^[I]);
  FList.Clear;
end;

procedure TGTNodeEventList.EndLock;
begin
  Dec(FLocked);
end;

procedure TGTNodeEventList.RegisterHandler(AHandler: TGTNodeEvent;
  AClassMask: TGTBaseClass);
var
  Item: PGTNodeEventListEntry;
begin
  if LookupHandler(AHandler, AClassMask) >= 0 then
    Exit;
  GetMem(Item, SizeOf(TGTNodeEventListEntry));
  Item^.AHandler := AHandler;
  Item^.AClassMask := AClassMask;
  if FCalling > 0 then
    FDeferredAdds.Add(Item)
  else
    FList.Add(Item);
end;

procedure TGTNodeEventList.UnRegisterHandler(AHandler: TGTNodeEvent;
  AClassMask: TGTBaseClass);
var
  I: Integer;
  Item: PGTNodeEventListEntry;
begin
  if FCalling > 0 then
  begin
    GetMem(Item, SizeOf(TGTNodeEventListEntry));
    Item^.AClassMask := AClassMask;
    Item^.AHandler := AHandler;
    FDeferredDeletes.Add(Item);
    Exit;
  end;
  I := LookupHandler(AHandler, AClassMask);
  if (I < 0) then
    Exit;
  FreeMem(FList[I]);
  FList.Delete(I);
end;

procedure TGTNodeEventList.UnRegisterAll(AHandler: TGTNodeEvent);
var
  I: Integer;
  List: PPointerList;
  Item: PGTNodeEventListEntry;
  ItemAll: PGTEventListEntry;
begin
  if FCalling > 0 then
  begin
    GetMem(ItemAll, SizeOf(TGTEventListEntry));
    ItemAll^.AHandler := AHandler;
    FDeferredDeletes.Add(ItemAll);
    Exit;
  end;
  List := FList.List;
  I := FList.Count - 1;
  while I >= 0 do
  begin
    Item := List^[I];
    if (Item^.AHandler = AHandler) then
    begin
      FreeMem(Item);
      FList.Delete(I);
      List := FList.List;
    end;
    Dec(I);
  end;
end;

{ TGTReferenceManager }

constructor TGTReferenceManager.Create;
begin
  FAllocatedBlocks := 0;
  SetLength(FBlocks, 0);
  FFirstFreeBlock := nil;
  FIDCapacity := 0;
  FLastFreeBlock := nil;
  Expand;
end;

destructor TGTReferenceManager.Destroy;
var
  I: Integer;
  NextBlock: PGTReferenceFreeBlock;
begin
  for I := 0 to FAllocatedBlocks - 1 do
    FreeMem(FBlocks[I]);
  while FFirstFreeBlock <> nil do
  begin
    NextBlock := FFirstFreeBlock^.Next;
    FreeMem(FFirstFreeBlock);
    FFirstFreeBlock := NextBlock;
  end;
  inherited Destroy;
end;

procedure TGTReferenceManager.Expand;
var
  I: Integer;
  NewBlock: PGTReferenceFreeBlock;
begin
  if FAllocatedBlocks = Length(FBlocks) then
  begin
    SetLength(FBlocks, Length(FBlocks)+4);
    for I := FAllocatedBlocks+1 to FAllocatedBlocks+3 do
      FBlocks[I] := nil;
  end;
  FBlocks[FAllocatedBlocks] := GetMem(SizeOf(TGTReferenceBlock));
  FillDWord(FBlocks[FAllocatedBlocks]^, SizeOf(TGTReferenceBlock) div 4, 0);
  Inc(FAllocatedBlocks);
  if FFirstFreeBlock = nil then
  begin
    FFirstFreeBlock := GetMem(SizeOf(TGTReferenceFreeBlock));
    FFirstFreeBlock^.Start := FIDCapacity;
    FFirstFreeBlock^.Length := ReferenceBlockSize;
    FFirstFreeBlock^.Prev := nil;
    FFirstFreeBlock^.Next := nil;

    if (FFirstFreeBlock^.Start = 0) then
    begin
      Inc(FFirstFreeBlock^.Start);
      Dec(FFirstFreeBlock^.Length);
    end;
    FLastFreeBlock := FFirstFreeBlock;
  end
  else if (FLastFreeBlock^.Start + FLastFreeBlock^.Length = FIDCapacity - 1) then
  begin
    Inc(FLastFreeBlock^.Length, ReferenceBlockSize);
  end
  else
  begin
    NewBlock := GetMem(SizeOf(TGTReferenceFreeBlock));
    NewBlock^.Start := FIDCapacity;
    NewBlock^.Length := ReferenceBlockSize;
    NewBlock^.Prev := FLastFreeBlock;
    NewBlock^.Next := nil;
    FLastFreeBlock^.Next := NewBlock;
    FLastFreeBlock := NewBlock;
  end;
  Inc(FIDCapacity, ReferenceBlockSize);
end;

procedure TGTReferenceManager.SetIDCapacity(const AValue: TGTReferenceID);
begin
  if (AValue <= FIDCapacity) then
    Exit;
  while (FIDCapacity < AValue) do
    Expand;
end;

function TGTReferenceManager.AllocateReference(const AObject: _Type
  ): TGTReferenceID;
var
  D, M: Integer;
begin
  if Pointer(AObject) = nil then
    raise EGTReferenceError.Create('Cowardly refusing to allocate an ID for a nil object.');
  if FFirstFreeBlock = nil then
    Expand;
  Dec(FFirstFreeBlock^.Length);
  Result := FFirstFreeBlock^.Start + FFirstFreeBlock^.Length;
  DivMod(Result, ReferenceBlockSize, D, M);
  FBlocks[D]^[M] := AObject;
end;

procedure TGTReferenceManager.Clear;
var
  I: Integer;
  NextBlock: PGTReferenceFreeBlock;
begin
  for I := 1 to FAllocatedBlocks - 1 do
  begin
    FreeMem(FBlocks[I]);
    FBlocks[I] := nil;
  end;
  SetLength(FBlocks, 4);
  FillDWord(FBlocks[0]^, SizeOf(TGTReferenceBlock) div 4, 0);
  while FFirstFreeBlock <> nil do
  begin
    NextBlock := FFirstFreeBlock^.Next;
    FreeMem(FFirstFreeBlock);
    FFirstFreeBlock := NextBlock;
  end;
  FAllocatedBlocks := 1;
  FFirstFreeBlock := GetMem(SizeOf(TGTReferenceFreeBlock));
  FFirstFreeBlock^.Start := 1;
  FFirstFreeBlock^.Length := ReferenceBlockSize - 1;
  FFirstFreeBlock^.Next := nil;
  FFirstFreeBlock^.Prev := nil;
  FLastFreeBlock := FFirstFreeBlock;
  FIDCapacity := ReferenceBlockSize;
end;

procedure TGTReferenceManager.DeclareReference(const AObject: _Type;
  const ReferenceID: TGTReferenceID);
begin
  if ReferenceID = NULL then
    raise ENullReferenceException.Create('Cannot overwrite NULL reference.');
  while (FIDCapacity <= ReferenceID) do
    Expand;
  FBlocks[(ReferenceID div ReferenceBlockSize)]^[(ReferenceID mod ReferenceBlockSize)] := AObject;
end;

procedure TGTReferenceManager.LoadFromXML(const XMLNode: TxmlNode;
  Context: IGTLoaderContext);
var
  ObjectNodes: TxmlNodeArray;
  ObjectNode: TxmlNode;
  I: Integer;
  ID: TGTReferenceID;
  Obj: _Type;
  AClassType: TGTBaseClass;
  AClassName: String;
  Intf: IGTObserver;
begin
  Clear;
  SetIDCapacity(StrToQWord(XMLNode.NodeValue['capacity']));
  ObjectNodes := XMLNode.Node['objects'].Nodes['object'];
  if Context.QueryInterface(IGTObserver, Intf) <> S_OK then
    Intf := nil;
  for I := 0 to High(ObjectNodes) do
  begin
    ObjectNode := ObjectNodes[I];
    ID := StrToQWord(ObjectNode.Attribute['id']);
    AClassName := ObjectNode.Attribute['class'];
    AClassType := FindGTClass(AClassName);
    if AClassType = nil then
      Continue;
    if not AClassType.InheritsFrom(_Type) then
      Continue;
    {$WARNING This may actually result in problems!}
    // Warning: Class types "TGTBaseObject" and "TGTTemplate" are not related
    Obj := _Type(AClassType.Create);
    try
      Obj.LoadFromXML(ObjectNode, Context);
      DeclareReference(Obj, ID);
      if IUnknown(Intf) <> nil then
        Intf.LoadedObject(Obj);
    except
      Obj.Free;
      DeclareReference(nil, ID);
      raise;
    end;
  end;
end;

procedure TGTReferenceManager.ReleaseReference(const ReferenceID: TGTReferenceID
  );
var
  CurrBlock, NextBlock, NewBlock: PGTReferenceFreeBlock;
begin
  if (ReferenceID = NULL) or (ReferenceID >= FIDCapacity) then
    Exit;
  FBlocks[(ReferenceID div ReferenceBlockSize)]^[(ReferenceID mod ReferenceBlockSize)] := nil;
  if FFirstFreeBlock = nil then
  begin
    FFirstFreeBlock := GetMem(SizeOf(TGTReferenceFreeBlock));
    FFirstFreeBlock^.Start := ReferenceID;
    FFirstFreeBlock^.Length := 1;
    FFirstFreeBlock^.Next := nil;
    FFirstFreeBlock^.Prev := nil;
    FLastFreeBlock := FFirstFreeBlock;
  end
  else
  begin
    CurrBlock := FFirstFreeBlock;
    if CurrBlock^.Start > ReferenceID then
    begin
      if CurrBlock^.Start = ReferenceID + 1 then
      begin
        Dec(CurrBlock^.Start);
        Inc(CurrBlock^.Length);
      end
      else
      begin
        NewBlock := GetMem(SizeOf(TGTReferenceFreeBlock));
        NewBlock^.Length := 1;
        NewBlock^.Start := ReferenceID;
        NewBlock^.Prev := nil;
        NewBlock^.Next := CurrBlock;
        CurrBlock^.Prev := NewBlock;
        FFirstFreeBlock := NewBlock;
      end;
    end;
    NextBlock := CurrBlock^.Next;
    while NextBlock <> nil do
    begin
      if (CurrBlock^.Start < ReferenceID) and (NextBlock^.Start > ReferenceID) then
      begin
        if CurrBlock^.Start + CurrBlock^.Length = ReferenceID then
        begin
          // CurrBlock ends right before ReferenceID
          Inc(CurrBlock^.Length);
          if CurrBlock^.Start + CurrBlock^.Length = NextBlock^.Start then
          begin
            // CurrBlock and NextBlock are now to be merged
            if NextBlock^.Next <> nil then
              NextBlock^.Next^.Prev := CurrBlock
            else
              FLastFreeBlock := CurrBlock;
            CurrBlock^.Next := NextBlock^.Next;
            CurrBlock^.Length += NextBlock^.Length;
            FreeMem(NextBlock);
          end;
        end
        else if NextBlock^.Start = ReferenceID + 1 then
        begin
          // NextBlock starts right after ReferenceID
          Dec(NextBlock^.Start);
          Inc(NextBlock^.Length);
          if CurrBlock^.Start + CurrBlock^.Length = NextBlock^.Start then
          begin
            // CurrBlock and NextBlock are now to be merged.
            if NextBlock^.Next <> nil then
              NextBlock^.Next^.Prev := CurrBlock
            else
              FLastFreeBlock := CurrBlock;
            CurrBlock^.Next := NextBlock^.Next;
            CurrBlock^.Length += NextBlock^.Length;
            FreeMem(NextBlock);
          end;
        end
        else
        begin
          NewBlock := GetMem(SizeOf(TGTReferenceFreeBlock));
          NewBlock^.Start := ReferenceID;
          NewBlock^.Length := 1;
          NewBlock^.Prev := CurrBlock;
          CurrBlock^.Next := NewBlock;
          NewBlock^.Next := NextBlock;
          NextBlock^.Prev := NewBlock;
        end;
        Exit;
      end;
    end;
    if CurrBlock^.Start + CurrBlock^.Length = ReferenceID then
    begin
      Inc(CurrBlock^.Length);
    end
    else
    begin
      NewBlock := GetMem(SizeOf(TGTReferenceFreeBlock));
      NewBlock^.Start := ReferenceID;
      NewBlock^.Length := 1;
      NewBlock^.Next := nil;
      NewBlock^.Prev := FLastFreeBlock;
      FLastFreeBlock^.Next := NewBlock;
      FLastFreeBlock := NewBlock;
    end;
  end;
end;

procedure TGTReferenceManager.SaveToXML(const XMLNode: TxmlNode);
var
  ObjectsNode: TxmlNode;
  ObjectNode: TxmlNode;
  I: TGTReferenceID;
  Obj: _Type;
begin
  XMLNode.NodeValue['capacity'] := IntToStr(FIDCapacity);
  ObjectsNode := XMLNode.AddNode('objects');
  for I := 1 to FIDCapacity do
  begin
    Obj := SolveReference(I);
    if Pointer(Obj) = nil then
      Continue;
    ObjectNode := ObjectsNode.AddNode('object');
    ObjectNode.Attribute['id'] := IntToStr(I);
    ObjectNode.Attribute['class'] := Obj.ClassName;
    Obj.SaveToXML(ObjectNode);
  end;
end;

function TGTReferenceManager.SolveReference(const ReferenceID: TGTReferenceID
  ): _Type;
begin
  if (ReferenceID >= FIDCapacity) then
  begin
    Result := nil;
    Exit;
  end;
  Result := _Type(FBlocks[(ReferenceID div ReferenceBlockSize)]^[(ReferenceID mod ReferenceBlockSize)]);
end;

{ TGTBaseEditable }

function TGTBaseEditable._AddRef: LongInt; stdcall;
begin
  Result := -1;
end;

function TGTBaseEditable._Release: LongInt; stdcall;
begin
  Result := -1;
end;

function TGTBaseEditable.QueryInterface(const iid: tguid; out obj): longint;
  stdcall;
begin
  if GetInterface(iid, obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

class function TGTBaseEditable.ClassToString: String;
begin
  Result := ClassName;
end;

function TGTBaseEditable.ToString: String;
begin
  Result := ClassToString+'@'+IntToHex(ptrint(Self), SizeOf(ptrint)*2);
end;

{ TGTBaseObject }

constructor TGTBaseObject.Create;
begin
  FTemplate := nil;
  FOnChange := TGTEventList.Create;
  FOnDestruction := TGTEventList.Create;
  FOnPropChange := TGTPropChangeEventList.Create;
  // WriteLn(Format('%s@%16.16x: Created', [ClassName, ptrint(Self)]));
end;

destructor TGTBaseObject.Destroy;
begin
  SetTemplate(nil);
  FOnChange.Free;
  FOnDestruction.Free;
  FOnPropChange.Free;
  inherited Destroy;
end;

function TGTBaseObject.DefaultNodeName: String;
begin
  Result := Copy(ClassName, 2, Length(ClassName));
end;

procedure TGTBaseObject.DoChange;
begin
  FOnChange.Call(Self);
end;

procedure TGTBaseObject.DoPropChange(PropName: String; OldValue,
  NewValue: Variant);
var
  PropInfo: PPropInfo;
begin
  PropInfo := GetPropInfo(ClassType, PropName);
  if (PropInfo <> nil) then // only do this for published properties
    OnPropChange.Call(Self, PropName, PropInfo, OldValue, NewValue);
end;

procedure TGTBaseObject.OnTemplateChange(Sender: TObject;
  const PropertyName: String; PropInfo: PPropInfo; const OldValue,
  NewValue: Variant);
var
  Value: Variant;
begin
  // Performance optimization possible: Somehow use the PropInfo instead of
  // anything else.
  Value := GetPropValue(Self, PropertyName);
  if Value = OldValue then
    SetPropValue(Self, PropertyName, NewValue);
end;

procedure TGTBaseObject.OnTemplateDestruction(Sender: TObject);
begin
  SetTemplate(nil);
end;

procedure TGTBaseObject.RaiseMissingContext(AContextName: String);
begin
  raise EGTContextError.CreateFmt('%s needs %s to finalize.', [ClassName, AContextName]);
end;

procedure TGTBaseObject.SetTemplate(ATemplate: TGTTemplate);
begin
  if FTemplate <> nil then
  begin
    FTemplate.OnDestruction.UnRegisterHandler(@OnTemplateDestruction);
    FTemplate.OnPropUpdate.UnRegisterHandler(@OnTemplateChange);
  end;
  FTemplate := ATemplate;
  if FTemplate <> nil then
  begin
    FTemplate.OnDestruction.RegisterHandler(@OnTemplateDestruction);
    FTemplate.OnPropUpdate.RegisterHandler(@OnTemplateChange);
  end;
end;

procedure TGTBaseObject.BeforeDestruction;
begin
  inherited BeforeDestruction;
  // WriteLn(Format('%s@%16.16x: BeforeDestruction', [ClassName, ptrint(Self)]));
  FOnDestruction.Call(Self);
end;

procedure TGTBaseObject.LoadFromFile(const AFileName: String);
var
  Doc: TxmlDocument;
begin
  Doc := TxmlDocument.Create;
  try
    Doc.LoadFromFile(AFileName);
    LoadFromXML(Doc.RootNode);
  finally
    Doc.Free;
  end;
end;

procedure TGTBaseObject.LoadFromStream(const AStream: TStream);
var
  Doc: TxmlDocument;
begin
  Doc := TxmlDocument.Create;
  try
    Doc.LoadFromStream(AStream);
    LoadFromXML(Doc.RootNode);
  finally
    Doc.Free;
  end;
end;

procedure TGTBaseObject.LoadFromURL(const AURL: String);
var
  S: TStream;
begin
  S := TGTURIStream.Create(AURL, omRead, wmIgnore, smDontCare);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TGTBaseObject.LoadFromXML(const XMLNode: TxmlNode; Context: IGTLoaderContext = nil);
var
  PropNode: TxmlNode;
  Prop: PPropInfo;
  I: Integer;
  I64: Int64;
  W64: QWord;
  Cls, Cls2: TClass;
  Obj: TGTBaseObject;
begin
  if not TryStrToQWord(XMLNode.Attribute['template'], W64) then
    W64 := NULL;
  if W64 <> NULL then
    Context.RegisterFinalization(Self, GetPropInfo(Self, 'Template'), W64)
  else
    FTemplate := nil;

  for I := 0 to XMLNode.Children.Count - 1 do
  begin
    PropNode := XMLNode.Children[I];
    if PropNode.Attribute['noprop'] = 'noprop' then
      Continue;

    Prop := GetPropInfo(Self, PropNode.Name);
    if Prop = nil then
      Continue;
    case Prop^.PropType^.Kind of
      tkChar, tkSString, tkLString, tkAString:
      begin
        SetStrProp(Self, Prop, PropNode.Content);
      end;
      tkWChar, tkWString, tkUChar, tkUString:
      begin
        SetWideStrProp(Self, Prop, UTF8Decode(PropNode.Content));
      end;

      tkFloat:
        SetFloatProp(Self, Prop, PropNode.ContentDouble);

      tkInt64, tkInteger:
      begin
        if TryStrToInt64(PropNode.Content, I64) then
          SetInt64Prop(Self, Prop, I64);
      end;
      tkQWord:
      begin
        if TryStrToQWord(PropNode.Content, W64) then
          SetInt64Prop(Self, PropNode.Name, Int64(W64));
      end;

      tkEnumeration:
        SetEnumProp(Self, Prop, PropNode.Content);

      tkSet:
        SetSetProp(Self, Prop, PropNode.Content);

      tkBool:
      begin
        if (PropNode.Content = '1') or (LowerCase(PropNode.Content) = LowerCase(BooleanIdents[True])) then
          SetOrdProp(Self, Prop, 1)
        else
          SetOrdProp(Self, Prop, 0);
      end;

      tkClass:
      begin
        Cls := GetObjectPropClass(ClassType, PropNode.Name);
        if not Cls.InheritsFrom(TGTBaseObject) then
          Continue;

        if Prop^.SetProc = nil then
        begin
          Obj := TGTBaseObject(GetObjectProp(Self, Prop, TGTBaseObject));
          if Obj = nil then
            Continue;
          Obj.LoadFromXML(PropNode, Context);
        end
        else
        begin
          Cls2 := FindGTClass(PropNode.Attribute['class']);
          if Cls2 = nil then
            raise EGTXMLInvalidProperty.CreateFmt('Unknown class: ''%s''.', [PropNode.Attribute['class']]);
          if not Cls2.InheritsFrom(Cls) then
            Continue;
          Obj := TGTObjectFactory.CreateGTClass(Cls2.ClassName);
          if Obj = nil then
            raise EGTCoreError.CreateFmt('Constructor was unable to create class which was found previously (''%s'').', [Cls2.ClassName]);
          SetObjectProp(Self, Prop, Obj);
          Obj.LoadFromXML(PropNode, Context);
        end;
      end;
    else
      raise EGTXMLInvalidProperty.CreateFmt('XML node for invalid property ''%s''.', [PropNode.Name]);
    end;
  end;
end;

procedure TGTBaseObject.SaveToFile(const AFileName: String);
var
  Doc: TxmlDocument;
begin
  Doc := TxmlDocument.Create;
  try
    Doc.RootNode.Name := DefaultNodeName;
    SaveToXML(Doc.RootNode);
    Doc.SaveToFile(AFileName);
  finally
    Doc.Free;
  end;
end;

procedure TGTBaseObject.SaveToStream(const AStream: TStream);
var
  Doc: TxmlDocument;
begin
  Doc := TxmlDocument.Create;
  try
    Doc.RootNode.Name := DefaultNodeName;
    SaveToXML(Doc.RootNode);
    Doc.SaveToStream(AStream);
  finally
    Doc.Free;
  end;
end;

procedure TGTBaseObject.SaveToURL(const AURL: String);
var
  S: TStream;
begin
  S := TGTURIStream.Create(AURL, omWrite, wmOverwrite, smDontCare);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TGTBaseObject.SaveToXML(const XMLNode: TxmlNode);
var
  PropList: PPropList;
  Count, I: Integer;
  Prop: PPropInfo;
  PropNode: TxmlNode;
  Cls: TClass;
  Obj: TGTBaseObject;
begin
  XMLNode.Attribute['class'] := ClassName;
  if FTemplate <> nil then
    XMLNode.Attribute['template'] := IntToStr(FTemplate.FID);

  Count := GetTypeData(ClassInfo)^.PropCount;
  PropList := GetMem(SizeOf(Pointer) * Count);
  GetPropInfos(ClassInfo, PropList);
  for I := 0 to Count - 1 do
  begin
    Prop := PropList^[I];
    if not IsStoredProp(Self, Prop) then
    begin
      WriteLn('Skipping ', Prop^.Name, ' #1');
      Continue;
    end;
    if (Prop^.PropType^.Kind in [tkInteger, tkChar, tkEnumeration, tkSet])
      and (Prop^.Default <> NODEFAULT_VALUE) then
    begin
      if GetOrdProp(Self, Prop) = Prop^.Default then
      begin
        WriteLn('Skipping ', Prop^.Name, ' #default');
        Continue;
      end;
    end;

    // Do not save properties which cannot be written except if its an object
    // property.
    if (Prop^.SetProc = nil) and (Prop^.PropType^.Kind <> tkClass) then
    begin
      WriteLn('Skipping ', Prop^.Name, ' #2 [Prop^.PropType^.Kind = ', GetEnumName(TypeInfo(TTypeKind), Ord(Prop^.PropType^.Kind)), ']');
      Continue;
    end;
    case Prop^.PropType^.Kind of
      tkChar, tkSString, tkLString, tkAString:
      begin
        XMLNode.NodeAC[Prop^.Name].Content := GetStrProp(Self, Prop);
      end;

      tkWChar, tkWString:
      begin
        XMLNode.NodeAC[Prop^.Name].Content := UTF8Encode(GetWideStrProp(Self, Prop));
      end;

      tkUString, tkUChar:
      begin
        XMLNode.NodeAC[Prop^.Name].Content := UTF8Encode(GetUnicodeStrProp(Self, Prop));
      end;

      tkFloat:
        XMLNode.NodeAC[Prop^.Name].ContentDouble := GetFloatProp(Self, Prop);

      tkInt64, tkInteger:
      begin
        XMLNode.NodeAC[Prop^.Name].Content := IntToStr(GetInt64Prop(Self, Prop));
      end;

      tkQWord:
      begin
        XMLNode.NodeAC[Prop^.Name].Content := IntToStr(QWord(GetInt64Prop(Self, Prop)));
      end;

      tkEnumeration:
        XMLNode.NodeAC[Prop^.Name].Content := GetEnumProp(Self, Prop);

      tkSet:
        XMLNode.NodeAC[Prop^.Name].Content := GetSetProp(Self, Prop, False);

      tkBool:
        XMLNode.NodeAC[Prop^.Name].Content := BooleanIdents[GetOrdProp(Self, Prop) <> 0];

      tkClass:
      begin
        Cls := GetObjectPropClass(Self, Prop^.Name);
        if not Cls.InheritsFrom(TGTBaseObject) then
        begin
          WriteLn('Skipping ', Prop^.Name, ' #3');
          Continue;
        end;
        Obj := TGTBaseObject(GetObjectProp(Self, Prop, TGTBaseObject));
        if Obj = nil then
        begin
          WriteLn('Skipping ', Prop^.Name, ' #4');
          Continue;
        end;
        Obj.SaveToXML(XMLNode.NodeAC[Prop^.Name]);
      end;
    else
      WriteLn('Skipping ', Prop^.Name, ': unknown type: ', GetEnumName(TypeInfo(TTypeKind), Ord(Prop^.PropType^.Kind)));
    end;
  end;
  FreeMem(PropList);
end;

{ TGTMultiRefObject }

constructor TGTMultiRefObject.Create;
begin
  inherited Create;
  FOnRefChange := TGTEventList.Create;
  FReferences := TGTBaseObjectList.Create;
end;

destructor TGTMultiRefObject.Destroy;
begin
  RemoveAllReferences;
  FReferences.Free;
  FOnRefChange.Free;
  inherited Destroy;
end;

function TGTMultiRefObject.GetReferencer(Index: Integer): TGTBaseObject;
begin
  Result := FReferences[Index];
end;

procedure TGTMultiRefObject.DoRefChange;
begin
  DoPropChange('ReferenceCount', 0, GetReferenceCount);
  FOnRefChange.Call(Self);
end;

function TGTMultiRefObject.GetReferenceCount: Integer;
begin
  Result := FReferences.Count;
end;

procedure TGTMultiRefObject.RemoveAllReferences;
begin
  FReferences.Clear;
  DoRefChange;
end;

procedure TGTMultiRefObject.AddReference(const AObject: TGTBaseObject);
begin
  FReferences.Add(AObject);
  DoRefChange;
end;

procedure TGTMultiRefObject.Debug_PrintUsers;
var
  I: Integer;
begin
  WriteLn('References of ', ToString, ' (', ClassName, '@', IntToHex(ptrint(Self), SizeOf(ptrint)*2), ')');
  for I := 0 to FReferences.Count - 1 do
    WriteLn(' ', FReferences[I].ToString);
end;

procedure TGTMultiRefObject.RemoveReference(const AObject: TGTBaseObject);
begin
  FReferences.Remove(AObject);
  DoRefChange;
end;

procedure TGTMultiRefObject.RemoveAllReferences(const AObject: TGTBaseObject);
var
  I: Integer;
  Changed: Boolean;
begin
  Changed := False;
  for I := FReferences.Count - 1 downto 0 do
  begin
    if FReferences[I] = AObject then
    begin
      FReferences.Delete(I);
      Changed := True;
    end;
  end;
  if Changed then
    DoRefChange;
end;

{ TGTReference }

constructor TGTReference.Create;
begin
  inherited Create;
  FOnAssigned := TGTEventList.Create;
  TGTBaseObject(FObj) := nil;
end;

destructor TGTReference.Destroy;
begin
  SetObj(nil);
  FOnAssigned.Free;
  inherited Destroy;
end;

procedure TGTReference.FinalizeLoading(Sender: TObject);
var
  Ctx: IGTLoaderContext2;
begin
  if not Sender.GetInterface(IGTLoaderContext2, Ctx) then
    RaiseMissingContext('IGTLoaderContext2');
  SetObj(TGTClass(Ctx.SolveReference(TGTBaseClass(TGTClass), FTmpID)));
end;

procedure TGTReference.LinkObject(const AObj: TGTClass);
begin
  AObj.OnDestruction.RegisterHandler(@ObjDeleting);
  AObj.OnChange.RegisterHandler(@OnChange.Call);
end;

procedure TGTReference.ObjDeleting(Sender: TObject);
begin
  UnlinkObject(TGTClass(Sender));
  OnDestruction.Call(Self);
  FObj := nil;
end;

procedure TGTReference.SetObj(const AObj: TGTClass);
begin
  if TGTBaseObject(FObj) <> nil then
    UnlinkObject(FObj);
  FObj := AObj;
  if TGTBaseObject(FObj) <> nil then
    LinkObject(FObj);
  FOnAssigned.Call(Self);
end;

procedure TGTReference.UnlinkObject(const AObj: TGTClass);
begin
  AObj.OnChange.UnregisterHandler(@OnChange.Call);
  AObj.OnDestruction.UnregisterHandler(@ObjDeleting);
end;

procedure TGTReference.BeforeDestruction;
begin
  // TGTReference should be transparent, so do not send an event here.
end;

procedure TGTReference.LoadFromXML(const XMLNode: TxmlNode;
  Context: IGTLoaderContext);
var
  Ctx: IGTLoaderContext2;
  ID: TGTReferenceID;
  Cls: TGTBaseClass;
begin
  ID := StrToQWord(XMLNode.Content);
  if (ID = NULL) then
  begin
    if (XMLNode.Attribute['refclass'] <> '') then
    begin
      Cls := FindGTClass(XMLNode.Attribute['refclass']);
      if Cls = nil then
        raise EGTXMLInvalidProperty.CreateFmt('Unknown class: ''%s''.', [XMLNode.Attribute['refclass']]);
      SetObj(TGTClass(Cls.Create));
    end
    else
    begin
      SetObj(nil);
    end
  end
  else
  begin
    if Context.QueryInterface(IGTLoaderContext2, Ctx) <> S_OK then
      RaiseMissingContext('IGTLoaderContext2');
    SetObj(TGTClass(Ctx.SolveReference(TGTBaseClass(TGTClass), ID)));
    if TGTBaseObject(FObj) = nil then
    begin
      FTmpID := ID;
      Context.RegisterFinalizationHandler(@FinalizeLoading);
    end;
  end;
end;

procedure TGTReference.SaveToXML(const XMLNode: TxmlNode);
var
  ID: TGTReferenceID;
begin
  if TGTBaseObject(FObj) <> nil then
  begin
    ID := FObj.ID;
    if ID = NULL then
    begin
      XMLNode.Attribute['id'] := IntToStr(NULL);
      FObj.SaveToXML(XMLNode);
      XMLNode.Attribute['refclass'] := FObj.ClassName;
    end
    else
      XMLNode.Attribute['id'] := IntToStr(FObj.ID)
  end
  else
    XMLNode.Attribute['id'] := IntToStr(NULL);
end;

{ TGTTemplate }

constructor TGTTemplate.Create(AClass: TGTBaseClass);
begin
  inherited Create;
  FInstance := AClass.Create;
  FName := 'Nameless template';
  FOnDestruction := TGTEventList.Create;
end;

destructor TGTTemplate.Destroy;
begin
  FOnDestruction.Call(Self);
  FOnDestruction.Free;
  FInstance.Free;
  inherited Destroy;
end;

function TGTTemplate.GetOnPropUpdate: TGTPropChangeEventList;
begin
  Result := FInstance.OnPropChange;
end;

procedure TGTTemplate.AssignTo(Dest: TPersistent);
begin
  if (Dest is TGTBaseObject) then
    Dest.Assign(FInstance)
  else
    inherited AssignTo(Dest);
end;

procedure TGTTemplate.Assign(Source: TPersistent);
begin
  if (Source is TGTBaseObject) then
    FInstance.Assign(Source)
  else if (Source is TGTTemplate) then
  begin
    with TGTTemplate(Source) do
    begin
      Self.FInstance.Assign(Instance);
      Self.FName := Name;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TGTTemplate.LoadFromXML(const XMLNode: TxmlNode; Context: IGTLoaderContext = nil);
var
  TmpValue: TxmlString;
  InstanceNode: TxmlNode;
begin
  TmpValue := XMLNode.NodeValue['id'];
  if TmpValue = '' then
    FID := NULL
  else
    FID := StrToQWord(TmpValue);
  FName := XMLNode.NodeValue['name'];
  InstanceNode := XMLNode.Node['instance'];
  FInstance := TGTObjectFactory.CreateGTClass(InstanceNode.Attribute['classname']);
  FInstance.LoadFromXML(InstanceNode, Context);
  if Context <> nil then
    Context.DeclareTemplateReference(FID, Self);
end;

procedure TGTTemplate.SaveToXML(const XMLNode: TxmlNode);
var
  InstanceNode: TXMLNode;
begin
  XMLNode.NodeValue['id'] := IntToStr(FID);
  XMLNode.NodeValue['name'] := FName;
  InstanceNode := XMLNode.NodeAC['instance'];
  InstanceNode.Attribute['classname'] := FInstance.ClassName;
  FInstance.SaveToXML(InstanceNode);
end;

{ TGTObjectFactory }

constructor TGTObjectFactory.Create;
begin

end;

class function TGTObjectFactory.CreateGTClass(AClassName: String
  ): TGTBaseObject;
var
  TmpClassType: TGTBaseClass;
begin
  TmpClassType := FindGTClass(AClassName);
  if TmpClassType = nil then
    raise EClassNotFound.CreateFmt('TGTBaseClass ''%s'' not found.', [AClassName]);
  Result := TmpClassType.Create;
end;

{ TGTTemplateManager }

function TGTTemplateManager.AllocateReference(const AObject: TGTTemplate
  ): TGTReferenceID;
begin
  Result:=inherited AllocateReference(AObject);
  if (Result <> NULL) then
    AObject.FID := Result;
end;

procedure TGTTemplateManager.DeclareReference(const AObject: TGTTemplate;
  const ReferenceID: TGTReferenceID);
begin
  inherited DeclareReference(AObject, ReferenceID);
  AObject.FID := ReferenceID;
end;

finalization
FreeAndNil(ClassHashTable);
FreeAndNil(ClassList);

end.

