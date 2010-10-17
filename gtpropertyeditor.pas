unit GTPropertyEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, GTPropertyGrid, Graphics, contnrs,
  typinfo, GTBase;

type

  { TGTMetadataSuppliant }

  TGTMetadataSuppliant = class (TObject)
  public
    constructor Create(AEditor: TPropertyEditor);
  private
    FEditor: TPropertyEditor;
    FPropMetadataCached: Boolean;
    FCachedName: String;
    FCachedHint: String;
    function GetHint: String;
    function GetName: String;
  protected
    procedure CacheMetadata;
  public
    property Name: String read GetName;
    property Hint: String read GetHint;
  public
    procedure PropMeasureHeight(const NewValue: ansistring; ACanvas: TCanvas;
       var AHeight: Integer);
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
       AState: TPropEditDrawState);
  end;

  { TGTMetadataBasedPropertyEditor }

  TGTMetadataBasedPropertyEditor = class (TPropertyEditor)
  public
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer);
       override;
    destructor Destroy; override;
  protected
    FSuppliant: TGTMetadataSuppliant;
  public
    function GetName: shortstring; override;
    function GetHint(HintType: TPropEditHint; x, y: integer): string; override;
    procedure Initialize; override;
  end;

  { TGTMultiLineMetadataBasedPropertyEditor }

  TGTMultiLineMetadataBasedPropertyEditor = class (TGTMetadataBasedPropertyEditor)
  public
    procedure PropMeasureHeight(const NewValue: ansistring; ACanvas: TCanvas;
       var AHeight: Integer); override;
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
       AState: TPropEditDrawState); override;
  end;

  { TGTMetadataEnumEditor }

  TGTMetadataEnumEditor = class (TGTMetadataBasedPropertyEditor)
  public
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer);
       override;
    destructor Destroy; override;
  private
    FValueMapCached: Boolean;
    FStrToOrd: TFPDataHashTable;
    FOrdToStr: array of String;
  protected
    procedure CacheValueMap;
    function GetMinValue: Integer;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;

  { TGTMetadataSetElementEditor }

  TGTMetadataSetElementEditor = class (TSetElementPropertyEditor)
  public
    constructor Create(Parent: TPropertyEditor; AElement: Integer); overload;
  private
    FElement: Integer;
    FNameCached: Boolean;
    FCachedName: ShortString;
  protected
    procedure CacheName;
  public
    function GetName: shortstring; override;
  end;

  { TGTMetadataSetEditor }

  TGTMetadataSetEditor = class (TSetPropertyEditor)
  public
    constructor Create(Hook: TPropertyEditorHook; APropCount: Integer);
       override;
  private
    FPropMetadataCached: Boolean;
    FCachedName: String;
    FCachedHint: String;
  protected
    procedure CacheMetadata;
  public
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function GetName: shortstring; override;
    function GetHint(HintType: TPropEditHint; x, y: integer): string; override;
  end;

  (*
    TIntegerPropertyEditor,    // tkInteger
    TCharpropertyEditor,       // tkChar
    TEnumPropertyEditor,       // tkEnumeration
    TFloatPropertyEditor,      // tkFloat
    TSetPropertyEditor,        // tkSet
    TMethodPropertyEditor,     // tkMethod
    TStringPropertyEditor,     // tkSString
    TStringPropertyEditor,     // tkLString
    TStringPropertyEditor,     // tkAString
    TWideStringPropertyEditor, // tkWString
    TPropertyEditor,           // tkVariant
    nil,                       // tkArray
    nil,                       // tkRecord
    nil,                       // tkInterface
    TClassPropertyEditor,      // tkClass
    nil,                       // tkObject
    TPropertyEditor,           // tkWChar
    TBoolPropertyEditor,       // tkBool
    TInt64PropertyEditor,      // tkInt64
    TQWordPropertyEditor,      // tkQWord      *)

  { TGTCharPropertyEditor }

  TGTCharPropertyEditor = class (TCharPropertyEditor)
  public
    destructor Destroy; override;
  protected
    FSuppliant: TGTMetadataSuppliant;
  public
    function GetName: shortstring; override;
    function GetHint(HintType: TPropEditHint; x, y: integer): string; override;
    procedure Initialize; override;
  end;

  { TGTStringPropertyEditor }

  TGTStringPropertyEditor = class (TStringPropertyEditor)
  public
    destructor Destroy; override;
  protected
    FSuppliant: TGTMetadataSuppliant;
  public
    function GetName: shortstring; override;
    function GetHint(HintType: TPropEditHint; x, y: integer): string; override;
    procedure Initialize; override;
  end;

  { TGTWideStringPropertyEditor }

  TGTWideStringPropertyEditor = class (TWideStringPropertyEditor)
  public
    destructor Destroy; override;
  protected
    FSuppliant: TGTMetadataSuppliant;
  public
    function GetName: shortstring; override;
    function GetHint(HintType: TPropEditHint; x, y: integer): string; override;
    procedure Initialize; override;
  end;

  { TGTClassPropertyEditor }

  TGTClassPropertyEditor = class (TClassPropertyEditor)
  public
    destructor Destroy; override;
  protected
    FSuppliant: TGTMetadataSuppliant;
  public
    function GetName: shortstring; override;
    function GetHint(HintType: TPropEditHint; x, y: integer): string; override;
    procedure Initialize; override;
  end;

  { TGTBoolPropertyEditor }

  TGTBoolPropertyEditor = class (TBoolPropertyEditor)
  public
    destructor Destroy; override;
  protected
    FSuppliant: TGTMetadataSuppliant;
  public
    function GetName: shortstring; override;
    function GetHint(HintType: TPropEditHint; x, y: integer): string; override;
    procedure Initialize; override;
  end;

implementation

uses
  stwIntMap;

{ TGTMetadataSuppliant }

constructor TGTMetadataSuppliant.Create(AEditor: TPropertyEditor);
begin
  FPropMetadataCached := False;
  FEditor := AEditor;
end;

function TGTMetadataSuppliant.GetHint: String;
begin
  CacheMetadata;
  Result := FCachedHint;
end;

function TGTMetadataSuppliant.GetName: String;
begin
  CacheMetadata;
  Result := FCachedName;
end;

procedure TGTMetadataSuppliant.CacheMetadata;
var
  Lookup: PGTPropertyMetadata;
  OldName: String;
begin
  if FPropMetadataCached then
    Exit;
  OldName := FEditor.GetPropInfo^.Name;
  Lookup := LookupPropertyMetadata(FEditor.GetPropType, FEditor.GetComponent(0).ClassType, OldName);
  if Lookup = nil then
  begin
    FCachedName := OldName;
    FCachedHint := '';
  end
  else
  begin
    FCachedName := Lookup^.DisplayName;
    FCachedHint := Lookup^.Hint;
  end;
  FPropMetadataCached := True;
end;

procedure TGTMetadataSuppliant.PropMeasureHeight(const NewValue: ansistring;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := ACanvas.TextHeight('Yg') * 2 + 4;
end;

procedure TGTMetadataSuppliant.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; AState: TPropEditDrawState);
var
  Style : TTextStyle;
  Height: Integer;
begin
  FillChar(Style,SizeOf(Style),0);
  With Style do begin
    Alignment := taLeftJustify;
    Layout := tlCenter;
    Opaque := False;
    Clipping := True;
    ShowPrefix := False;
    WordBreak := False;
    SingleLine := True;
    ExpandTabs := True;
    SystemFont := False;
  end;
  Height := ACanvas.TextHeight('Yg');
  CacheMetadata;
  ACanvas.TextRect(ARect,ARect.Left+2,ARect.Top,FCachedName,Style);
  ACanvas.TextRect(ARect,ARect.Left+2,ARect.Top+2+Height,FCachedHint,Style);
end;

{ TGTMetadataBasedPropertyEditor }

constructor TGTMetadataBasedPropertyEditor.Create(Hook: TPropertyEditorHook;
  APropCount: Integer);
begin
  inherited Create(Hook, APropCount);
end;

destructor TGTMetadataBasedPropertyEditor.Destroy;
begin
  FreeAndNil(FSuppliant);
  inherited Destroy;
end;

function TGTMetadataBasedPropertyEditor.GetName: shortstring;
begin
  Result := FSuppliant.Name;
end;

function TGTMetadataBasedPropertyEditor.GetHint(HintType: TPropEditHint; x,
  y: integer): string;
begin
  Result:=inherited GetHint(HintType, x, y);
  if HintType in [pehName, pehNone, pehTree] then
    Result := FSuppliant.Hint;
end;

procedure TGTMetadataBasedPropertyEditor.Initialize;
begin
  FreeAndNil(FSuppliant);
  FSuppliant := TGTMetadataSuppliant.Create(Self);
end;

{ TGTMultiLineMetadataBasedPropertyEditor }

procedure TGTMultiLineMetadataBasedPropertyEditor.PropMeasureHeight(
  const NewValue: ansistring; ACanvas: TCanvas; var AHeight: Integer);
begin
  FSuppliant.PropMeasureHeight(NewValue, ACanvas, AHeight);
end;

procedure TGTMultiLineMetadataBasedPropertyEditor.PropDrawName(
  ACanvas: TCanvas; const ARect: TRect; AState: TPropEditDrawState);
begin
  FSuppliant.PropDrawName(ACanvas, ARect, AState);
end;

{ TGTMetadataEnumEditor }

constructor TGTMetadataEnumEditor.Create(Hook: TPropertyEditorHook;
  APropCount: Integer);
begin
  inherited Create(Hook, APropCount);
  FValueMapCached := False;
end;

destructor TGTMetadataEnumEditor.Destroy;
begin
  FStrToOrd.Free;
  inherited Destroy;
end;

procedure TGTMetadataEnumEditor.CacheValueMap;
var
  I: Integer;
  MinValue, MaxValue: Integer;
  TypeInfo: PTypeInfo;
  TypeData: PTypeData;
  CustomMatch: TGTEnumValueMetadata;
  Match: PGTEnumValueMetadata;
begin
  if FValueMapCached then
    Exit;
  TypeInfo := GetPropType;
  TypeData := GetTypeData(TypeInfo);
  MinValue := TypeData^.MinValue;
  MaxValue := TypeData^.MaxValue;
  FStrToOrd := TFPDataHashTable.CreateWith((MaxValue - MinValue) + 1, @RSHash);
  SetLength(FOrdToStr, (MaxValue - MinValue) + 1);
  for I := MinValue to MaxValue do
  begin
    Match := LookupEnumValueMetadata(TypeInfo, I);
    if Match = nil then
    begin
      Match := @CustomMatch;
      CustomMatch.DisplayName := GetEnumName(TypeInfo, I);
      CustomMatch.Hint := '';
    end;
    FStrToOrd.Add(Match^.DisplayName, Pointer((I - MinValue)+1)); // make sure that the lowest value is <> 0
    FOrdToStr[I - MinValue] := Match^.DisplayName;
  end;
  FValueMapCached:= True;
end;

function TGTMetadataEnumEditor.GetMinValue: Integer;
begin
  Result := GetTypeData(GetPropType)^.MinValue;
end;

function TGTMetadataEnumEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList];
end;

function TGTMetadataEnumEditor.GetValue: ansistring;
begin
  CacheValueMap;
  Result := FOrdToStr[GetOrdValue - GetMinValue];
end;

procedure TGTMetadataEnumEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  CacheValueMap;
  for I := 0 to High(FOrdToStr) do
  begin
    if FOrdToStr[I] <> '' then
      Proc(FOrdToStr[I]);
  end;
end;

procedure TGTMetadataEnumEditor.SetValue(const NewValue: ansistring);
var
  Idx: Integer;
begin
  CacheValueMap;
  Idx := Integer(FStrToOrd[NewValue]);
  if Idx = 0 then
    raise EPropertyError.CreateFmt('Invalid value: %s', [NewValue]);
  SetOrdValue((Idx-1)+GetMinValue);
end;

{ TGTMetadataSetElementEditor }

constructor TGTMetadataSetElementEditor.Create(Parent: TPropertyEditor;
  AElement: Integer);
begin
  inherited Create(Parent, AElement);
  FNameCached := False;
  FElement := AElement;
end;

procedure TGTMetadataSetElementEditor.CacheName;
var
  Match: PGTEnumValueMetadata;
  TypeInfo: PTypeInfo;
begin
  if FNameCached then
    Exit;
  FNameCached := True;
  TypeInfo := GetTypeData(ParentEditor.GetPropType)^.CompType;
  Match := LookupEnumValueMetadata(TypeInfo, FElement);
  if Match <> nil then
    FCachedName := Match^.DisplayName
  else
    FCachedName := GetEnumName(TypeInfo, FElement);
end;

function TGTMetadataSetElementEditor.GetName: shortstring;
begin
  CacheName;
  Result:=FCachedName;
end;

{ TGTMetadataSetEditor }

constructor TGTMetadataSetEditor.Create(Hook: TPropertyEditorHook;
  APropCount: Integer);
begin
  inherited Create(Hook, APropCount);
  FPropMetadataCached := False;
end;

procedure TGTMetadataSetEditor.CacheMetadata;
var
  Lookup: PGTPropertyMetadata;
begin
  if FPropMetadataCached then
    Exit;
  Lookup := LookupPropertyMetadata(GetPropType, GetComponent(0).ClassType, inherited GetName);
  if Lookup = nil then
  begin
    FCachedName := inherited GetName;
    FCachedHint := '';
  end
  else
  begin
    FCachedName := Lookup^.DisplayName;
    FCachedHint := Lookup^.Hint;
  end;
  FPropMetadataCached := True;
end;

procedure TGTMetadataSetEditor.GetProperties(Proc: TGetPropEditProc);
var
  I: Integer;
begin
  with GetTypeData(GetTypeData(GetPropType)^.CompType)^ do
    for I := MinValue to MaxValue do
      Proc(TGTMetadataSetElementEditor.Create(Self, I));
end;

function TGTMetadataSetEditor.GetName: shortstring;
begin
  CacheMetadata;
  Result:=FCachedName;
end;

function TGTMetadataSetEditor.GetHint(HintType: TPropEditHint; x, y: integer
  ): string;
begin
  Result:=inherited GetHint(HintType, x, y);
  if HintType in [pehName, pehNone, pehTree] then
  begin
    CacheMetadata;
    Result := FCachedHint;
  end;
end;

{ TGTCharPropertyEditor }

destructor TGTCharPropertyEditor.Destroy;
begin
  FreeAndNil(FSuppliant);
  inherited Destroy;
end;

function TGTCharPropertyEditor.GetName: shortstring;
begin
  Result := FSuppliant.Name;
end;

function TGTCharPropertyEditor.GetHint(HintType: TPropEditHint; x, y: integer): string;
begin
  Result:=inherited GetHint(HintType, x, y);
  if HintType in [pehName, pehNone, pehTree] then
    Result := FSuppliant.Hint;
end;

procedure TGTCharPropertyEditor.Initialize;
begin
  FreeAndNil(FSuppliant);
  FSuppliant := TGTMetadataSuppliant.Create(Self);
end;

{ TGTStringPropertyEditor }

destructor TGTStringPropertyEditor.Destroy;
begin
  FreeAndNil(FSuppliant);
  inherited Destroy;
end;

function TGTStringPropertyEditor.GetName: shortstring;
begin
  Result := FSuppliant.Name;
end;

function TGTStringPropertyEditor.GetHint(HintType: TPropEditHint; x, y: integer
  ): string;
begin
  Result:=inherited GetHint(HintType, x, y);
  if HintType in [pehName, pehNone, pehTree] then
    Result := FSuppliant.Hint;
end;

procedure TGTStringPropertyEditor.Initialize;
begin
  FreeAndNil(FSuppliant);
  FSuppliant := TGTMetadataSuppliant.Create(Self);
end;

{ TGTWideStringPropertyEditor }

destructor TGTWideStringPropertyEditor.Destroy;
begin
  FreeAndNil(FSuppliant);
  inherited Destroy;
end;

function TGTWideStringPropertyEditor.GetName: shortstring;
begin
  Result := FSuppliant.Name;
end;

function TGTWideStringPropertyEditor.GetHint(HintType: TPropEditHint; x,
  y: integer): string;
begin
  Result:=inherited GetHint(HintType, x, y);
  if HintType in [pehName, pehNone, pehTree] then
    Result := FSuppliant.Hint;
end;

procedure TGTWideStringPropertyEditor.Initialize;
begin
  FreeAndNil(FSuppliant);
  FSuppliant := TGTMetadataSuppliant.Create(Self);
end;

{ TGTClassPropertyEditor }

destructor TGTClassPropertyEditor.Destroy;
begin
  FreeAndNil(FSuppliant);
  inherited Destroy;
end;

function TGTClassPropertyEditor.GetName: shortstring;
begin
  Result := FSuppliant.Name;
end;

function TGTClassPropertyEditor.GetHint(HintType: TPropEditHint; x, y: integer
  ): string;
begin
  Result:=inherited GetHint(HintType, x, y);
  if HintType in [pehName, pehNone, pehTree] then
    Result := FSuppliant.Hint;
end;

procedure TGTClassPropertyEditor.Initialize;
begin
  FreeAndNil(FSuppliant);
  FSuppliant := TGTMetadataSuppliant.Create(Self);
end;

{ TGTBoolPropertyEditor }

destructor TGTBoolPropertyEditor.Destroy;
begin
  FreeAndNil(FSuppliant);
  inherited Destroy;
end;

function TGTBoolPropertyEditor.GetName: shortstring;
begin
  Result := FSuppliant.Name;
end;

function TGTBoolPropertyEditor.GetHint(HintType: TPropEditHint; x, y: integer
  ): string;
begin
  Result:=inherited GetHint(HintType, x, y);
  if HintType in [pehName, pehNone, pehTree] then
    Result := FSuppliant.Hint;
end;

procedure TGTBoolPropertyEditor.Initialize;
begin
  FreeAndNil(FSuppliant);
  FSuppliant := TGTMetadataSuppliant.Create(Self);
end;

initialization
PropClassMap[tkEnumeration] := TGTMetadataEnumEditor;
PropClassMap[tkSet] := TGTMetadataSetEditor;
PropClassMap[tkChar] := TGTCharPropertyEditor;
PropClassMap[tkString] := TGTStringPropertyEditor;
PropClassMap[tkLString] := TGTStringPropertyEditor;
PropClassMap[tkAString] := TGTStringPropertyEditor;
PropClassMap[tkWString] := TGTWideStringPropertyEditor;
PropClassMap[tkClass] := TGTClassPropertyEditor;
PropClassMap[tkBool] := TGTBoolPropertyEditor;

RegisterPropertyEditor(TypeInfo(TGTTemplate), nil, 'Template', nil);

end.

