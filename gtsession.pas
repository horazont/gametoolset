unit GTSession;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gtbase, variants, typinfo,
  GTFileBase, contnrs, fgl, libtar, sharedlogger, GTFileUtils;

type
  EGTSessionError = class (Exception);

  TGTFileList = specialize TFPGList<TGTFileNode>;

  { TGTSession }

  TGTSession = class(TComponent, IGTLoaderContext)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FFileHash: TFPObjectHashTable;
    FFileList: TGTFileList;
    FFileName: String;
    FOnCreatedNode: TGTNodeEventList;
    FOnDeletingNode: TGTNodeEventList;
    FOnDestroy: TGTEventList;
    FOnEditObject: TGTEditObject;
    FOnFinalizeLoading: TGTEventList;
    FOnInspectObject: TGTInspectObject;
    FOnNewSession: TGTEventList;
    FOnSelectObject: TGTInspectObject;
    FOnUpdatedNode: TGTNodeEventList;
    FPendingFinalizations: TFPList;
    FTemplateManager: TGTTemplateManager;
    FWorkingDirectory: String;
    function GetFile(Index: Integer): TGTFileNode;
    function GetFileCount: Integer;
  protected
    procedure BeginLoading; virtual;
    procedure ClearFileList;
    class function EncapsulateInputStream(const AInput: TStream): TStream; virtual;
    class function EncapsulateOutputStream(const AOutput: TStream): TStream; virtual;
    procedure EndLoading; virtual;
    procedure FinalizeLoading; virtual;
    class function GetGTFileStreamClass: TGTFileStreamClass; virtual;
    function NewFile(AFileName: String; AClass: TGTFileClass): TGTFileNode;
    function PrepareTmpDir: String;
  protected // IGTLoaderContext
    procedure DeclareTemplateReference(const AReferenceID: TGTReferenceID; Instance: TGTTemplate);
    procedure RegisterFinalizationHandler(AHandler: TGTEvent);
    procedure RegisterFinalization(Instance: TObject; Prop: PPropInfo; ID: TGTReferenceID);
    function SolveTemplateReference(const AReferenceID: TGTReferenceID): TGTTemplate;
  public
    procedure BeforeDestruction; override;
    procedure BeginUpdate; virtual;
    procedure DoEditObject(ANode: TGTBaseEditable; AFrom: TObject = nil);
    procedure DoInspectObject(ANode: TGTBaseObject; AFrom: TObject = nil);
    procedure DoSelectObject(ANode: TGTBaseObject; AFrom: TObject = nil);
    procedure EndUpdate; virtual;
    procedure LoadFromDirectory(const ARootPath: String); virtual; abstract;
    procedure LoadFromFile(const AFileName: String); virtual;
    procedure LoadFromStream(const AStream: TStream); virtual;
    procedure SaveToDirectory(const ARootPath: String); virtual; abstract;
    procedure SaveToFile(const AFileName: String); virtual;
    procedure SaveToStream(const AStream: TStream); virtual;
    function TranslateFileName(ARelativeFileName: String): String; virtual;
  public
    property FileCount: Integer read GetFileCount;
    property FileName: String read FFileName write FFileName;
    property Files[Index: Integer]: TGTFileNode read GetFile;
    property OnCreatedNode: TGTNodeEventList read FOnCreatedNode;
    property OnDeletingNode: TGTNodeEventList read FOnDeletingNode;
    property OnDestroy: TGTEventList read FOnDestroy;
    property OnNewSession: TGTEventList read FOnNewSession;
    property OnUpdatedNode: TGTNodeEventList read FOnUpdatedNode;
    property WorkingDirectory: String read FWorkingDirectory write FWorkingDirectory;
  published
    property OnEditObject: TGTEditObject read FOnEditObject write FOnEditObject;
    property OnInspectObject: TGTInspectObject read FOnInspectObject write FOnInspectObject;
    property OnSelectObject: TGTInspectObject read FOnSelectObject write FOnSelectObject;
  end;

implementation

type
  TPendingFinalization = record
    Instance: TObject;
    Prop: PPropInfo;
    ID: TGTReferenceID;
  end;
  PPendingFinalization = ^TPendingFinalization;

{ TGTSession }

constructor TGTSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileList := TGTFileList.Create;
  FFileHash := TFPObjectHashTable.Create(True);
  FOnCreatedNode := TGTNodeEventList.Create;
  FOnDeletingNode := TGTNodeEventList.Create;
  FOnDestroy := TGTEventList.Create;
  FOnFinalizeLoading := nil;
  FOnInspectObject := nil;
  FOnNewSession := TGTEventList.Create;
  FOnSelectObject := nil;
  FOnUpdatedNode := TGTNodeEventList.Create;
  FTemplateManager := TGTTemplateManager.Create;
end;

destructor TGTSession.Destroy;
begin
  ClearFileList;
  FFileList.Free;
  FFileHash.Free;
  FOnCreatedNode.Free;
  FOnDeletingNode.Free;
  FOnDestroy.Free;
  FOnNewSession.Free;
  FOnUpdatedNode.Free;
  FTemplateManager.Free;
  inherited Destroy;
end;

function TGTSession.GetFile(Index: Integer): TGTFileNode;
begin
  Result := FFileList[Index];
end;

function TGTSession.GetFileCount: Integer;
begin
  Result := FFileList.Count;
end;

procedure TGTSession.BeginLoading;
begin
  if FOnFinalizeLoading <> nil then
    raise EInvalidOperation.Create('You must not nest BeginLoading calls.');
  FOnFinalizeLoading := TGTEventList.Create;
  FPendingFinalizations := TFPList.Create;
end;

procedure TGTSession.ClearFileList;
begin
  FFileList.Clear;
  FFileHash.Clear;
end;

class function TGTSession.EncapsulateInputStream(const AInput: TStream
  ): TStream;
begin
  Result := GetGTFileStreamClass.CreateReader(AInput);
end;

class function TGTSession.EncapsulateOutputStream(const AOutput: TStream
  ): TStream;
begin
  Result := GetGTFileStreamClass.CreateWriter(AOutput);
end;

procedure TGTSession.EndLoading;
var
  I: Integer;
begin
  FreeAndNil(FOnFinalizeLoading);
  if FPendingFinalizations <> nil then
  begin
    for I := 0 to FPendingFinalizations.Count - 1 do
      FreeMem(FPendingFinalizations[I]);
  end;
  FreeAndNil(FPendingFinalizations);
end;

procedure TGTSession.FinalizeLoading;
var
  I: Integer;
  Rec: PPendingFinalization;
  Obj: TObject;
begin
  FOnFinalizeLoading.Call(Self);
  for I := 0 to FPendingFinalizations.Count - 1 do
  begin
    Rec := PPendingFinalization(FPendingFinalizations[I]);
    Obj := SolveTemplateReference(Rec^.ID);
    SetObjectProp(Rec^.Instance, Rec^.Prop, Obj);
  end;
end;

class function TGTSession.GetGTFileStreamClass: TGTFileStreamClass;
begin
  Result := TGTFileStream;
end;

function TGTSession.NewFile(AFileName: String; AClass: TGTFileClass): TGTFileNode;
begin
  if FFileHash[AFileName] <> nil then
    raise EGTSessionError.CreateFmt('File ''%s'' already registered.', [AFileName]);
  Result := AClass.Create(AFileName);
  FFileHash[AFileName] := Result;
  FFileList.Add(Result);
end;

function TGTSession.PrepareTmpDir: String;
begin
  Result := '/tmp/'+ExtractFileName(ParamStr(0));
  if not ForceDirectories(Result) then
    raise EGTSessionError.CreateFmt('Could not create temporary directory ''%s''.', [Result]);
end;

procedure TGTSession.DeclareTemplateReference(
  const AReferenceID: TGTReferenceID; Instance: TGTTemplate);
begin
  FTemplateManager.DeclareReference(Instance, AReferenceID);
end;

procedure TGTSession.RegisterFinalizationHandler(AHandler: TGTEvent);
begin
  if not Assigned(FOnFinalizeLoading) then
    raise EInvalidOperation.Create('You may not call RegisterFinalizationHandler outside of a loading process.');
  FOnFinalizeLoading.RegisterHandler(AHandler);
end;

procedure TGTSession.RegisterFinalization(Instance: TObject; Prop: PPropInfo; ID: TGTReferenceID);
var
  Rec: PPendingFinalization;
begin
  if not Assigned(FPendingFinalizations) then
    raise EInvalidOperation.Create('You may not call RegisterFinalization outside of a loading process.');
  Rec := GetMem(SizeOf(TPendingFinalization));
  Rec^.Instance := Instance;
  Rec^.Prop := Prop;
  Rec^.ID := ID;
  FPendingFinalizations.Add(Rec);
end;

function TGTSession.SolveTemplateReference(const AReferenceID: TGTReferenceID
  ): TGTTemplate;
begin
  Result := FTemplateManager.SolveReference(AReferenceID);
end;

procedure TGTSession.BeforeDestruction;
begin
  inherited BeforeDestruction;
  FOnDestroy.Call(Self);
end;

procedure TGTSession.BeginUpdate;
begin
  FOnCreatedNode.BeginLock;
  FOnDeletingNode.BeginLock;
  FOnUpdatedNode.BeginLock;
end;

procedure TGTSession.DoEditObject(ANode: TGTBaseEditable; AFrom: TObject);
begin
  if not Assigned(FOnEditObject) then
    Exit;
  if AFrom = nil then
    FOnEditObject(Self, ANode)
  else
    FOnEditObject(AFrom, ANode);
end;

procedure TGTSession.DoInspectObject(ANode: TGTBaseObject; AFrom: TObject);
begin
  if not Assigned(FOnInspectObject) then
    Exit;
  if AFrom = nil then
    FOnInspectObject(Self, ANode)
  else
    FOnInspectObject(AFrom, ANode);
end;

procedure TGTSession.DoSelectObject(ANode: TGTBaseObject; AFrom: TObject);
begin
  if not Assigned(FOnSelectObject) then
    Exit;
  if AFrom = nil then
    FOnSelectObject(Self, ANode)
  else
    FOnSelectObject(AFrom, ANode);
end;

procedure TGTSession.EndUpdate;
begin
  FOnCreatedNode.EndLock;
  FOnUpdatedNode.EndLock;
  FOnDeletingNode.EndLock;
end;

procedure TGTSession.LoadFromFile(const AFileName: String);
var
  FS: TStream;
begin
  FS := EncapsulateInputStream(TFileStream.Create(AFileName, fmOpenRead));
  try
    LoadFromStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TGTSession.LoadFromStream(const AStream: TStream);
var
  OutFS: TFileStream;
  Archive: TTarArchive;
  DirRec: TTarDirRec;
  TarFileName: String;
  TargetDir: String;
begin
  TargetDir := PrepareTmpDir;
  Archive := TTarArchive.Create(AStream);
  try
    FillByte(DirRec, SizeOf(TTarDirRec), 0);
    while Archive.FindNext(DirRec) do
    begin
      if DirRec.FileType = ftNormal then
      begin
        TarFileName := DirRec.Name;
        if Length(TarFileName) = 0 then
          Continue;
        if (TarFileName[1] = '/') or (TarFileName[1] = PathDelim) then
          Delete(TarFileName, 1, 1);
        TarFileName := TargetDir + PathDelim + TarFileName;
        ForceDirectories(ExtractFileDir(TarFileName));
        OutFS := TFileStream.Create(TarFileName, fmCreate);
        try
          Archive.ReadFile(OutFS);
        finally
          OutFS.Free;
        end;
      end;
    end;
  finally
    Archive.Free;
  end;
  LoadFromDirectory(TargetDir + PathDelim);
  RemoveDir(TargetDir);
end;

procedure TGTSession.SaveToFile(const AFileName: String);
var
  FS: TStream;
begin
  FS := EncapsulateOutputStream(TFileStream.Create(AFileName, fmCreate));
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TGTSession.SaveToStream(const AStream: TStream);
var
  Archive: TTarWriter;
  TarFileName: String;
  TargetDir: String;

  procedure AddFiles(const ADir: String);
  var
    FindRec: TSearchRec;
    Success: LongInt;
  begin
    Success := FindFirst(ADir + PathDelim + '*', faDirectory or faHidden or faReadOnly or faArchive, FindRec);
    while Success = 0 do
    begin
      if (faDirectory and FindRec.Attr <> 0) and (FindRec.Name <> '.') and (FindRec.Name <> '..') then
        AddFiles(ADir + PathDelim + FindRec.Name)
      else if (faDirectory and FindRec.Attr = 0) then
      begin
        TarFileName := ADir + PathDelim + FindRec.Name;
        Archive.AddFile(TarFileName, Copy(TarFileName, Length(TargetDir) + 2, Length(TarFileName)));
      end;
      Success := FindNext(FindRec);
    end;
    FindClose(FindRec);
  end;

begin
  TargetDir := PrepareTmpDir;
  SaveToDirectory(TargetDir + PathDelim);
  Archive := TTarWriter.Create(AStream);
  try
    AddFiles(TargetDir);
    Archive.Finalize;
  finally
    Archive.Free;
  end;
  RemoveDir(TargetDir);
end;

function TGTSession.TranslateFileName(ARelativeFileName: String): String;
begin
  Result := FWorkingDirectory + ARelativeFileName;
end;

end.

