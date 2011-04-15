(*******************************************************************************
** File Name: gteditor.pas
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

For feedback and questions about Thorium Scripting Language please mail me,
Jonas Wielicki:
  j.wielicki@sotecware.net
*******************************************************************************)
unit GTEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, GTSession,
  Menus, ComCtrls, gtbase;

type
  TGTGUIAttachPoint = type Cardinal;

const
  GT_AP_CONTROL = $00000;
  GT_AP_CONTROL_ROOT = GT_AP_CONTROL or $100;

  GT_AP_MENU = $10000;
  GT_AP_MENU_EDIT = GT_AP_MENU or $100;
  GT_AP_MENU_EDIT_ACTIONS = GT_AP_MENU_EDIT or 1;
  GT_AP_MENU_EDIT_PREFERENCES = GT_AP_MENU_EDIT or 2;
  GT_AP_MENU_VIEW = GT_AP_MENU or $200;
  GT_AP_MENU_VIEW_PANELS = GT_AP_MENU_VIEW or 1;
  GT_AP_MENU_TOOLS = GT_AP_MENU or $300;

  GT_AP_TOOLBAR = $20000;

  GT_AP_PAGE = $30000;
  GT_AP_PAGE_LEFT = GT_AP_PAGE or $100;
  GT_AP_PAGE_RIGHT = GT_AP_PAGE or $200;

type
  TGTEditorCapability = (ecUndo, ecRedo, ecCut, ecCopy, ecPaste, ecDelete,
    ecSelectAll, ecMenuAppends, ecControlAppends, ecToolAppends, ecPageAppends);
  TGTEditorCapabilities = set of TGTEditorCapability;

  EGTEditorError = class (Exception);
    EGTEditorNeedsSession = class (EGTEditorError);

  { TGTEditor }

  TGTEditor = class(TCustomControl)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    procedure SessionDestroy(Sender: TObject);
  protected
    FEditorEnabled: Boolean;
    FInitialized: Boolean;
    FSession: TGTSession;
  protected
    procedure CleanupEditor; virtual;
    function GetCapabilities: TGTEditorCapabilities; virtual;
    procedure EnterSession(ASession: TGTSession); virtual;
    function ForceSession: TGTSession;
    procedure LeaveSession(ASession: TGTSession); virtual;
    procedure RaiseCannotEdit(AClass: TClass);
    procedure SetSession(ASession: TGTSession); virtual;
  public
    procedure BeforeDestruction; override;
    procedure DisableEditor; virtual;
    procedure DoCopy; virtual;
    procedure DoCut; virtual;
    procedure DoDelete; virtual;
    procedure DoPaste; virtual;
    procedure DoRedo; virtual;
    procedure DoSelectAll; virtual;
    procedure DoUndo; virtual;
    procedure EnableEditor; virtual;
    procedure InitEditor(const AObject: TGTBaseEditable); virtual;
    procedure QueryControlAppend(Target: TGTGUIAttachPoint;
      ParentControl: TWinControl); virtual;
    procedure QueryMenuAppend(Target: TGTGUIAttachPoint;
      ParentMenuItem: TMenuItem; StartIndex: Integer = -1); virtual;
    procedure QueryPageAppend(Target: TGTGUIAttachPoint;
      ParentPageControl: TPageControl); virtual;
    procedure QueryToolAppend(Target: TGTGUIAttachPoint;
      ParentToolBar: TToolBar); virtual;
    procedure UpdateView; virtual;
  published
    property Capabilities: TGTEditorCapabilities read GetCapabilities;
    property EditorEnabled: Boolean read FEditorEnabled;
    property Session: TGTSession read FSession write SetSession;
  end;

  TGTEditorClass = class of TGTEditor;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('',[TGTEditor]);
end;

{ TGTEditor }

constructor TGTEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TGTEditor.Destroy;
begin
  if FSession <> nil then
    LeaveSession(FSession);
  FSession := nil;
  inherited Destroy;
end;

procedure TGTEditor.SessionDestroy(Sender: TObject);
begin
  if FSession <> nil then
    LeaveSession(FSession);
  FSession := nil;
end;

procedure TGTEditor.CleanupEditor;
begin
  FInitialized := False;
end;

function TGTEditor.GetCapabilities: TGTEditorCapabilities;
begin
  Result := [];
end;

procedure TGTEditor.EnterSession(ASession: TGTSession);
begin
  FSession.OnDestroy.RegisterHandler(@SessionDestroy);
end;

function TGTEditor.ForceSession: TGTSession;
begin
  if FSession = nil then
    raise EGTEditorNeedsSession.CreateFmt('%s needs a session assigned to perform this operation.', [ClassName]);
  Result := FSession;
end;

procedure TGTEditor.LeaveSession(ASession: TGTSession);
begin
  FSession.OnDestroy.UnRegisterHandler(@SessionDestroy);
end;

procedure TGTEditor.RaiseCannotEdit(AClass: TClass);
begin
  raise EGTEditorError.CreateFmt('%s cannot edit an object of class %s.', [ClassName, AClass.ClassName]);
end;

procedure TGTEditor.SetSession(ASession: TGTSession);
begin
  if FSession <> nil then
    LeaveSession(FSession);
  FSession := ASession;
  if FSession <> nil then
    EnterSession(FSession);
end;

procedure TGTEditor.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if FInitialized then
    CleanupEditor;
end;

procedure TGTEditor.DisableEditor;
begin
  FEditorEnabled := False;
end;

procedure TGTEditor.DoCopy;
begin

end;

procedure TGTEditor.DoCut;
begin

end;

procedure TGTEditor.DoDelete;
begin

end;

procedure TGTEditor.DoPaste;
begin

end;

procedure TGTEditor.DoRedo;
begin

end;

procedure TGTEditor.DoSelectAll;
begin

end;

procedure TGTEditor.DoUndo;
begin

end;

procedure TGTEditor.EnableEditor;
begin
  FEditorEnabled := True;
end;

procedure TGTEditor.InitEditor(const AObject: TGTBaseEditable);
begin
  FInitialized := True;
end;

procedure TGTEditor.QueryControlAppend(Target: TGTGUIAttachPoint;
  ParentControl: TWinControl);
begin

end;

procedure TGTEditor.QueryMenuAppend(Target: TGTGUIAttachPoint;
  ParentMenuItem: TMenuItem; StartIndex: Integer);
begin

end;

procedure TGTEditor.QueryPageAppend(Target: TGTGUIAttachPoint;
  ParentPageControl: TPageControl);
begin

end;

procedure TGTEditor.QueryToolAppend(Target: TGTGUIAttachPoint;
  ParentToolBar: TToolBar);
begin

end;

procedure TGTEditor.UpdateView;
begin

end;

end.
