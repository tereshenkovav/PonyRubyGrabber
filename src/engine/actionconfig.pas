unit ActionConfig;

interface

uses
  Classes, SysUtils,
  SfmlWindow,
  Helpers ;

type
  TActionType = (atKey,atMouseButton) ;

  TActionInfo = record
  private
  public
    actionname:string ;
    actiontype:TActionType ;
    def_key:TSfmlKeyCode ;
    def_mousebutton:TSfmlMouseButton ;
    key:TSfmlKeyCode ;
    mousebutton:TSfmlMouseButton ;
    isset:Boolean ;
    class operator Equal(a: TActionInfo; b: TActionInfo): Boolean;
  end;

  { TActionConfig }

  TActionConfig = class
  private
    actions:TUniList<TActionInfo> ;
  public
    procedure addAction(actionname:string; key:TSfmlKeyCode) ; overload ;
    procedure addAction(actionname:string; mousebutton:TSfmlMouseButton) ; overload ;
    function isMatchEvent(const event:TSfmlEvent; var actionname:string):Boolean ;
    constructor Create() ;
    destructor Destroy() ; override ;
  end;

implementation

{ TActionConfig }

constructor TActionConfig.Create();
begin
  actions:=TUniList<TActionInfo>.Create() ;
end ;

destructor TActionConfig.Destroy();
begin
  actions.Free ;
  inherited Destroy();
end;

function TActionConfig.isMatchEvent(const event: TSfmlEvent;
  var actionname: string): Boolean;
var action:TActionInfo ;
begin
  Result:=False ;
  for action in actions do begin
    if not action.isset then Continue ;
    case action.actiontype of
      atKey:
        if event.EventType=sfEvtKeyPressed then
          if event.Key.Code=action.key then begin
            actionname:=action.actionname ;
            Exit(True) ;
          end;
      atMouseButton:
        if event.EventType=sfEvtMouseButtonPressed then
          if event.MouseButton.Button=action.mousebutton then begin
            actionname:=action.actionname ;
            Exit(True) ;
          end;
    end;
  end;
end;

procedure TActionConfig.addAction(actionname:string; key:TSfmlKeyCode) ;
var keyinfo:TActionInfo ;
begin
  keyinfo.actionname:=actionname ;
  keyinfo.actiontype:=TActionType.atKey ;
  keyinfo.def_key:=key ;
  keyinfo.key:=key ;
  keyinfo.isset:=True ;
  actions.Add(keyinfo) ;
end;

procedure TActionConfig.addAction(actionname:string; mousebutton:TSfmlMouseButton) ;
var keyinfo:TActionInfo ;
begin
  keyinfo.actionname:=actionname ;
  keyinfo.actiontype:=TActionType.atMouseButton ;
  keyinfo.def_mousebutton:=mousebutton ;
  keyinfo.mousebutton:=mousebutton ;
  keyinfo.isset:=True ;
  actions.Add(keyinfo) ;
end ;

{ TSfmlEventEx }

class operator TActionInfo.Equal(a: TActionInfo; b: TActionInfo): Boolean;
begin
  Result:=a.actionname=b.actionname ;
end;

end.

