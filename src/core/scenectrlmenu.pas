unit SceneCtrlMenu;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers, SfmlAnimation, MenuKeyboardText ;

type

  { TSceneCtrlMenu }

  TSceneCtrlMenu = class(TScene)
  private
    logo:TSfmlSprite ;
    menu:TMenuKeyboardText ;
    active_idx:Integer ;
    procedure buildMenu() ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses SceneGame, SceneMainMenu, SfmlUtils, CommonData,
  StrUtils ;

function TSceneCtrlMenu.Init():Boolean ;
begin
  logo:=loadSprite(TCommonData.languages.formatFileNameWithLang('images'+PATH_SEP+'intro.png'));
  logo.Position:=SfmlVector2f(0,0) ;

  active_idx:=-1 ;

  menu:=TMenuKeyboardText.Create(TCommonData.selector,wwidth div 2-150,290,34,
    TCommonData.Font,24,SfmlWhite) ;
  buildMenu() ;
  overscene:=menu ;

  Result:=True ;
end ;

procedure TSceneCtrlMenu.buildMenu;
var i:Integer ;
begin
  menu.clearItems() ;
  for i := 0 to profile.getActionConfig().Count-1 do
     menu.addItem(TCommonData.texts.getText('ACTION_'+
       profile.getActionConfig().getActionName(i))+': '+
       IfThen(active_idx=i,'_',profile.getActionConfig().getActionView(i))) ;
end;

function TSceneCtrlMenu.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
begin
  Result:=Normal ;
  for event in events do
    if (event.event.EventType = sfEvtKeyPressed) then begin
      if (event.event.key.code = sfKeyEscape) then begin
        nextscene:=TSceneMainMenu.Create() ;
        Exit(TSceneResult.Switch) ;
      end;
      if active_idx=-1 then begin
        if (event.event.key.code in [sfKeySpace,sfKeyReturn]) then begin
          active_idx:=menu.getSelIndex() ;
          buildMenu() ;
        end;
      end
      else begin
        profile.getActionConfig().setActionByEvent(active_idx,event.event) ;
        profile.Save() ;
        active_idx:=-1 ;
        buildMenu() ;
      end;
    end ;

  TCommonData.selector.Update(dt) ;
end ;

procedure TSceneCtrlMenu.RenderFunc() ;
begin
  window.Draw(logo) ;
end ;

procedure TSceneCtrlMenu.UnInit() ;
begin
  logo.Free ;
  menu.Free ;
end ;

end.
