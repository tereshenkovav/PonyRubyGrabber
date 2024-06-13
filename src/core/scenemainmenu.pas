unit SceneMainMenu;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers, SfmlAnimation, MenuKeyboardText ;

type

  { TSceneMainMenu }

  TSceneMainMenu = class(TScene)
  private
    logo:TSfmlSprite ;
    menu:TMenuKeyboardText ;
    activeidx:Integer ;
    class var lastindex:Integer ;
    procedure buildMenu() ;
    procedure loadLogo() ;
  public
    constructor Create(Aactiveidx:Integer=0) ;
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses StrUtils,
 SceneLevelMenu, SceneAbout, SceneCtrlMenu,
 SfmlUtils, CommonData, Game ;

function TSceneMainMenu.Init():Boolean ;
begin
  loadLogo() ;

  menu:=TMenuKeyboardText.Create(TCommonData.selector,wwidth div 2-100,320,55,
    TCommonData.Font,32,SfmlWhite) ;
  buildMenu() ;
  menu.setIndex(activeidx) ;
  overscene:=menu ;

  Result:=True ;
end ;

procedure TSceneMainMenu.loadLogo;
begin
  if logo<>nil then logo.Free ;
  logo:=loadSprite(TCommonData.languages.formatFileNameWithLang('images'+PATH_SEP+'intro.png'));
  logo.Position:=SfmlVector2f(0,0) ;
end;

procedure TSceneMainMenu.buildMenu;
begin
  menu.clearItems() ;
  menu.addItem(TCommonData.texts.getText('MENU_START')) ;
  menu.addItem(TCommonData.texts.getText('MENU_LANG')+': '+TCommonData.languages.getCurrent().ToUpper()) ;
  menu.addItem(TCommonData.texts.getText('MENU_SOUND')+': '+IfThen(profile.IsSoundOn(),
    TCommonData.texts.getText('TEXT_ON'),TCommonData.texts.getText('TEXT_OFF'))) ;
  menu.addItem(TCommonData.texts.getText('MENU_FULLSCR')+': '+IfThen(profile.isFullScreen(),
    TCommonData.texts.getText('TEXT_ON'),TCommonData.texts.getText('TEXT_OFF'))) ;
  menu.addItem(TCommonData.texts.getText('MENU_CTRL')) ;
  menu.addItem(TCommonData.texts.getText('MENU_ABOUT')) ;
  menu.addItem(TCommonData.texts.getText('MENU_EXIT')) ;
end;

constructor TSceneMainMenu.Create(Aactiveidx: Integer);
begin
  if lastindex<>-1 then begin
    activeidx:=lastindex ;
    lastindex:=0 ;
  end
  else
    activeidx:=Aactiveidx ;
end;

function TSceneMainMenu.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
begin
  Result:=Normal ;
  for event in events do
    if (event.event.EventType = sfEvtKeyPressed) then begin
      if (event.event.key.code = sfKeyEscape) then
        Exit(TSceneResult.Close) ;
      if (event.event.key.code in [sfKeySpace,sfKeyReturn]) then begin
        case menu.getSelIndex() of
          0: begin
            nextscene:=TSceneLevelMenu.Create() ;
            Exit(TSceneResult.Switch) ;
          end;
          1: begin
            TCommonData.languages.switchCurrent() ;
            TCommonData.reloadTexts() ;
            newwindowtitle:=TCommonData.texts.getText('GAME_TITLE') ;
            loadLogo() ;
            buildMenu() ;
            Exit(TSceneResult.SetWindowTitle) ;
          end;
          2: begin
            profile.switchSoundOn() ;
            buildMenu() ;
          end;
          3: begin
            profile.switchFullScreen() ;
            nextscene:=TSceneMainMenu.Create(menu.getSelIndex()) ;
            Exit(TSceneResult.RebuildWindow) ;
          end;
          4: begin
            nextscene:=TSceneCtrlMenu.Create() ;
            lastindex:=menu.getSelIndex() ;
            Exit(TSceneResult.Switch) ;
          end;
          5: begin
            nextscene:=TSceneAbout.Create() ;
            lastindex:=menu.getSelIndex() ;
            Exit(TSceneResult.Switch) ;
          end;
          6: Exit(TSceneResult.Close) ;
        end;
      end;

    end ;

  TCommonData.selector.Update(dt) ;
end ;

procedure TSceneMainMenu.RenderFunc() ;
begin
  window.Draw(logo) ;
end ;

procedure TSceneMainMenu.UnInit() ;
begin
  logo.Free ;
  menu.Free ;
end ;

initialization

  TSceneMainMenu.lastindex:=-1 ;

end.
