unit SceneLevelMenu;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers, SfmlAnimation, MenuKeyboardText ;

type

  { TSceneLevelMenu }

  TSceneLevelMenu = class(TScene)
  private
    logo:TSfmlSprite ;
    menu:TMenuKeyboardText ;
    procedure buildMenu() ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses SceneGame, SceneMainMenu, SfmlUtils, CommonData ;

function TSceneLevelMenu.Init():Boolean ;
begin
  logo:=loadSprite(TCommonData.languages.formatFileNameWithLang('images'+PATH_SEP+'intro.png'));
  logo.Position:=SfmlVector2f(0,0) ;

  menu:=TMenuKeyboardText.Create(TCommonData.selector,wwidth div 2-50,300,40,
    TCommonData.Font,26,SfmlWhite) ;
  buildMenu() ;
  overscene:=menu ;

  Result:=True ;
end ;

procedure TSceneLevelMenu.buildMenu;
var i:Integer ;
begin
  menu.clearItems() ;
  for I := 0 to TCommonData.profile.getAvailLevel() do
     menu.addItem(TCommonData.texts.getText('LEVEL')+' '+IntToStr(i+1)) ;
end;

function TSceneLevelMenu.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
begin
  Result:=Normal ;
  for event in events do
    if (event.event.EventType = sfEvtKeyPressed) then begin
      if (event.event.key.code = sfKeyEscape) then begin
        nextscene:=TSceneMainMenu.Create() ;
        Exit(TSceneResult.Switch) ;
      end;
      if (event.event.key.code in [sfKeySpace,sfKeyReturn]) then begin
        nextscene:=TSceneGame.Create(menu.getSelIndex()) ;
        Exit(TSceneResult.Switch) ;
      end;
    end ;

  TCommonData.selector.Update(dt) ;
end ;

procedure TSceneLevelMenu.RenderFunc() ;
begin
  window.Draw(logo) ;
end ;

procedure TSceneLevelMenu.UnInit() ;
begin
  logo.Free ;
  menu.Free ;
end ;

end.
