unit SceneLevelMenu;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers, SfmlAnimation, MenuKeyboardTextIcon ;

type

  { TSceneLevelMenu }

  TSceneLevelMenu = class(TScene)
  private
    logo:TSfmlSprite ;
    menu:TMenuKeyboardTextIcon ;
    procedure buildMenu() ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses SceneGame, SceneMainMenu, SfmlUtils, CommonData,
  ProfileLevel ;

function TSceneLevelMenu.Init():Boolean ;
begin
  logo:=loadSprite(TCommonData.languages.formatFileNameWithLang('images'+PATH_SEP+'intro.png'));
  logo.Position:=SfmlVector2f(0,0) ;

  menu:=TMenuKeyboardTextIcon.Create(110,300,160,140,6,
    TCommonData.Font,22,SfmlWhite) ;
  buildMenu() ;
  overscene:=menu ;

  Result:=True ;
end ;

procedure TSceneLevelMenu.buildMenu;
var i:Integer ;
begin
  menu.clearItems() ;
  for i := 0 to TProfileLevel(profile).getAvailLevel() do
     menu.addItem(TCommonData.texts.getText('LEVEL')+' '+IntToStr(i+1),
     TCommonData.minimaps[i]) ;
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
