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
    procedure buildMenu() ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses SceneGame, SfmlUtils, CommonData ;

function TSceneMainMenu.Init():Boolean ;
begin
  logo:=loadSprite('images'+PATH_SEP+'intro.png');
  logo.Position:=SfmlVector2f(0,0) ;

  menu:=TMenuKeyboardText.Create(TCommonData.selector,wwidth div 2-50,350,70,
    TCommonData.Font,32,SfmlWhite) ;
  buildMenu() ;
  overscene:=menu ;

  Result:=True ;
end ;

procedure TSceneMainMenu.buildMenu;
begin
  menu.clearItems() ;
  menu.addItem('Start') ;
  menu.addItem('About') ;
  menu.addItem('Exit') ;
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
            nextscene:=TSceneGame.Create(0) ;
            Exit(TSceneResult.Switch) ;
          end;
          1: begin

          end;
          2: Exit(TSceneResult.Close) ;
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

end.
