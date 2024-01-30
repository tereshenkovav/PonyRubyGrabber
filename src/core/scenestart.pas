unit SceneStart;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers ;

type

  { TSceneStart }

  TSceneStart = class(TScene)
  private
    intro:TSfmlSprite ;
    stage:Single ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses SceneMainMenu, SfmlUtils ;

function TSceneStart.Init():Boolean ;
begin
  intro:=loadSprite('images'+PATH_SEP+'intro.png');
  intro.Position:=SfmlVector2f(0,0) ;
  intro.Color:=SfmlColorFromRGBA(255,255,255,Trunc(stage)) ;
  Result:=True ;
end ;

function TSceneStart.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
begin
  Result:=Normal ;
  for event in events do
    if (event.event.EventType = sfEvtKeyPressed) then begin
      nextscene:=TSceneMainMenu.Create() ;
      Exit(TSceneResult.Switch) ;
    end ;

  if Trunc(stage)<255 then stage:=stage+100*dt else stage:=255 ;
end ;

procedure TSceneStart.RenderFunc() ;
begin
  intro.Color:=SfmlColorFromRGBA(255,255,255,Trunc(stage)) ;
  window.Draw(intro) ;
end ;

procedure TSceneStart.UnInit() ;
begin
end ;

end.
