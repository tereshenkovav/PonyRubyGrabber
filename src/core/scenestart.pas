﻿unit SceneStart;

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
    t:Single ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses SceneMainMenu, SfmlUtils, CommonData ;

function TSceneStart.Init():Boolean ;
begin
  intro:=loadSprite('images'+PATH_SEP+'totalwin.png');
  intro.Position:=SfmlVector2f(0,0) ;
  intro.Color:=SfmlColorFromRGBA(255,255,255,0) ;
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

  t:=t+dt ;
  if t>5.0 then begin
    nextscene:=TSceneMainMenu.Create() ;
    Exit(TSceneResult.Switch) ;
  end;
end ;

procedure TSceneStart.RenderFunc() ;
begin
  if Trunc(100*t)<255 then
    intro.Color:=SfmlColorFromRGBA(255,255,255,Trunc(100*t))
  else
    intro.Color:=SfmlColorFromRGBA(255,255,255,255) ;
  window.Draw(intro) ;
end ;

procedure TSceneStart.UnInit() ;
begin
end ;

end.
