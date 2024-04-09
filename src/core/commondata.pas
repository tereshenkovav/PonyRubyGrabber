﻿unit CommonData;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,
  Profile, SfmlAnimation, Texts, Languages, ActionConfig ;

type

  { TCommonData }

  TCommonData = class
  private
  public
    class var Font:TSfmlFont ;
    class var selector:TSfmlAnimation ;
    class var texts:TTexts ;
    class var languages:TLanguages ;
    class var minimaps:array of TSfmlSprite ;
    class function Init():Boolean ;
    class procedure reloadTexts() ;
    class procedure preloadMiniMaps() ;
    class procedure UnInit() ;
  end;

const ACTION_LEFT = 'left' ;
const ACTION_RIGHT = 'right' ;
const ACTION_UP = 'up' ;
const ACTION_DOWN = 'down' ;
const ACTION_STOP = 'stop' ;
const ACTION_USE = 'use' ;
const ACTION_TRANSFORM_0 = 'transform_0' ;
const ACTION_TRANSFORM_1 = 'transform_1' ;
const ACTION_TRANSFORM_2 = 'transform_2' ;
const ACTION_TRANSFORM_3 = 'transform_3' ;
const ACTION_TRANSFORM_4 = 'transform_4' ;
const ACTION_TRANSFORM_5 = 'transform_5' ;

implementation
uses SfmlUtils, Helpers, Scene,
  SceneMiniMapRender, Level ;

{ TCommonData }

class function TCommonData.Init():Boolean ;
var i:Integer ;
begin
  Font:=TSfmlFont.Create('fonts'+PATH_SEP+'arial.ttf');
  selector:=TSfmlAnimation.Create('images'+PATH_SEP+'walkbot.png',5,8);
  selector.Scale(0.75,0.75) ;
  selector.Origin:=SfmlVector2f(100,30) ;
  selector.Play() ;
  languages:=TLanguages.Create() ;
  languages.loadFromFile('texts'+PATH_SEP+'languages');
  languages.setCurrentByFile('texts'+PATH_SEP+'deflang');
  texts:=TTexts.Create() ;
  reloadTexts() ;
  preloadMiniMaps() ;

  Result:=True ;
end ;

class procedure TCommonData.preloadMiniMaps;
var scene:TSceneMiniMapRender ;
    texdraw,tex:TSfmlRenderTexture ;
    spr:TSfmlSprite ;
    i:Integer ;
    w,h:Integer ;
const MAPSCALE = 8 ;
begin
  texdraw:=TSfmlRenderTexture.Create(1024,768) ;

  SetLength(minimaps,TLevel.getMaxLevel('levels')+1) ;
  scene:=TSceneMiniMapRender.Create() ;
  scene.setWindow(texdraw,1024,768) ;
  scene.Init() ;
  spr:=TSfmlSprite.Create() ;
  spr.Scale(1.0/MAPSCALE,1.0/MAPSCALE) ;
  spr.Position:=SfmlVector2f(0,0) ;

  w:=1024 div MAPSCALE ;
  h:=768 div MAPSCALE ;
  for i := 0 to TLevel.getMaxLevel('levels') do begin
    texdraw.Clear(SfmlColorFromRGB(64,64,64)) ;
    scene.SetLevel(i) ;
    scene.RenderFunc() ;
    texdraw.Display() ;

    spr.SetTexture(texdraw.Texture) ;
    tex:=TSfmlRenderTexture.Create(w,h) ;
    tex.Draw(spr) ;
    tex.Display() ;
    minimaps[i]:=TSfmlSprite.Create(tex.Texture) ;
    minimaps[i].Origin:=SfmlVector2f(w/2,0) ;
  end;
  texdraw.Free ;
  spr.Free ;
  scene.UnInit() ;
  scene.Free ;
end;

class procedure TCommonData.reloadTexts;
begin
  texts.loadFromFile('texts'+PATH_SEP+'texts.'+languages.getCurrent()) ;
end;

class procedure TCommonData.UnInit() ;
var i:Integer ;
begin
  Font.Free ;
  selector.Free ;
  texts.Free ;
  languages.Free ;
  for i:=0 to Length(minimaps)-1 do
    minimaps[i].Free ;
  SetLength(minimaps,0) ;
end ;

end.

