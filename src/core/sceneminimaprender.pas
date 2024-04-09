unit SceneMiniMapRender;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Helpers, Scene, Level, SfmlAnimation, Monster, Spawner, Hero, SpriteEffects ;

type

  { TSceneMiniMapRender }

  TSceneMiniMapRender = class(TScene)
  private
    spr_block:array of TSfmlSprite ;
    spr_stair:array of TSfmlSprite ;
    spr_spawner:TSfmlSprite ;
    level:TLevel ;
    spawners:TUniList<TSpawner> ;
    portal:TSfmlAnimation ;
    scale:Integer ;
    cw,ch:Single ;
  public
    constructor Create(Ascale:Integer) ;
    function Init():Boolean ; override ;
    procedure SetLevel(leveln:Integer) ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses Math,
  SfmlUtils, CommonData, SceneGame ;

{ TSceneMiniMapRender }

constructor TSceneMiniMapRender.Create(Ascale: Integer);
begin
  scale:=Ascale ;
end;

function TSceneMiniMapRender.Init():Boolean ;
var i:Integer ;
begin
  cw:=CELL_WIDTH/scale ;
  ch:=CELL_HEIGHT/scale ;
  SetLength(spr_block,5) ;
  for i := 0 to Length(spr_block)-1 do begin
    spr_block[i]:=loadSprite('images'+PATH_SEP+'block'+IntToStr(i)+'.png');
    spr_block[i].Scale(1.0/scale,1.0/scale) ;
  end;
  SetLength(spr_stair,5) ;
  for i := 0 to Length(spr_stair)-1 do begin
    spr_stair[i]:=loadSprite('images'+PATH_SEP+'stair'+IntToStr(i)+'.png');
    spr_stair[i].Scale(1.0/scale,1.0/scale) ;
  end;
  spr_spawner:=loadSprite('images'+PATH_SEP+'spawner.png');
  spr_spawner.Origin:=SfmlVector2f(SfmlTextureGetSize(spr_spawner.Texture).x/2,0) ;
  spr_spawner.Scale(1.0/scale,1.0/scale) ;

  portal:=TSfmlAnimation.Create('images'+PATH_SEP+'portal.png',4,4);
  portal.Origin:=SfmlVector2f(SfmlTextureGetSize(portal.Texture).x/2,0) ;
  portal.Scale(1.0/scale,1.0/scale) ;

  level:=TLevel.Create ;
  spawners:=TUniList<TSpawner>.Create() ;

  Result:=True ;
end ;

procedure TSceneMiniMapRender.RenderFunc() ;
var i,j:Integer ;
    spawn:TSpawner ;
begin
  for i := 0 to level.getWidth()-1 do
    for j := 0 to level.getHeight-1 do begin
      if level.isBlockAt(i,j) then drawSprite(spr_block[level.getTexId(i,j)],cw*i,ch*j) ;
      if level.isStairAt(i,j) then drawSprite(spr_stair[level.getTexId(i,j)],cw*i,ch*j) ;
      if level.isFinishAt(i,j) then DrawSprite(portal, cw*(i+0.5), ch*j) ;
    end;

  for spawn in spawners do
    DrawSprite(spr_spawner,cw*spawn.getX()+cw/2,ch*spawn.getY()) ;
end ;

procedure TSceneMiniMapRender.SetLevel(leveln: Integer);
begin
  level.LoadFromFile('levels'+PATH_SEP+'level'+IntToStr(leveln)+'.dat');
  level.fillSpawns(TUniList<TObject>(spawners),TUniList<TObject>(nil)) ;
end;

procedure TSceneMiniMapRender.UnInit() ;
var i:Integer ;
begin
  for i := 0 to Length(spr_block)-1 do
    spr_block[i].Free ;
  for i := 0 to Length(spr_stair)-1 do
    spr_stair[i].Free ;
  level.Free ;
end ;

end.

