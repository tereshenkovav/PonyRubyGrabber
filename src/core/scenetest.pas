unit SceneTest;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,
  Helpers, Scene, Level ;

type

  { TSceneTest }

  TSceneTest = class(TScene)
  private
    cursor:TSfmlSprite ;
    spr_block:TSfmlSprite ;
    spr_stair:TSfmlSprite ;
    spr_crystall:TSfmlSprite ;
    spr_icons,spr_icons_gray:array of TSfmlSprite ;
    level:TLevel ;
    textLevel:TSfmlText ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses SfmlUtils, CommonData ;

const CELL_HEIGHT=40 ;
      CELL_WIDTH=40 ;

{ TSceneTest }

function TSceneTest.Init():Boolean ;
var i,x,y:Integer ;
    pimg:PSfmlImage ;
    c:TSfmlColor ;
    gr:Integer ;
begin
  Cursor:=loadSprite('images'+PATH_SEP+'cursor.png');
  Cursor.Origin:=SfmlVector2f(0,10) ;

  spr_block:=loadSprite('images'+PATH_SEP+'block.png');
  spr_stair:=loadSprite('images'+PATH_SEP+'stair.png');
  spr_crystall:=loadSprite('images'+PATH_SEP+'crystall.png');

  SetLength(spr_icons,6) ;
  spr_icons[0]:=loadSprite('images'+PATH_SEP+'applejack_ico.png',[sloCentered]);
  spr_icons[1]:=loadSprite('images'+PATH_SEP+'pinki_ico.png',[sloCentered]);
  spr_icons[2]:=loadSprite('images'+PATH_SEP+'rarity_ico.png',[sloCentered]);
  spr_icons[3]:=loadSprite('images'+PATH_SEP+'flatter_ico.png',[sloCentered]);
  spr_icons[4]:=loadSprite('images'+PATH_SEP+'rainbow_ico.png',[sloCentered]);
  spr_icons[5]:=loadSprite('images'+PATH_SEP+'twily_ico.png',[sloCentered]);
  for i := 0 to Length(spr_icons)-1 do
    spr_icons[i].Scale(0.75,0.75) ;

  SetLength(spr_icons_gray,6) ;
  spr_icons_gray[0]:=loadSprite('images'+PATH_SEP+'applejack_ico.png',[sloCentered]);
  spr_icons_gray[1]:=loadSprite('images'+PATH_SEP+'pinki_ico.png',[sloCentered]);
  spr_icons_gray[2]:=loadSprite('images'+PATH_SEP+'rarity_ico.png',[sloCentered]);
  spr_icons_gray[3]:=loadSprite('images'+PATH_SEP+'flatter_ico.png',[sloCentered]);
  spr_icons_gray[4]:=loadSprite('images'+PATH_SEP+'rainbow_ico.png',[sloCentered]);
  spr_icons_gray[5]:=loadSprite('images'+PATH_SEP+'twily_ico.png',[sloCentered]);
  for i := 0 to Length(spr_icons_gray)-1 do
    spr_icons_gray[i].Scale(0.75,0.75) ;

  for i := 0 to Length(spr_icons_gray)-1 do begin
    pimg:=SfmlTextureCopyToImage(spr_icons_gray[i].Texture) ;
    for x:=0 to SfmlImageGetSize(pimg).x-1 do
      for y:=0 to SfmlImageGetSize(pimg).y-1 do begin
        c:=SfmlImageGetPixel(pimg,x,y) ;
        gr:=(c.R+c.G+c.B) div 3 ;
        SfmlImageSetPixel(pimg,x,y,SfmlColorFromRGBA(gr,gr,gr,C.A)) ;
      end ;
    SfmlTextureUpdateFromImage(spr_icons_gray[i].Texture,pimg,0,0) ;
    SfmlImageDestroy(pimg) ;
  end;

  textLevel:=createText(TCommonData.Font,'LEVEL 1',18,SfmlWhite) ;

  level:=TLevel.Create ;
  level.LoadFromFile('levels'+PATH_SEP+'level1.dat');
  Result:=True ;
end ;

function TSceneTest.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
begin
  Result:=Normal ;
  for event in events do
    if (event.event.EventType = sfEvtKeyPressed) then begin
      if (event.event.key.code = sfKeyEscape) then Exit(TSceneResult.Close)
    end ;
end ;

procedure TSceneTest.RenderFunc() ;
var mx,my:Integer ;
    i,j:Integer ;

begin
  mx:=window.MousePosition.X ;
  my:=window.MousePosition.Y ;

  for i := 0 to level.getWidth()-1 do
    for j := 0 to level.getHeight-1 do begin
      if level.isBlockAt(i,j) then drawSprite(spr_block,CELL_WIDTH*i,CELL_HEIGHT*j) ;
      if level.isStairAt(i,j) then drawSprite(spr_stair,CELL_WIDTH*i,CELL_HEIGHT*j) ;
      if level.isCrystallAt(i,j) then drawSprite(spr_crystall,CELL_WIDTH*i,CELL_HEIGHT*j) ;
    end;

  for i := 0 to Length(spr_icons)-1 do
    if i>2 then
      drawSprite(spr_icons_gray[i],(CELL_WIDTH*23+wwidth)/2,85+70*i)
    else
      drawSprite(spr_icons[i],(CELL_WIDTH*23+wwidth)/2,85+70*i) ;

  DrawTextCentered(textLevel,(CELL_WIDTH*23+wwidth)/2,10) ;

  DrawSprite(cursor,mx,my) ;
end ;

procedure TSceneTest.UnInit() ;
begin
  Cursor.Free ;
  spr_block.Free ;
  spr_stair.Free ;
  spr_crystall.Free ;
  textLevel.Free ;
end ;

end.

