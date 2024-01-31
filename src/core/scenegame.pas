unit SceneGame;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Helpers, Scene, Level, SfmlAnimation ;

type

  TCommand = ( cmdNone, cmdStop, cmdLeft, cmdRight, cmdUp, cmdDown ) ;

  { TSceneGame }

  TSceneGame = class(TScene)
  private
    cursor:TSfmlSprite ;
    spr_block:TSfmlSprite ;
    spr_stair:TSfmlSprite ;
    spr_crystall:TSfmlSprite ;
    spr_icons,spr_icons_gray:array of TSfmlSprite ;
    level:TLevel ;
    leveln:Integer ;
    textLevel:TSfmlText ;
    waitbot:TSfmlSprite ;
    walkbot:TSfmlAnimation ;
    portal:TSfmlAnimation ;
    // Позиция игрока
    player_x:Single ;
    player_y:Single ;
    // Скорость игрока
    player_dx:Integer ;
    player_dy:Integer ;
    tek_cmd:TCommand ;
    ismirr:Boolean ;
    left_scale_bot:TSfmlVector2f ;
    right_scale_bot:TSfmlVector2f ;
    galop:TSfmlSound ;
    grab:TSfmlSound ;
    function isWayCorrect(x, y: Integer): Boolean;
    procedure setCmdToDxy();
    function fixXifCrossed(stopx, dx:Single):Boolean ;
    function fixYifCrossed(stopy, dy:Single):Boolean ;
  public
    constructor Create(Aleveln:Integer) ;
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses SfmlUtils, CommonData, SubSceneMenuFin, SubSceneMenuGame ;

const CELL_HEIGHT=40 ;
      CELL_WIDTH=40 ;
      PLAYER_SPEED = 5 ;

function cmd2sig(cmd:TCommand):Integer ;
begin
  case cmd of
    cmdNone: Result:=0;
    cmdStop: Result:=0;
    cmdLeft: Result:=-1;
    cmdRight: Result:=1;
    cmdUp: Result:=-1;
    cmdDown: Result:=1;
  end;
end;

function isDXDYRevers(dx1, dy1, dx2, dy2: Integer): Boolean;
begin
  if (dy1=0)and(dy2=0) then Result:=dx1=-dx2 else
  if (dx1=0)and(dx2=0) then Result:=dy1=-dy2 else
  Result:=False ;
end;

{ TSceneGame }

function TSceneGame.isWayCorrect(x, y: Integer): Boolean;
begin
  Result:=False ;
  if x<0 then Exit ;
  if y<0 then Exit ;
  if x>=level.getWidth() then Exit ;
  if y>=level.getHeight() then Exit ;
  if level.isBlockAt(x,y) then Exit ;
  Result:=True ;
end;

procedure TSceneGame.setCmdToDxy();
begin
  if tek_cmd<>cmdNone then begin
    case tek_cmd of
      cmdStop: begin
        player_dx:=0 ;
        player_dy:=0 ;
      end;
      cmdLeft: begin
        player_dx:=-1 ;
        player_dy:=0 ;
      end;
      cmdRight: begin
        player_dx:=1 ;
        player_dy:=0 ;
      end;
      cmdUp: begin
        player_dx:=0 ;
        player_dy:=-1 ;
      end;
      cmdDown: begin
        player_dx:=0 ;
        player_dy:=1 ;
      end;
    end;
    tek_cmd:=cmdNone ;
  end;
end;

constructor TSceneGame.Create(Aleveln:Integer);
begin
  leveln:=Aleveln ;
end;

function TSceneGame.fixXifCrossed(stopx, dx:Single):Boolean ;
var borderx:Single ;
begin
    if (player_dx=-1) then begin
        borderx := player_x-0 ;
        if (borderx+dx<stopx) then begin
            player_x := stopx;
            Result:=True ;
        end
        else
          Result:=False ;
    end
    else begin
        borderx := player_x+1 ;
        if (borderx+dx>stopx) then begin
            player_x := stopx-0 ;
            Result:=True ;
        end
        else
          Result:=False ;
    end ;
end ;

function TSceneGame.fixYifCrossed(stopy, dy:Single):Boolean ;
var bordery:Single ;
begin
    if (player_dy=-1) then begin
        bordery := player_y-0 ;
        if (bordery+dy<stopy) then begin
            player_y := stopy ;
            Result:=True ;
        end
        else
          Result:=False ;
    end
    else begin
        bordery := player_y+1 ;
        if (bordery+dy>stopy) then begin
            player_y := stopy-0 ;
            Result:=True ;
        end
        else
          Result:=False ;
    end ;
end ;

function TSceneGame.Init():Boolean ;
var i:Integer ;
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

  for i := 0 to Length(spr_icons_gray)-1 do
    convertSpriteTexture(spr_icons_gray[i],funcMakeGray) ;

  textLevel:=createText(TCommonData.Font,'LEVEL '+IntToStr(leveln+1),18,SfmlWhite) ;

  waitbot:=loadSprite('images'+PATH_SEP+'waitbot.png');
  waitbot.Origin:=SfmlVector2f(SfmlTextureGetSize(waitbot.Texture).x/2,29) ;
  waitbot.Scale(0.75,0.75) ;
  walkbot:=TSfmlAnimation.Create('images'+PATH_SEP+'walkbot.png',5,8);
  walkbot.Origin:=waitbot.Origin ;
  walkbot.Scale(0.75,0.75) ;
  walkbot.Play() ;

  portal:=TSfmlAnimation.Create('images'+PATH_SEP+'portal.png',4,4);
  portal.Origin:=SfmlVector2f(SfmlTextureGetSize(portal.Texture).x/2,0) ;
  portal.Play() ;

  galop:=TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'galop.ogg'));
  galop.Loop:=True ;
  galop.Stop() ;
  grab:=TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'grab.ogg'));

  level:=TLevel.Create ;
  level.LoadFromFile('levels'+PATH_SEP+'level'+IntToStr(leveln)+'.dat');

  tek_cmd:=cmdNone ;
  player_dx:=0 ;
  player_dy:=0 ;
  level.fillStartXY(player_x,player_y) ;
  ismirr:=False ;

  left_scale_bot:=SfmlVector2f(-0.75,0.75) ;
  right_scale_bot:=SfmlVector2f(0.75,0.75) ;

  Result:=True ;
end ;

function TSceneGame.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
    playermapx,playermapy:Integer ;
begin
  Result:=Normal ;
  for event in events do
    if (event.event.EventType = sfEvtKeyPressed) then begin
      if (event.event.key.code = sfKeyEscape) then begin
        subscene:=TSubSceneMenuGame.Create() ;
        Exit(TSceneResult.SetSubScene) ;
      end;
      if (event.event.key.code = sfKeyLeft) then tek_cmd:=cmdLeft ;
      if (event.event.key.code = sfKeyRight) then tek_cmd:=cmdRight ;
      if (event.event.key.code = sfKeyUp) then tek_cmd:=cmdUp ;
      if (event.event.key.code = sfKeyDown) then tek_cmd:=cmdDown ;
      if (event.event.key.code = sfKeySpace) then tek_cmd:=cmdStop ;
    end ;

  // Движение игрока
  // Смена направления
  if (tek_cmd=cmdDown)or(tek_cmd=cmdUp) then
        if isWayCorrect(Trunc(player_x),Trunc(player_y)+cmd2sig(tek_cmd)) then begin
            if player_dx=1 then begin
                if fixXifCrossed(Trunc(player_x),player_dx*PLAYER_SPEED*dt) then setCmdToDxy() ;
            end
            else begin
                if (fixXifCrossed(Trunc(player_x),player_dx*PLAYER_SPEED*dt)) then setCmdToDxy() ;
            end ;
        end ;

  if (tek_cmd=cmdLeft)or(tek_cmd=cmdRight) then
        if isWayCorrect(Trunc(player_x)+cmd2sig(tek_cmd),Trunc(player_y)) then begin
            if player_dy=1 then begin
                if fixYifCrossed(Trunc(player_y),player_dy*PLAYER_SPEED*dt) then setCmdToDxy() ;
            end
            else begin
                if (fixYifCrossed(Trunc(player_y),player_dy*PLAYER_SPEED*dt)) then setCmdToDxy() ;
            end ;
        end ;

  if tek_cmd=cmdStop then begin
    player_dx:=0 ;
    player_dy:=0 ;
  end ;

  // Торможение о стены
  if player_dx=-1 then
    if not isWayCorrect(Trunc(player_x)-1,Trunc(player_y)) then
      if fixXifCrossed(Trunc(player_x),player_dx*PLAYER_SPEED*dt) then player_dx:=0 ;
  if player_dx=1 then
    if not isWayCorrect(Trunc(player_x)+1,Trunc(player_y)) then
      if fixXifCrossed(Trunc(player_x),player_dx*PLAYER_SPEED*dt) then player_dx:=0 ;
  if player_dy=-1 then
    if not isWayCorrect(Trunc(player_x),Trunc(player_y)-1) then
      if fixYifCrossed(Trunc(player_y),player_dy*PLAYER_SPEED*dt) then player_dy:=0 ;
  if player_dy=1 then
    if not isWayCorrect(Trunc(player_x),Trunc(player_y)+1) then
      if fixYifCrossed(Trunc(player_y),player_dy*PLAYER_SPEED*dt) then player_dy:=0 ;

  player_x:=player_x+player_dx*PLAYER_SPEED*dt ;
  player_y:=player_y+player_dy*PLAYER_SPEED*dt ;

  playermapx:=Trunc(player_x+0.5) ;
  playermapy:=Trunc(player_y+0.5) ;
  // Поедание ячеек
  if level.isCrystallAt(playermapx,playermapy) then begin
    level.clearCell(playermapx,playermapy) ;
    grab.Play() ;
  end;

  if player_dx=-1 then ismirr:=True ;
  if player_dx=1 then ismirr:=False ;

  if (player_dx<>0)or(player_dy<>0) then begin
    if galop.Status<>sfPlaying then galop.Play() ;
  end
  else begin
    if galop.Status=sfPlaying then galop.Pause() ;
  end ;

  if level.isFinishAt(playermapx,playermapy) then
    if level.getCrystallCount()=0 then begin
      TCommonData.profile.MarkLevelCompleted(leveln) ;
      subscene:=TSubSceneMenuFin.Create(leveln,True) ;
      Exit(TSceneResult.SetSubScene) ;
    end;

  walkbot.Update(dt) ;
  portal.Update(dt) ;
end ;

procedure TSceneGame.RenderFunc() ;
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
      if level.isFinishAt(i,j) then DrawSprite(portal, CELL_WIDTH*(i+0.5), CELL_HEIGHT*j) ;
    end;

  for i := 0 to Length(spr_icons)-1 do
    if i>2 then
      drawSprite(spr_icons_gray[i],(CELL_WIDTH*23+wwidth)/2,85+70*i)
    else
      drawSprite(spr_icons[i],(CELL_WIDTH*23+wwidth)/2,85+70*i) ;

  DrawTextCentered(textLevel,(CELL_WIDTH*23+wwidth)/2,10) ;

  if ismirr then begin
    waitbot.ScaleFactor:=left_scale_bot ;
    walkbot.ScaleFactor:=left_scale_bot ;
  end
  else begin
    waitbot.ScaleFactor:=right_scale_bot ;
    walkbot.ScaleFactor:=right_scale_bot ;
  end;

  if (player_dx=0)and(player_dy=0) then
    DrawSprite(waitbot, CELL_WIDTH*player_x + 20, CELL_HEIGHT*player_y)
  else
    DrawSprite(walkbot, CELL_WIDTH*player_x + 20, CELL_HEIGHT*player_y) ;

  DrawSprite(cursor,mx,my) ;
end ;

procedure TSceneGame.UnInit() ;
begin
  Cursor.Free ;
  spr_block.Free ;
  spr_stair.Free ;
  spr_crystall.Free ;
  walkbot.Free ;
  waitbot.Free ;
  textLevel.Free ;
  galop.Free ;
  grab.Free ;
end ;

end.

