﻿unit SceneGame;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Helpers, Scene, Level, SfmlAnimation, Monster, Spawner, Hero, SpriteEffects ;

type

  TCommand = ( cmdNone, cmdStop, cmdLeft, cmdRight, cmdUp, cmdDown ) ;
  TTeleportType = (ttIn,ttOut) ;

  { TSceneGame }

  TSceneGame = class(TScene)
  private
    spr_block:array of TSfmlSprite ;
    spr_outmap:TSfmlSprite ;
    spr_stair:array of TSfmlSprite ;
    spr_crystall:TSfmlSprite ;
    spr_spawner:TSfmlSprite ;
    spr_circle:TSfmlSprite ;
    spr_circle_gray:TSfmlSprite ;
    spr_circle_mini:TSfmlSprite ;
    spr_shield:TSfmlAnimation ;
    spr_teleportation:TSfmlAnimation ;
    spr_icons,spr_icons_gray,spr_icons_mini:TUniDictionary<string,TSfmlSprite> ;
    spr_monsters:array of TSfmlSprite ;
    spr_monsters_w:array of Integer ;
    spr_heros_walk:TUniDictionary<string,TSfmlAnimation> ;
    spr_heros_wait:TUniDictionary<string,TSfmlSprite> ;
    level:TLevel ;
    leveln:Integer ;
    spawners:TUniList<TSpawner> ;
    textLevel:TSfmlText ;
    textHelp:TSfmlText ;
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
    teleport:TSfmlSound ;
    magic:TSfmlSound ;
    active_hero:THero ;
    hero_storage:TUniDictionary<string,Integer> ;
    active_actions:TUniList<THeroAction> ;
    time_teleport:Single ;
    tdir:TTeleportType ;
    iswin:Boolean ;
    time_exit:Single ;
    pool:TSpriteEffectPool ;
    flag_entered_menu:Boolean ;
    procedure setCmdToDxy();
    function fixXifCrossed(stopx, dx:Single):Boolean ;
    function fixYifCrossed(stopy, dy:Single):Boolean ;
    function getPlayerSpeed():Single ;
    procedure startTeleport(Atdir:TTeleportType) ;
    function isPonyActive():Boolean ;
    function isPonyVisible():Boolean ;
  public
    speedup:Boolean ;
    shield:Boolean ;
    monsters:TUniList<TMonster> ;
    constructor Create(Aleveln:Integer) ;
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
    procedure jumpHeroTo(newx,newy:Integer) ;
    procedure FocusChanged(isfocus: Boolean); override ;
  end;

const CELL_HEIGHT=40 ;
      CELL_WIDTH=40 ;

implementation
uses Math,
  SfmlUtils, CommonData, SubSceneMenuFin, SubSceneMenuGame, ProfileLevel ;

const PLAYER_SPEED = 3 ;
      SPEEDUP_K = 3 ;

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

{ TSceneGame }

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

procedure TSceneGame.startTeleport(Atdir: TTeleportType);
begin
  tdir:=Atdir ;
  time_teleport:=1.0 ;
  teleport.Play() ;
  spr_teleportation.PlayOnce() ;
end;

constructor TSceneGame.Create(Aleveln:Integer);
begin
  leveln:=Aleveln ;
  pool:=TSpriteEffectPool.Create ;
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
    spr:TSfmlSprite ;
    code:string ;
begin
  SetLength(spr_block,5) ;
  for i := 0 to Length(spr_block)-1 do
    spr_block[i]:=loadSprite('images'+PATH_SEP+'block'+IntToStr(i)+'.png');
  SetLength(spr_stair,5) ;
  for i := 0 to Length(spr_stair)-1 do
    spr_stair[i]:=loadSprite('images'+PATH_SEP+'stair'+IntToStr(i)+'.png');
  spr_outmap:=loadSprite('images'+PATH_SEP+'outmap.png');
  spr_crystall:=loadSprite('images'+PATH_SEP+'crystall.png');
  spr_spawner:=loadSprite('images'+PATH_SEP+'spawner.png');
  spr_spawner.Origin:=SfmlVector2f(SfmlTextureGetSize(spr_spawner.Texture).x/2,0) ;
  spr_circle:=loadSprite('images'+PATH_SEP+'circle.png',[sloCentered]);
  spr_circle_gray:=loadSprite('images'+PATH_SEP+'circle_gray.png',[sloCentered]);
  spr_circle_mini:=loadSprite('images'+PATH_SEP+'circle.png',[sloCentered]);
  spr_circle_mini.Scale(0.5,0.5);

  spr_icons:=TUniDictionary<string,TSfmlSprite>.Create() ;
  spr_icons_gray:=TUniDictionary<string,TSfmlSprite>.Create() ;
  spr_icons_mini:=TUniDictionary<string,TSfmlSprite>.Create() ;
  for code in THero.getHeroCodes() do begin
    spr:=loadSprite('images'+PATH_SEP+code+'_ico.png',[sloCentered]);
    spr.Scale(0.6,0.6) ;
    spr_icons.Add(code,spr) ;
    spr:=loadSprite('images'+PATH_SEP+code+'_ico.png',[sloCentered]);
    spr.Scale(0.6,0.6) ;
    convertSpriteTexture(spr,funcMakeGray) ;
    spr_icons_gray.Add(code,spr) ;
    spr:=loadSprite('images'+PATH_SEP+code+'_ico.png',[sloCentered]);
    spr.Scale(0.3,0.3) ;
    spr_icons_mini.Add(code,spr) ;
  end;

  SetLength(spr_monsters,3) ;
  spr_monsters[0]:=loadSprite('images'+PATH_SEP+'monster0.png');
  spr_monsters[1]:=loadSprite('images'+PATH_SEP+'monster1.png');
  spr_monsters[2]:=loadSprite('images'+PATH_SEP+'monster2.png');
  SetLength(spr_monsters_w,Length(spr_monsters)) ;
  for i := 0 to Length(spr_monsters)-1 do begin
    spr_monsters_w[i]:=SfmlTextureGetSize(spr_monsters[i].Texture).x ;
    spr_monsters[i].Origin:=SfmlVector2f(spr_monsters_w[i]/2,0) ;
  end;

  textLevel:=createText(TCommonData.Font,TCommonData.texts.getText('LEVEL')+' '+
    IntToStr(leveln+1),18,SfmlWhite) ;

  waitbot:=loadSprite('images'+PATH_SEP+'waitbot.png');
  waitbot.Origin:=SfmlVector2f(SfmlTextureGetSize(waitbot.Texture).x/2,29) ;
  walkbot:=TSfmlAnimation.Create('images'+PATH_SEP+'walkbot.png',5,8);
  walkbot.Origin:=waitbot.Origin ;
  walkbot.Play() ;

  spr_shield:=TSfmlAnimation.Create('images'+PATH_SEP+'shield.png',60,60,28,14);
  spr_shield.Origin:=SfmlVector2f(30,10) ;
  spr_shield.Play() ;

  spr_teleportation:=TSfmlAnimation.Create('images'+PATH_SEP+'teleportation.png',60,60,9,9);
  spr_teleportation.Origin:=SfmlVector2f(30,10) ;

  spr_heros_wait:=TUniDictionary<string,TSfmlSprite>.Create() ;
  spr_heros_walk:=TUniDictionary<string,TSfmlAnimation>.Create() ;

  for code in THero.getHeroCodes() do begin
    spr:=loadSprite('images'+PATH_SEP+code+'_wait.png');
    spr.Origin:=SfmlVector2f(SfmlTextureGetSize(spr.Texture).x/2,29) ;
    spr_heros_wait.Add(code,spr) ;
    spr:=TSfmlAnimation.Create('images'+PATH_SEP+code+'_walk.png',5,8);
    spr.Origin:=SfmlVector2f(SfmlTextureGetSize(spr.Texture).x/2,29) ;
    TSfmlAnimation(spr).Play() ;
    spr_heros_walk.Add(code,TSfmlAnimation(spr)) ;
  end;

  portal:=TSfmlAnimation.Create('images'+PATH_SEP+'portal.png',4,4);
  portal.Origin:=SfmlVector2f(SfmlTextureGetSize(portal.Texture).x/2,0) ;
  portal.Play() ;

  galop:=TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'galop.ogg'));
  galop.Loop:=True ;
  galop.Stop() ;
  galop.Volume:=IfThen(profile.IsSoundOn(),100,0) ;
  grab:=TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'grab.ogg'));
  grab.Volume:=IfThen(profile.IsSoundOn(),100,0) ;
  teleport:=TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'teleport.ogg'));
  teleport.Volume:=IfThen(profile.IsSoundOn(),100,0) ;
  magic:=TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'magic.ogg'));
  magic.Volume:=IfThen(profile.IsSoundOn(),100,0) ;

  level:=TLevel.Create ;
  level.LoadFromFile('levels'+PATH_SEP+'level'+IntToStr(leveln)+'.dat');

  if level.getTextData()<>'' then
    textHelp:=createText(TCommonData.Font,
      profile.getActionConfig().formatTextWithActionCodes(
        TCommonData.texts.getText(level.getTextData())),24,SfmlWhite) ;

  tek_cmd:=cmdNone ;
  player_dx:=0 ;
  player_dy:=0 ;
  level.fillStartXY(player_x,player_y) ;
  ismirr:=False ;

  monsters:=TUniList<TMonster>.Create() ;
  level.fillMonsters(TUniList<TObject>(monsters)) ;

  spawners:=TUniList<TSpawner>.Create() ;
  level.fillSpawns(TUniList<TObject>(spawners),TUniList<TObject>(monsters)) ;

  left_scale_bot:=SfmlVector2f(-0.75,0.75) ;
  right_scale_bot:=SfmlVector2f(0.75,0.75) ;

  active_hero:=THero.getNoHero() ;
  hero_storage:=TUniDictionary<string,Integer>.Create ;
  level.fillHeroStorage(hero_storage) ;

  active_actions:=TUniList<THeroAction>.Create() ;

  speedup:=False ;
  shield:=False ;
  Result:=True ;
  StartTeleport(ttIn) ;
  pool.Clear() ;

  flag_entered_menu:=False ;
end ;

function TSceneGame.isPonyActive: Boolean;
begin
  Result:=time_exit<=0 ;
end;

function TSceneGame.isPonyVisible: Boolean;
begin
  case tdir of
    ttIn: Result:=time_teleport<0.5 ;
    ttOut: Result:=time_teleport>0.5 ;
    else Result:=False ;
  end;
end;

procedure TSceneGame.jumpHeroTo(newx, newy: Integer);
begin
  player_x:=newx ;
  player_y:=newy ;
end;

procedure TSceneGame.FocusChanged(isfocus: Boolean);
begin
  if (isfocus) then begin
    if Galop.Status=sfPaused then Galop.Play ;
  end
  else
    Galop.Pause() ;
end;

function TSceneGame.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
    playermapx,playermapy:Integer ;
    m:TMonster ;
    spawn:TSpawner ;
    code:string ;
    i:Integer ;
    action:THeroAction ;
    ddt:Single ;
    actionname:string ;
begin
  Result:=Normal ;

  playermapx:=Trunc(player_x+0.5) ;
  playermapy:=Trunc(player_y+0.5) ;

  if isPonyActive() then begin
  // Все активности, связанные с обработкой пони
    for event in events do begin
      // Сначала системные действия
      if (event.event.EventType = sfEvtKeyPressed) then begin
        if (event.event.key.code = sfKeyEscape) then begin
          subscene:=TSubSceneMenuGame.Create(leveln) ;
          Galop.Pause;
          flag_entered_menu:=True ;
         Exit(TSceneResult.SetSubScene) ;
        end;
      end;

      // Обработка команд
      if not profile.getActionConfig().isMatchEvent(event.event,actionname) then Continue ;

      if actionname=ACTION_LEFT then tek_cmd:=cmdLeft ;
      if actionname=ACTION_RIGHT then tek_cmd:=cmdRight ;
      if actionname=ACTION_UP then tek_cmd:=cmdUp ;
      if actionname=ACTION_DOWN then tek_cmd:=cmdDown ;
      if actionname=ACTION_STOP then tek_cmd:=cmdStop ;
      code:='' ;
      if actionname=ACTION_TRANSFORM_0 then code:=THero.getHeroCodes()[0] ;
      if actionname=ACTION_TRANSFORM_1 then code:=THero.getHeroCodes()[1] ;
      if actionname=ACTION_TRANSFORM_2 then code:=THero.getHeroCodes()[2] ;
      if actionname=ACTION_TRANSFORM_3 then code:=THero.getHeroCodes()[3] ;
      if actionname=ACTION_TRANSFORM_4 then code:=THero.getHeroCodes()[4] ;
      if actionname=ACTION_TRANSFORM_5 then code:=THero.getHeroCodes()[5] ;
      if (code<>'')and active_hero.isNoHero() then
          if hero_storage[code]>0 then begin
            active_hero:=THero.Create(code) ;
            hero_storage[code]:=hero_storage[code]-1 ;
            teleport.Play() ;
          end ;
      if actionname=ACTION_USE then begin
        if not active_hero.isNoHero() then begin
          action:=active_hero.createAction() ;
          if action.Apply(level,playermapx,playermapy,IfThen(ismirr,-1,1),Self) then begin
            active_hero.Free ;
            active_hero:=THero.getNoHero() ;
            for i:=0 to active_actions.Count-1 do
              if active_actions[i].ClassType=action.ClassType then begin
                active_actions.Remove(active_actions[i]) ;
                break ;
              end;
            active_actions.Add(action) ;
            magic.Play() ;
          end
          else
            action.Free ;
        end;
      end ;
    end ;

  if tek_cmd=cmdStop then begin
    player_dx:=0 ;
    player_dy:=0 ;
  end ;

  ddt:=dt/10 ;
  for i := 1 to 10 do begin

  // Движение игрока
  // Смена направления
  if (tek_cmd=cmdDown)or(tek_cmd=cmdUp) then
        if level.isWayCorrect(Trunc(player_x),Trunc(player_y)+cmd2sig(tek_cmd)) then begin
            if player_dx=1 then begin
                if fixXifCrossed(Trunc(player_x),player_dx*getPlayerSpeed()*ddt) then setCmdToDxy() ;
            end
            else begin
                if (fixXifCrossed(Trunc(player_x),player_dx*getPlayerSpeed()*ddt)) then setCmdToDxy() ;
            end ;
        end ;

  if (tek_cmd=cmdLeft)or(tek_cmd=cmdRight) then
        if level.isWayCorrect(Trunc(player_x)+cmd2sig(tek_cmd),Trunc(player_y)) then begin
            if player_dy=1 then begin
                if fixYifCrossed(Trunc(player_y),player_dy*getPlayerSpeed()*ddt) then setCmdToDxy() ;
            end
            else begin
                if (fixYifCrossed(Trunc(player_y),player_dy*getPlayerSpeed()*ddt)) then setCmdToDxy() ;
            end ;
        end ;

  // Торможение о стены
  if player_dx=-1 then
    if not level.isWayCorrect(Trunc(player_x)-1,Trunc(player_y)) then
      if fixXifCrossed(Trunc(player_x),player_dx*getPlayerSpeed()*ddt) then player_dx:=0 ;
  if player_dx=1 then
    if not level.isWayCorrect(Trunc(player_x)+1,Trunc(player_y)) then
      if fixXifCrossed(Trunc(player_x),player_dx*getPlayerSpeed()*ddt) then player_dx:=0 ;
  if player_dy=-1 then
    if not level.isWayCorrect(Trunc(player_x),Trunc(player_y)-1) then
      if fixYifCrossed(Trunc(player_y),player_dy*getPlayerSpeed()*ddt) then player_dy:=0 ;
  if player_dy=1 then
    if not level.isWayCorrect(Trunc(player_x),Trunc(player_y)+1) then
      if fixYifCrossed(Trunc(player_y),player_dy*getPlayerSpeed()*ddt) then player_dy:=0 ;

  player_x:=player_x+player_dx*getPlayerSpeed()*ddt ;
  player_y:=player_y+player_dy*getPlayerSpeed()*ddt ;

  end;
  // Поедание ячеек
  if level.isCrystallAt(playermapx,playermapy) then begin
    level.clearCell(playermapx,playermapy) ;
    grab.Play() ;
  end;

  if level.isHeroIconAt(playermapx,playermapy) then begin
    hero_storage[level.getHeroIconAt(playermapx,playermapy)]:=
       hero_storage[level.getHeroIconAt(playermapx,playermapy)]+1;
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
        startTeleport(ttOut) ;
        iswin:=True ;
        time_exit:=1.5 ;
        galop.Stop() ;
        TProfileLevel(profile).MarkLevelCompleted(leveln) ;
    end;

  end; // Действия, связанные с активным пони

  for m in monsters do begin
    m.Update(dt,player_x,player_y) ;
    if (not shield)and(isPonyActive()) then
    if (Abs(player_x-m.getX())<0.5*(1+spr_monsters_w[m.getTypeID()]/CELL_WIDTH))and
       (playermapy=Trunc(m.getY()+0.5)) then begin
      startTeleport(ttOut) ;
      iswin:=False ;
      time_exit:=1.5 ;
      galop.Stop() ;
    end ;
  end;

  for spawn in spawners do
    spawn.Update(dt) ;

  walkbot.Update(dt) ;
  for code in spr_heros_walk.AllKeys do
    spr_heros_walk[code].Update(dt) ;

  i:=0 ;
  while i<active_actions.Count do begin
    active_actions[i].Update(dt) ;
    if active_actions[i].isTimeOut() then begin
      active_actions[i].Finish() ;
      active_actions[i].Free ;
      active_actions.Delete(i) ;
    end
    else
      Inc(i) ;
  end;

  pool.Update(dt) ;
  spr_shield.Update(dt) ;
  portal.Update(dt) ;
  spr_teleportation.Update(dt) ;
  if time_teleport>0 then time_teleport:=time_teleport-dt ;

  if time_exit>0 then begin
    time_exit:=time_exit-dt ;
    if time_exit<=0 then begin
      subscene:=TSubSceneMenuFin.Create(leveln,iswin) ;
      Exit(TSceneResult.SetSubScene) ;
    end;
  end ;

  // Возобновление после паузы при входе в меню
  if flag_entered_menu then begin
    if Galop.Status=sfPaused then Galop.Play ;
    flag_entered_menu:=False ;
  end;

end ;

function TSceneGame.getPlayerSpeed(): Single;
begin
  Result:=IfThen(speedup,SPEEDUP_K,1)*PLAYER_SPEED ;
end;

procedure TSceneGame.RenderFunc() ;
var i,j:Integer ;
    m:TMonster ;
    spawn:TSpawner ;
    spr_wait,spr_walk:TSfmlSprite ;
    code:string ;
    se:TSpriteEffect ;
    a0:Single ;
begin
  for i := 0 to level.getWidth()-1 do
    for j := 0 to level.getHeight-1 do begin
      if level.isOutMapAt(i,j) then drawSprite(spr_outmap,CELL_WIDTH*i,CELL_HEIGHT*j) ;
      if level.isBlockAt(i,j) then drawSprite(spr_block[level.getTexId(i,j)],CELL_WIDTH*i,CELL_HEIGHT*j) ;
      if level.isStairAt(i,j) then drawSprite(spr_stair[level.getTexId(i,j)],CELL_WIDTH*i,CELL_HEIGHT*j) ;
      if level.isFinishAt(i,j) then DrawSprite(portal, CELL_WIDTH*(i+0.5), CELL_HEIGHT*j) ;
      if level.isHeroIconAt(i,j) then begin
        se:=pool.findEffect(i*1024+j) ;
        if se=nil then begin
          a0:=Random(100)/100 ;
          se:=TSEMoveHarmonicVert.Create(spr_circle_mini,i*1024+j,5,3,a0) ;
          pool.addEffect(se) ;
        end;
        drawSpriteEffect(se,CELL_WIDTH*i+CELL_WIDTH/2,CELL_HEIGHT*j+CELL_HEIGHT/2) ;
        se:=pool.findEffect(-(i*1024+j)) ;
        if se=nil then begin
          se:=TSEMoveHarmonicVert.Create(spr_icons_mini[level.getHeroIconAt(i,j)],-(i*1024+j),5,3,a0) ;
          pool.addEffect(se) ;
        end;
        drawSpriteEffect(se,CELL_WIDTH*i+CELL_WIDTH/2,CELL_HEIGHT*j+CELL_HEIGHT/2) ;
      end;
      if level.isCrystallAt(i,j) then drawSprite(spr_crystall,CELL_WIDTH*i,CELL_HEIGHT*j) ;
    end;

  for i := 0 to THero.getHeroCodes().Count-1 do begin
    code:=THero.getHeroCodes()[i] ;
    if hero_storage[code]>0 then begin
      for j := 0 to hero_storage[code]-1 do
        drawSprite(spr_circle,(CELL_WIDTH*23+wwidth)/2+j*3,85+80*i+j*3) ;
      j:=hero_storage[code]-1 ;
      drawSprite(spr_icons[code],(CELL_WIDTH*23+wwidth)/2+j*3,85+80*i+j*3)
    end
    else begin
      drawSprite(spr_circle_gray,(CELL_WIDTH*23+wwidth)/2,85+80*i) ;
      drawSprite(spr_icons_gray[code],(CELL_WIDTH*23+wwidth)/2,85+80*i) ;
    end;
  end;

  DrawTextCentered(textLevel,(CELL_WIDTH*23+wwidth)/2,10) ;

  if textHelp<>nil then DrawTextCentered(textHelp,CELL_WIDTH*23/2,level.getTextPos()) ;

  for spawn in spawners do
    DrawSprite(spr_spawner,CELL_WIDTH*spawn.getX()+CELL_WIDTH/2,CELL_WIDTH*spawn.getY()) ;

  for m in monsters do
    DrawSpriteMirr(spr_monsters[m.getTypeID()],
      CELL_WIDTH*m.getX()+CELL_WIDTH/2,
      CELL_WIDTH*m.getY(),
      IfThen(m.isMirrHorz(),[MirrorHorz],[])) ;

// Переделать всё, добавив nohero в основной массив
  if active_hero.isNoHero() then begin
    spr_wait:=waitbot ;
    spr_walk:=walkbot ;
  end
  else begin
    spr_wait:=spr_heros_wait[active_hero.getCode()] ;
    spr_walk:=spr_heros_walk[active_hero.getCode()] ;
  end;

  if ismirr then begin
    spr_wait.ScaleFactor:=left_scale_bot ;
    spr_walk.ScaleFactor:=left_scale_bot ;
  end
  else begin
    spr_wait.ScaleFactor:=right_scale_bot ;
    spr_walk.ScaleFactor:=right_scale_bot ;
  end;

  if isPonyVisible() then begin
    if (player_dx=0)and(player_dy=0) then
      DrawSprite(spr_wait, CELL_WIDTH*player_x + CELL_WIDTH/2, CELL_HEIGHT*player_y)
    else
      DrawSprite(spr_walk, CELL_WIDTH*player_x + CELL_WIDTH/2, CELL_HEIGHT*player_y) ;
  end;

  if spr_teleportation.isPlayed() then
    DrawSprite(spr_teleportation, CELL_WIDTH*player_x + CELL_WIDTH/2, CELL_HEIGHT*player_y) ;

  if shield then DrawSprite(spr_shield,CELL_WIDTH*player_x + CELL_WIDTH/2, CELL_HEIGHT*player_y) ;
end ;

procedure TSceneGame.UnInit() ;
var i:Integer ;
begin
  for i := 0 to Length(spr_block)-1 do
    spr_block[i].Free ;
  for i := 0 to Length(spr_stair)-1 do
    spr_stair[i].Free ;
  spr_crystall.Free ;
  spr_outmap.Free ;
  walkbot.Free ;
  waitbot.Free ;
  textLevel.Free ;
  galop.Free ;
  grab.Free ;
  if textHelp<>nil then textHelp.Free ;
end ;

end.

