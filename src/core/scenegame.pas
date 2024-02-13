unit SceneGame;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Helpers, Scene, Level, SfmlAnimation, Monster, Spawner, Hero ;

type

  TCommand = ( cmdNone, cmdStop, cmdLeft, cmdRight, cmdUp, cmdDown ) ;

  { TSceneGame }

  TSceneGame = class(TScene)
  private
    spr_block:TSfmlSprite ;
    spr_stair:TSfmlSprite ;
    spr_crystall:TSfmlSprite ;
    spr_spawner:TSfmlSprite ;
    spr_circle:TSfmlSprite ;
    spr_circle_gray:TSfmlSprite ;
    spr_shield:TSfmlAnimation ;
    spr_icons,spr_icons_gray:TUniDictionary<string,TSfmlSprite> ;
    spr_monsters:array of TSfmlSprite ;
    spr_monsters_w:array of Integer ;
    spr_heros_walk:TUniDictionary<string,TSfmlAnimation> ;
    spr_heros_wait:TUniDictionary<string,TSfmlSprite> ;
    level:TLevel ;
    leveln:Integer ;
    monsters:TUniList<TMonster> ;
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
    active_hero:THero ;
    hero_storage:TUniDictionary<string,Integer> ;
    active_actions:TUniList<THeroAction> ;
    procedure setCmdToDxy();
    function fixXifCrossed(stopx, dx:Single):Boolean ;
    function fixYifCrossed(stopy, dy:Single):Boolean ;
  public
    speedup:Boolean ;
    shield:Boolean ;
    constructor Create(Aleveln:Integer) ;
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
    procedure jumpHeroTo(newx,newy:Integer) ;
  end;

implementation
uses Math,
  SfmlUtils, CommonData, SubSceneMenuFin, SubSceneMenuGame ;

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
    spr:TSfmlSprite ;
    code:string ;
begin
  spr_block:=loadSprite('images'+PATH_SEP+'block.png');
  spr_stair:=loadSprite('images'+PATH_SEP+'stair.png');
  spr_crystall:=loadSprite('images'+PATH_SEP+'crystall.png');
  spr_spawner:=loadSprite('images'+PATH_SEP+'spawner.png');
  spr_spawner.Origin:=SfmlVector2f(SfmlTextureGetSize(spr_spawner.Texture).x/2,0) ;
  spr_circle:=loadSprite('images'+PATH_SEP+'circle.png',[sloCentered]);
  spr_circle_gray:=loadSprite('images'+PATH_SEP+'circle_gray.png',[sloCentered]);

  spr_icons:=TUniDictionary<string,TSfmlSprite>.Create() ;
  spr_icons_gray:=TUniDictionary<string,TSfmlSprite>.Create() ;
  for code in THero.getHeroCodes() do begin
    spr:=loadSprite('images'+PATH_SEP+code+'_ico.png',[sloCentered]);
    spr.Scale(0.6,0.6) ;
    spr_icons.Add(code,spr) ;
    spr:=loadSprite('images'+PATH_SEP+code+'_ico.png',[sloCentered]);
    spr.Scale(0.6,0.6) ;
    convertSpriteTexture(spr,funcMakeGray) ;
    spr_icons_gray.Add(code,spr) ;
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

  spr_heros_wait:=TUniDictionary<string,TSfmlSprite>.Create() ;
  spr_heros_walk:=TUniDictionary<string,TSfmlAnimation>.Create() ;

  spr:=loadSprite('images'+PATH_SEP+'pinkie_wait.png');
  spr.Origin:=SfmlVector2f(SfmlTextureGetSize(spr.Texture).x/2,29) ;
  spr_heros_wait.Add('pinkie',spr) ;
  spr:=TSfmlAnimation.Create('images'+PATH_SEP+'pinkie_walk.png',5,8);
  spr.Origin:=SfmlVector2f(SfmlTextureGetSize(spr.Texture).x/2,29) ;
  TSfmlAnimation(spr).Play() ;
  spr_heros_walk.Add('pinkie',TSfmlAnimation(spr)) ;

  spr:=loadSprite('images'+PATH_SEP+'rainbow_wait.png');
  spr.Origin:=SfmlVector2f(SfmlTextureGetSize(spr.Texture).x/2,29) ;
  spr_heros_wait.Add('rainbow',spr) ;
  spr:=TSfmlAnimation.Create('images'+PATH_SEP+'rainbow_walk.png',5,8);
  spr.Origin:=SfmlVector2f(SfmlTextureGetSize(spr.Texture).x/2,29) ;
  TSfmlAnimation(spr).Play() ;
  spr_heros_walk.Add('rainbow',TSfmlAnimation(spr)) ;

  spr:=loadSprite('images'+PATH_SEP+'twily_wait.png');
  spr.Origin:=SfmlVector2f(SfmlTextureGetSize(spr.Texture).x/2,29) ;
  spr_heros_wait.Add('twily',spr) ;
  spr:=TSfmlAnimation.Create('images'+PATH_SEP+'twily_walk.png',5,8);
  spr.Origin:=SfmlVector2f(SfmlTextureGetSize(spr.Texture).x/2,29) ;
  TSfmlAnimation(spr).Play() ;
  spr_heros_walk.Add('twily',TSfmlAnimation(spr)) ;

  spr:=loadSprite('images'+PATH_SEP+'applejack_wait.png');
  spr.Origin:=SfmlVector2f(SfmlTextureGetSize(spr.Texture).x/2,29) ;
  spr_heros_wait.Add('applejack',spr) ;
  spr:=TSfmlAnimation.Create('images'+PATH_SEP+'applejack_walk.png',5,8);
  spr.Origin:=SfmlVector2f(SfmlTextureGetSize(spr.Texture).x/2,29) ;
  TSfmlAnimation(spr).Play() ;
  spr_heros_walk.Add('applejack',TSfmlAnimation(spr)) ;

  spr:=loadSprite('images'+PATH_SEP+'rarity_wait.png');
  spr.Origin:=SfmlVector2f(SfmlTextureGetSize(spr.Texture).x/2,29) ;
  spr_heros_wait.Add('rarity',spr) ;
  spr:=TSfmlAnimation.Create('images'+PATH_SEP+'rarity_walk.png',5,8);
  spr.Origin:=SfmlVector2f(SfmlTextureGetSize(spr.Texture).x/2,29) ;
  TSfmlAnimation(spr).Play() ;
  spr_heros_walk.Add('rarity',TSfmlAnimation(spr)) ;

  portal:=TSfmlAnimation.Create('images'+PATH_SEP+'portal.png',4,4);
  portal.Origin:=SfmlVector2f(SfmlTextureGetSize(portal.Texture).x/2,0) ;
  portal.Play() ;

  galop:=TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'galop.ogg'));
  galop.Loop:=True ;
  galop.Stop() ;
  galop.Volume:=IfThen(TCommonData.soundon,100,0) ;
  grab:=TSfmlSound.Create(TSfmlSoundBuffer.Create('sounds'+PATH_SEP+'grab.ogg'));
  grab.Volume:=IfThen(TCommonData.soundon,100,0) ;

  level:=TLevel.Create ;
  level.LoadFromFile('levels'+PATH_SEP+'level'+IntToStr(leveln)+'.dat');

  if level.getTextData()<>'' then
    textHelp:=createText(TCommonData.Font,
      TCommonData.texts.getText(level.getTextData()),24,SfmlWhite) ;

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
end ;

procedure TSceneGame.jumpHeroTo(newx, newy: Integer);
begin
  player_x:=newx ;
  player_y:=newy ;
end;

function TSceneGame.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
    playermapx,playermapy:Integer ;
    m:TMonster ;
    spawn:TSpawner ;
    code:string ;
    i:Integer ;
    action:THeroAction ;
begin
  Result:=Normal ;

  playermapx:=Trunc(player_x+0.5) ;
  playermapy:=Trunc(player_y+0.5) ;

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
      if (event.event.Key.code in
        [sfKeyNum1,sfKeyNum2,sfKeyNum3,sfKeyNum4,sfKeyNum5,sfKeyNum6]) then begin
        code:=THero.getHeroCodes()[ord(event.event.Key.code)-ord(sfKeyNum1)] ;
        if active_hero.isNoHero() then
          if hero_storage[code]>0 then begin
            active_hero:=THero.Create(code) ;
            hero_storage[code]:=hero_storage[code]-1 ;
          end ;
      end ;
      if (event.event.Key.code = sfKeyLControl) then begin
        if not active_hero.isNoHero() then begin
          action:=active_hero.createAction() ;
          if action.Apply(level,playermapx,playermapy,IfThen(ismirr,-1,1),Self) then begin
            active_hero.Free ;
            active_hero:=THero.getNoHero() ;
            active_actions.Add(action) ;
          end
          else
            action.Free ;
        end;
      end ;
    end ;

  // Движение игрока
  // Смена направления
  if (tek_cmd=cmdDown)or(tek_cmd=cmdUp) then
        if level.isWayCorrect(Trunc(player_x),Trunc(player_y)+cmd2sig(tek_cmd)) then begin
            if player_dx=1 then begin
                if fixXifCrossed(Trunc(player_x),player_dx*PLAYER_SPEED*dt) then setCmdToDxy() ;
            end
            else begin
                if (fixXifCrossed(Trunc(player_x),player_dx*PLAYER_SPEED*dt)) then setCmdToDxy() ;
            end ;
        end ;

  if (tek_cmd=cmdLeft)or(tek_cmd=cmdRight) then
        if level.isWayCorrect(Trunc(player_x)+cmd2sig(tek_cmd),Trunc(player_y)) then begin
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
    if not level.isWayCorrect(Trunc(player_x)-1,Trunc(player_y)) then
      if fixXifCrossed(Trunc(player_x),player_dx*PLAYER_SPEED*dt) then player_dx:=0 ;
  if player_dx=1 then
    if not level.isWayCorrect(Trunc(player_x)+1,Trunc(player_y)) then
      if fixXifCrossed(Trunc(player_x),player_dx*PLAYER_SPEED*dt) then player_dx:=0 ;
  if player_dy=-1 then
    if not level.isWayCorrect(Trunc(player_x),Trunc(player_y)-1) then
      if fixYifCrossed(Trunc(player_y),player_dy*PLAYER_SPEED*dt) then player_dy:=0 ;
  if player_dy=1 then
    if not level.isWayCorrect(Trunc(player_x),Trunc(player_y)+1) then
      if fixYifCrossed(Trunc(player_y),player_dy*PLAYER_SPEED*dt) then player_dy:=0 ;

  player_x:=player_x+IfThen(speedup,2,1)*player_dx*PLAYER_SPEED*dt ;
  player_y:=player_y+IfThen(speedup,2,1)*player_dy*PLAYER_SPEED*dt ;

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

  for m in monsters do begin
    m.Update(dt,player_x,player_y) ;
    if not shield then
    if (Abs(player_x-m.getX())<0.5*(1+spr_monsters_w[m.getTypeID()]/CELL_WIDTH))and
       (playermapy=Trunc(m.getY()+0.5)) then begin
      subscene:=TSubSceneMenuFin.Create(leveln,False) ;
      Exit(TSceneResult.SetSubScene) ;
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

  spr_shield.Update(dt) ;
  portal.Update(dt) ;
end ;

procedure TSceneGame.RenderFunc() ;
var i,j:Integer ;
    m:TMonster ;
    spawn:TSpawner ;
    spr_wait,spr_walk:TSfmlSprite ;
    code:string ;
begin
  for i := 0 to level.getWidth()-1 do
    for j := 0 to level.getHeight-1 do begin
      if level.isBlockAt(i,j) then drawSprite(spr_block,CELL_WIDTH*i,CELL_HEIGHT*j) ;
      if level.isStairAt(i,j) then drawSprite(spr_stair,CELL_WIDTH*i,CELL_HEIGHT*j) ;
      if level.isCrystallAt(i,j) then drawSprite(spr_crystall,CELL_WIDTH*i,CELL_HEIGHT*j) ;
      if level.isFinishAt(i,j) then DrawSprite(portal, CELL_WIDTH*(i+0.5), CELL_HEIGHT*j) ;
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
  if (player_dx=0)and(player_dy=0) then
    DrawSprite(spr_wait, CELL_WIDTH*player_x + CELL_WIDTH/2, CELL_HEIGHT*player_y)
  else
    DrawSprite(spr_walk, CELL_WIDTH*player_x + CELL_WIDTH/2, CELL_HEIGHT*player_y) ;

  if shield then DrawSprite(spr_shield,CELL_WIDTH*player_x + CELL_WIDTH/2, CELL_HEIGHT*player_y) ;
end ;

procedure TSceneGame.UnInit() ;
begin
  spr_block.Free ;
  spr_stair.Free ;
  spr_crystall.Free ;
  walkbot.Free ;
  waitbot.Free ;
  textLevel.Free ;
  galop.Free ;
  grab.Free ;
  if textHelp<>nil then textHelp.Free ;
end ;

end.

