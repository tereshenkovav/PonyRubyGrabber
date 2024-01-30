unit SceneMainMenu;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers, SfmlAnimation ;

type

  { TSceneMainMenu }

  TSceneMainMenu = class(TScene)
  private
    logo:TSfmlSprite ;
    walkbot:TSfmlAnimation ;
    items:TUniList<TSfmlText> ;
    selindex:Integer ;
    procedure buildMenu() ;
    procedure clearItems() ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses SceneGame, SfmlUtils, CommonData ;

const
  TOP = 350 ;
  HEIGHT = 70 ;

function TSceneMainMenu.Init():Boolean ;
begin
  logo:=loadSprite('images'+PATH_SEP+'intro.png');
  logo.Position:=SfmlVector2f(0,0) ;

  items:=TUniList<TSfmlText>.Create ;
  buildMenu() ;

  walkbot:=TSfmlAnimation.Create('images'+PATH_SEP+'walkbot.png',5,8);
  walkbot.Scale(0.75,0.75) ;
  walkbot.Origin:=SfmlVector2f(100,30) ;
  walkbot.Play() ;

  selindex:=0 ;
  Result:=True ;
end ;

procedure TSceneMainMenu.buildMenu;
begin
  clearItems() ;
  items.Add(createText(TCommonData.Font,'Start',32,SfmlWhite)) ;
  items.Add(createText(TCommonData.Font,'About',32,SfmlWhite)) ;
  items.Add(createText(TCommonData.Font,'Exit',32,SfmlWhite)) ;
end;

procedure TSceneMainMenu.clearItems;
var i:Integer ;
begin
  for i := 0 to items.Count-1 do
    items[i].Free ;
  items.Clear() ;
end;

function TSceneMainMenu.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
begin
  Result:=Normal ;
  for event in events do
    if (event.event.EventType = sfEvtKeyPressed) then begin
      if (event.event.key.code = sfKeyEscape) then
        Exit(TSceneResult.Close) ;
      if (event.event.key.code = sfKeyUp) then
        if selindex>0 then Dec(selindex) ;
      if (event.event.key.code = sfKeyDown) then
        if selindex<items.Count-1 then Inc(selindex) ;
      if (event.event.key.code in [sfKeySpace,sfKeyReturn]) then begin
        case selindex of
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

  walkbot.Update(dt) ;
end ;

procedure TSceneMainMenu.RenderFunc() ;
var i:Integer ;
begin
  window.Draw(logo) ;
  for i := 0 to items.Count-1 do
    drawText(items[i],wwidth/2-50,TOP+i*HEIGHT) ;
  drawSprite(walkbot,wwidth/2-50,TOP+selindex*HEIGHT) ;
end ;

procedure TSceneMainMenu.UnInit() ;
var i:Integer ;
begin
  logo.Free ;
  clearItems() ;
  items.Free ;
end ;

end.
