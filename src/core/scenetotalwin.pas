unit SceneTotalWin;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers ;

type

  { TSceneTotalWin }

  TSceneTotalWin = class(TScene)
  private
    intro:TSfmlSprite ;
    textOK:TSfmlText ;
    stage:Single ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses SceneMainMenu, SfmlUtils, CommonData ;

function TSceneTotalWin.Init():Boolean ;
begin
  intro:=loadSprite('images'+PATH_SEP+'totalwin.png');
  intro.Position:=SfmlVector2f(0,0) ;
  intro.Color:=SfmlColorFromRGBA(255,255,255,Trunc(stage)) ;
  textOK:=createText(TCommonData.Font,TCommonData.texts.getText('TEXT_TOTALWIN'),32,
    SfmlColorFromRGB(120,40,40)) ;
  Result:=True ;
end ;

function TSceneTotalWin.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
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

procedure TSceneTotalWin.RenderFunc() ;
begin
  intro.Color:=SfmlColorFromRGBA(255,255,255,Trunc(stage)) ;
  window.Draw(intro) ;
  if stage=255 then drawTextCentered(textOK,wwidth/2,30) ;
end ;

procedure TSceneTotalWin.UnInit() ;
begin
end ;

end.
