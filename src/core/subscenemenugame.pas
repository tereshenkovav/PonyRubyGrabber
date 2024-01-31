unit SubSceneMenuGame;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers ;

type

  { TSubSceneMenuGame }

  TSubSceneMenuGame = class(TScene)
  private
    textResume:TSfmlText ;
    textMenu:TSfmlText ;
    rect:TSfmlRectangleShape ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses CommonData, SfmlUtils, SceneMainMenu ;

function TSubSceneMenuGame.Init():Boolean ;
begin
  textResume:=createText(TCommonData.Font,'Resume (Esc)',24,SfmlWhite) ;
  textMenu:=createText(TCommonData.Font,'Menu (F10)',24,SfmlWhite) ;
  rect:=TSfmlRectangleShape.Create() ;
  rect.OutlineThickness:=4;
  rect.Size:=SfmlVector2f(250,130) ;
  rect.Position:=SfmlVector2f(wwidth/2-125,wheight/2-50);
  rect.FillColor:=SfmlColorFromRGBA(40,40,40,192) ;
  rect.OutlineColor:=SfmlWhite ;
  Result:=True ;
end ;

function TSubSceneMenuGame.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
begin
  Result:=Normal ;
  for event in events do
    if (event.event.EventType = sfEvtKeyPressed) then begin
      if (event.event.key.code = sfKeyEscape) then begin
        Exit(TSceneResult.ExitSubScene) ;
      end;
      if (event.event.key.code = sfKeyF10) then begin
        nextscene:=TSceneMainMenu.Create() ;
        Exit(TSceneResult.Switch) ;
      end;
    end ;
end ;

procedure TSubSceneMenuGame.RenderFunc() ;
begin
  window.Draw(rect) ;
  drawTextCentered(textResume,wwidth/2,wheight/2-25) ;
  drawTextCentered(textMenu,wwidth/2,wheight/2+25) ;
end ;

procedure TSubSceneMenuGame.UnInit() ;
begin
  rect.Free ;
  textResume.Free ;
  textMenu.Free ;
end ;

end.
