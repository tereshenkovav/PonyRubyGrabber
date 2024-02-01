unit SceneCloseHandler;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers ;

type

  { TSceneCloseHandler }

  TSceneCloseHandler = class(TScene)
  private
    textResume:TSfmlText ;
    textClose:TSfmlText ;
    rect:TSfmlRectangleShape ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses CommonData, SfmlUtils, SceneMainMenu ;

function TSceneCloseHandler.Init():Boolean ;
begin
  textResume:=createText(TCommonData.Font,TCommonData.texts.getText('BUT_RESUME'),24,SfmlWhite) ;
  textClose:=createText(TCommonData.Font,TCommonData.texts.getText('BUT_CLOSE'),24,SfmlWhite) ;
  rect:=TSfmlRectangleShape.Create() ;
  rect.OutlineThickness:=4;
  rect.Size:=SfmlVector2f(250,130) ;
  rect.Position:=SfmlVector2f(wwidth/2-125,wheight/2-50);
  rect.FillColor:=SfmlColorFromRGBA(40,40,40,192) ;
  rect.OutlineColor:=SfmlWhite ;
  Result:=True ;
end ;

function TSceneCloseHandler.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
begin
  Result:=Normal ;
  for event in events do
    if (event.event.EventType = sfEvtKeyPressed) then begin
      if (event.event.key.code = sfKeyEscape) then begin
        Exit(TSceneResult.Switch) ;
      end;
      if (event.event.key.code = sfKeyF10) then begin
        Exit(TSceneResult.Close) ;
      end;
    end ;
end ;

procedure TSceneCloseHandler.RenderFunc() ;
begin
  window.Draw(rect) ;
  drawTextCentered(textResume,wwidth/2,wheight/2-25) ;
  drawTextCentered(textClose,wwidth/2,wheight/2+25) ;
end ;

procedure TSceneCloseHandler.UnInit() ;
begin
  rect.Free ;
  textResume.Free ;
  textClose.Free ;
end ;

end.
