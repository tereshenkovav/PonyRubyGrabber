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
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses CommonData, SfmlUtils, SceneStart ;

function TSubSceneMenuGame.Init():Boolean ;
begin
  textResume:=createText(TCommonData.Font,'Resume (Esc)',18,SfmlWhite) ;
  textMenu:=createText(TCommonData.Font,'Menu (F10)',18,SfmlWhite) ;
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
        nextscene:=TSceneStart.Create() ;
        Exit(TSceneResult.Switch) ;
      end;
    end ;
end ;

procedure TSubSceneMenuGame.RenderFunc() ;
begin
  drawTextCentered(textResume,wwidth/2,100) ;
  drawTextCentered(textMenu,wwidth/2,150) ;
end ;

procedure TSubSceneMenuGame.UnInit() ;
begin
  textResume.Free ;
  textMenu.Free ;
end ;

end.
