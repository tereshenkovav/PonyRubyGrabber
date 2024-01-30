unit SubSceneMenuFin;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,SfmlAudio,
  Scene, Helpers ;

type

  { TSubSceneMenuFin }

  TSubSceneMenuFin = class(TScene)
  private
    textNext:TSfmlText ;
    textMenu:TSfmlText ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses CommonData, SfmlUtils, SceneMainMenu, SceneGame ;

function TSubSceneMenuFin.Init():Boolean ;
begin
  textNext:=createText(TCommonData.Font,'Next (Space)',18,SfmlWhite) ;
  textMenu:=createText(TCommonData.Font,'Menu (Esc)',18,SfmlWhite) ;
  Result:=True ;
end ;

function TSubSceneMenuFin.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
begin
  Result:=Normal ;
  for event in events do
    if (event.event.EventType = sfEvtKeyPressed) then begin
      if (event.event.key.code = sfKeyEscape) then begin
        nextscene:=TSceneMainMenu.Create() ;
        Exit(TSceneResult.Switch) ;
      end;
      if (event.event.key.code = sfKeySpace) then begin
        nextscene:=TSceneGame.Create(TCommonData.profile.getAvailLevel()) ;
        Exit(TSceneResult.Switch) ;
      end;
    end ;
end ;

procedure TSubSceneMenuFin.RenderFunc() ;
begin
  drawTextCentered(textNext,wwidth/2,100) ;
  drawTextCentered(textMenu,wwidth/2,150) ;
end ;

procedure TSubSceneMenuFin.UnInit() ;
begin
  textNext.Free ;
  textMenu.Free ;
end ;

end.
