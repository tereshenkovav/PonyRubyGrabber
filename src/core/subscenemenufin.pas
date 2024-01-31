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
    leveln:Integer ;
    iswin:Boolean ;
    rect:TSfmlRectangleShape ;
  public
    constructor Create(Aleveln:Integer; Aiswin:Boolean) ;
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc() ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses CommonData, SfmlUtils, SceneMainMenu, SceneTotalWin, SceneGame, Level ;

function TSubSceneMenuFin.Init():Boolean ;
begin
  textNext:=createText(TCommonData.Font,'Next (Space)',24,SfmlWhite) ;
  textMenu:=createText(TCommonData.Font,'Menu (Esc)',24,SfmlWhite) ;
  rect:=TSfmlRectangleShape.Create() ;
  rect.OutlineThickness:=4;
  rect.Size:=SfmlVector2f(250,130) ;
  rect.Position:=SfmlVector2f(wwidth/2-125,wheight/2-50);
  rect.FillColor:=SfmlColorFromRGBA(40,40,40,192) ;
  rect.OutlineColor:=SfmlWhite ;
  Result:=True ;
end ;

constructor TSubSceneMenuFin.Create(Aleveln: Integer; Aiswin: Boolean);
begin
  leveln:=Aleveln ;
  iswin:=Aiswin ;
end;

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
        if leveln=TLevel.getMaxLevel('levels') then
          nextscene:=TSceneTotalWin.Create()
        else
          nextscene:=TSceneGame.Create(leveln+1) ;
        Exit(TSceneResult.Switch) ;
      end;
    end ;
end ;

procedure TSubSceneMenuFin.RenderFunc() ;
begin
  window.Draw(rect);
  drawTextCentered(textNext,wwidth/2,wheight/2-25) ;
  drawTextCentered(textMenu,wwidth/2,wheight/2+25) ;
end ;

procedure TSubSceneMenuFin.UnInit() ;
begin
  rect.Free ;
  textNext.Free ;
  textMenu.Free ;
end ;

end.
