unit SceneTest;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,
  Helpers, Scene ;

type

  { TSceneTest }

  TSceneTest = class(TScene)
  private
    cursor:TSfmlSprite ;
  public
    function Init():Boolean ; override ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; override ;
    procedure RenderFunc(window:TSfmlRenderWindow) ; override ;
    procedure UnInit() ; override ;
  end;

implementation
uses SfmlUtils ;

{ TSceneTest }

function TSceneTest.Init():Boolean ;
begin
  Cursor:=loadSprite('images'+PATH_SEP+'cursor.png');
  Cursor.Origin:=SfmlVector2f(0,10) ;
  Result:=True ;
end ;

function TSceneTest.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
var event:TSfmlEventEx ;
begin
  Result:=Normal ;
  for event in events do
    if (event.event.EventType = sfEvtKeyPressed) then begin
      if (event.event.key.code = sfKeyEscape) then Exit(TSceneResult.Close)
    end ;
end ;

procedure TSceneTest.RenderFunc(window:TSfmlRenderWindow) ;
var mx,my:Integer ;
begin
  mx:=window.MousePosition.X ;
  my:=window.MousePosition.Y ;
  DrawSprite(window,cursor,mx,my) ;
end ;

procedure TSceneTest.UnInit() ;
begin
end ;

end.

