unit Scene;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,
  Helpers ;

type

  TSceneResult = (Normal,Close,Switch,RebuildWindow) ;

  TSfmlEventEx = record
    event:TSfmlEvent ;
    constructor Create(event:TSfmlEvent) ;
    class operator Equal(a: TSfmlEventEx; b: TSfmlEventEx): Boolean;
  end;

  { TScene }

  TScene = class
  private
  protected
    procedure drawSprite(window:TSfmlRenderWindow; spr:TSfmlSprite; x,y:Single) ;
  public
    constructor Create() ;
    function Init():Boolean ; virtual ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; virtual ;
    procedure RenderFunc(window:TSfmlRenderWindow) ; virtual ;
    procedure UnInit() ; virtual ;
    destructor Destroy() ; override ;
  end;

implementation

{ TScene }

constructor TScene.Create();
begin
end ;

function TScene.Init():Boolean ;
begin
end ;

function TScene.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
begin
end ;

procedure TScene.RenderFunc(window:TSfmlRenderWindow) ;
begin
end ;

procedure TScene.UnInit() ;
begin
end ;

destructor TScene.Destroy();
begin
  inherited Destroy();
end;

procedure TScene.drawSprite(window: TSfmlRenderWindow; spr: TSfmlSprite; x,
  y: Single);
begin
  spr.Position:=SfmlVector2f(x,y) ;
  window.draw(spr) ;
end;

{ TSfmlEventEx }

constructor TSfmlEventEx.Create(event: TSfmlEvent);
begin
  Self.event:=event ;
end;

class operator TSfmlEventEx.Equal(a: TSfmlEventEx; b: TSfmlEventEx): Boolean;
begin
  Result:=False ;
end;

end.

