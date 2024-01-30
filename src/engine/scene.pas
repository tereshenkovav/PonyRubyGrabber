unit Scene;

interface

uses
  Classes, SysUtils,
  SfmlSystem,SfmlWindow,SfmlGraphics,
  Helpers ;

type

  TSceneResult = (Normal,Close,Switch,SetSubScene,ExitSubScene,RebuildWindow) ;

  TSfmlEventEx = record
    event:TSfmlEvent ;
    constructor Create(event:TSfmlEvent) ;
    class operator Equal(a: TSfmlEventEx; b: TSfmlEventEx): Boolean;
  end;

  { TScene }

  TScene = class
  private
  protected
    window:TSfmlRenderWindow ;
    wwidth:Integer ;
    wheight:Integer ;
    nextscene:TScene ;
    subscene:TScene ;
    procedure drawSprite(spr:TSfmlSprite; x,y:Single) ;
    procedure drawText(text:TSfmlText; x,y:Single) ;
    procedure drawTextCentered(text:TSfmlText; x,y:Single) ;
  public
    procedure setWindow(Awindow:TSfmlRenderWindow; Awidth,Aheight:Integer);
    function getNextScene():TScene ;
    function getSubScene():TScene ;
    function Init():Boolean ; virtual ;
    function FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ; virtual ;
    procedure RenderFunc() ; virtual ;
    procedure UnInit() ; virtual ;
    destructor Destroy() ; override ;
  end;

implementation

{ TScene }

function TScene.Init():Boolean ;
begin
  Result:=True ;
end ;

function TScene.FrameFunc(dt:Single; events:TUniList<TSfmlEventEx>):TSceneResult ;
begin
  Result:=TSceneResult.Normal ;
end ;

function TScene.getNextScene: TScene;
begin
  Result:=nextscene ;
end;

function TScene.getSubScene: TScene;
begin
  Result:=subscene ;
end;

procedure TScene.RenderFunc() ;
begin
end ;

procedure TScene.setWindow(Awindow: TSfmlRenderWindow; Awidth,
  Aheight: Integer);
begin
  window:=Awindow ;
  wwidth:=Awidth ;
  wheight:=Aheight ;
end;

procedure TScene.UnInit() ;
begin
end ;

destructor TScene.Destroy();
begin
  inherited Destroy();
end;

procedure TScene.drawSprite(spr: TSfmlSprite; x,
  y: Single);
begin
  spr.Position:=SfmlVector2f(x,y) ;
  window.draw(spr) ;
end;

procedure TScene.drawText(text: TSfmlText; x,
  y: Single);
begin
  text.Position:=SfmlVector2f(x,y) ;
  window.Draw(text) ;
end;

procedure TScene.drawTextCentered(text: TSfmlText; x,
  y: Single);
begin
  text.Position:=SfmlVector2f(x-text.LocalBounds.Width/2,y) ;
  window.Draw(text) ;
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

