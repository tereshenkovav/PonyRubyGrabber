unit Game;

interface

uses
  Classes, SysUtils,
  SfmlGraphics,SfmlSystem,SfmlWindow,
  Scene;

type

  { TGame }

  TGame = class
  private
    window: TSfmlRenderWindow;
    clock:TSfmlClock ;
    mode: TSfmlVideoMode;
    scene: TScene ;
  public
    constructor Create() ;
    procedure Run(initscene:TScene) ;
    destructor Destroy() ; override ;
  end;

implementation
uses Helpers ;

{ TGame }

constructor TGame.Create();
begin
  ChDir(ExtractFilePath(ParamStr(0))+PATH_SEP+'..'+PATH_SEP+'data') ;
  Randomize() ;

  mode.Width := 1024;
  mode.Height := 768;
  mode.BitsPerPixel := 32;
  {$ifndef Darwin}
  if not SfmlVideoModeIsValid(Mode) then
    raise Exception.Create('Invalid video mode');
  {$endif}
  window := TSfmlRenderWindow.Create(mode, UTF8Decode('Game'),
    [sfClose], nil);
  window.SetVerticalSyncEnabled(True);
  window.setFramerateLimit(60);
  window.SetMouseCursorVisible(False);
end ;

procedure TGame.Run(initscene:TScene);
var lasttime,newtime:Single ;
    sr:TSceneResult ;
    event:TSfmlEvent ;
    events:TUniList<TSfmlEventEx> ;
begin
  scene:=initscene ;
  scene.Init() ;

  events:=TUniList<TSfmlEventEx>.Create() ;

  clock:=TSfmlClock.Create() ;
  lasttime:=clock.ElapsedTime.AsSeconds() ;
  while window.IsOpen do begin
    events.Clear ;
    while window.PollEvent(event) do begin
      if event.EventType = sfEvtClosed then begin 
        window.Close; 
        break ;
      end ;
      events.Add(TSfmlEventEx.Create(event)) ;
    end ;

    newtime:=clock.ElapsedTime.asSeconds() ;
    sr:=scene.FrameFunc(newtime-lasttime,events) ;
    lasttime:=newtime ;

    if sr=TSceneResult.Close then begin
      window.Close() ;
      break ;
    end ;

    window.Clear(SfmlBlack);
    scene.RenderFunc(window) ;
    window.Display;
  end;
  scene.UnInit() ;
end;

destructor TGame.Destroy();
begin
  window.Close() ;
  window.Free ;
  inherited Destroy();
end;

end.

