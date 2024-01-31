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
    tekscene: TScene ;
    subscene: TScene ;
    prevscene: TScene ;
  public
    constructor Create(width,height:Integer) ;
    procedure Run(initscene:TScene) ;
    destructor Destroy() ; override ;
  end;

implementation
uses Helpers ;

{ TGame }

constructor TGame.Create(width,height:Integer);
begin
  ChDir(ExtractFilePath(ParamStr(0))+PATH_SEP+'..'+PATH_SEP+'data') ;
  Randomize() ;

  mode.Width := width;
  mode.Height := height;
  mode.BitsPerPixel := 32;
  {$ifndef Darwin}
  if not SfmlVideoModeIsValid(Mode) then
    raise Exception.Create('Invalid video mode');
  {$endif}
  window := TSfmlRenderWindow.Create(mode, UTF8ToString('Game'),
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
    activescene:TScene ;
begin

  prevscene:=nil ;
  subscene:=nil ;
  tekscene:=initscene ;
  tekscene.setWindow(window,mode.Width,mode.Height) ;
  tekscene.Init() ;

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

    if subscene<>nil then activescene:=subscene else activescene:=tekscene ;
    if activescene.getOverScene()<>nil then activescene.getOverScene().FrameFunc(newtime-lasttime,events) ;
    sr:=activescene.FrameFunc(newtime-lasttime,events) ;

    lasttime:=newtime ;

    case sr of
      TSceneResult.Close: begin
        window.Close() ;
        break ;
      end;
      TSceneResult.Switch: begin
        if (subscene<>nil) then begin
          subscene.UnInit() ;
          subscene:=nil ;
        end;
        if tekscene.getOverScene()<>nil then tekscene.getOverScene().UnInit() ;
        tekscene.UnInit();
        tekscene:=activescene.getNextScene();
        tekscene.setWindow(window,mode.Width,mode.Height) ;
        tekscene.Init();
        if tekscene.getOverScene()<>nil then begin
          tekscene.getOverScene().setWindow(window,mode.Width,mode.Height) ;
          tekscene.getOverScene().Init() ;
        end ;
        continue ;
      end ;
      TSceneResult.SetSubScene: begin
        subscene:=tekscene.getSubScene() ;
        subscene.setWindow(window,mode.Width,mode.Height) ;
        subscene.Init();
        continue ;
      end ;
      TSceneResult.ExitSubScene: begin
        subscene.UnInit() ;
        subscene:=nil ;
        continue ;
      end ;
    end ;

    window.Clear(SfmlBlack);
    tekscene.RenderFunc() ;
    if subscene<>nil then subscene.RenderFunc() ;
    if tekscene.getOverScene()<>nil then tekscene.getOverScene().RenderFunc() ;
    window.Display;
  end;
  tekscene.UnInit() ;
end;

destructor TGame.Destroy();
begin
  window.Close() ;
  window.Free ;
  inherited Destroy();
end;

end.

