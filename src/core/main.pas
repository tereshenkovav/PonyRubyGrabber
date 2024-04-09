unit main ;

interface

type
  TMain = class
  private
  public
    procedure Run() ;
  end;

implementation
uses SysUtils,
  SfmlWindow,
  Game, Scene, SceneStart, SceneCloseHandler, CommonData, Helpers, ProfileLevel ;

procedure TMain.Run() ;
var game:TGame ;
begin
  ChDir(ExtractFilePath(ParamStr(0))+PATH_SEP+'..'+PATH_SEP+'data') ;
  TCommonData.Init() ;
  game:=TGame.Create(1024,768,'PonyRubyGrabber',
    TCommonData.texts.getText('GAME_TITLE'),'images'+PATH_SEP+'icon.png') ;
  game.setCloseHandler(TSceneCloseHandler.Create()) ;
  game.setCustomProfile(TProfileLevel) ;
  // Инициализация действий
  with game.getProfile().getActionConfig() do begin
    addAction(ACTION_LEFT,sfKeyLeft) ;
    addAction(ACTION_RIGHT,sfKeyRight) ;
    addAction(ACTION_UP,sfKeyUp) ;
    addAction(ACTION_DOWN,sfKeyDown) ;
    addAction(ACTION_STOP,sfKeySpace) ;
    addAction(ACTION_USE,sfKeyLControl) ;
    addAction(ACTION_TRANSFORM_0,sfKeyNum1) ;
    addAction(ACTION_TRANSFORM_1,sfKeyNum2) ;
    addAction(ACTION_TRANSFORM_2,sfKeyNum3) ;
    addAction(ACTION_TRANSFORM_3,sfKeyNum4) ;
    addAction(ACTION_TRANSFORM_4,sfKeyNum5) ;
    addAction(ACTION_TRANSFORM_5,sfKeyNum6) ;
  end;
  game.Run(TSceneStart.Create()) ;
  game.Free ;
  TCommonData.UnInit() ;
end ;

end.
