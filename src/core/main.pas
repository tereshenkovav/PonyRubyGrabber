unit main ;

interface

type
  TMain = class
  private
  public
    procedure Run() ;
  end;

implementation
uses SysUtils, Game, Scene, SceneStart, SceneCloseHandler, CommonData, Helpers,
 ProfileLevel ;

procedure TMain.Run() ;
var game:TGame ;
begin
  ChDir(ExtractFilePath(ParamStr(0))+PATH_SEP+'..'+PATH_SEP+'data') ;
  TCommonData.Init() ;
  game:=TGame.Create(1024,768,'PonyRubyGrabber',
    TCommonData.texts.getText('GAME_TITLE'),'images'+PATH_SEP+'icon.png') ;
  game.setCloseHandler(TSceneCloseHandler.Create()) ;
  game.setCustomProfile(TProfileLevel) ;
  game.Run(TSceneStart.Create()) ;
  game.Free ;
  TCommonData.UnInit() ;
end ;

end.
