unit main ;

interface

type
  TMain = class
  private
  public
    procedure Run() ;
  end;

implementation
uses SysUtils, Game, Scene, SceneStart, SceneCloseHandler, CommonData, Helpers ;

procedure TMain.Run() ;
var game:TGame ;
begin
  ChDir(ExtractFilePath(ParamStr(0))+PATH_SEP+'..'+PATH_SEP+'data') ;
  TCommonData.Init() ;
  game:=TGame.Create(1024,768,TCommonData.texts.getText('GAME_TITLE'),'images'+PATH_SEP+'icon.png') ;
  TScene.closehandler:=TSceneCloseHandler.Create() ;
  game.Run(TSceneStart.Create()) ;
  game.Free ;
  TCommonData.UnInit() ;
end ;

end.
