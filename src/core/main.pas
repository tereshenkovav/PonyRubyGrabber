unit main ;

interface

type
  TMain = class
  private
  public
    procedure Run() ;
  end;

implementation
uses Game, Scene, SceneStart, SceneCloseHandler, CommonData, Helpers ;

procedure TMain.Run() ;
var game:TGame ;
begin
  game:=TGame.Create(1024,768,'images'+PATH_SEP+'icon.png') ;
  TCommonData.Init() ;
  TScene.closehandler:=TSceneCloseHandler.Create() ;
  game.Run(TSceneStart.Create()) ;
  TCommonData.UnInit() ;
end ;

end.
