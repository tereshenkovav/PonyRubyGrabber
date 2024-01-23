unit main ;

interface

type
  TMain = class
  private
  public
    procedure Run() ;
  end;

implementation
uses Game, SceneTest, CommonData ;

procedure TMain.Run() ;
var game:TGame ;
    scene:TSceneTest ;
begin
  game:=TGame.Create() ;
  scene:=TSceneTest.Create() ;
  TCommonData.Init() ;
  game.Run(scene) ;
  TCommonData.UnInit() ;
end ;

end.
