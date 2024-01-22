unit main ;

interface

type
  TMain = class
  private
  public
    procedure Run() ;
  end;

implementation
uses Game, SceneTest ;

procedure TMain.Run() ;
var game:TGame ;
    scene:TSceneTest ;
begin
  game:=TGame.Create() ;
  scene:=TSceneTest.Create() ;
  game.Run(scene) ;
end ;

end.
