unit Hero;

interface
uses Classes, Level ;

type
  THeroAction = class ;

  { THero }

  THero = class
  private
    code:string ;
  public
    constructor Create(Acode:string) ;
    function getCode():string ;
    class function getNoHero():THero ;
    class function getHeroCodes():TStringList ;
    function isNoHero():Boolean ;
    function createAction():THeroAction ;
  end;

  THeroAction = class
  private
  protected
    timeleft:Single ;
  public
    function Apply(level:TLevel; herox,heroy:Integer; herodx:Integer; Ascene:TObject):Boolean ; virtual ;
    procedure Finish() ; virtual ;
    function isTimeOut():Boolean ;
    procedure Update(dt:Single) ;
  end;

  THeroActionCrushWall = class(THeroAction)
  private
  public
    function Apply(level:TLevel; herox,heroy:Integer; herodx:Integer; Ascene:TObject):Boolean ; override ;
  end;

  THeroActionBuildWall = class(THeroAction)
  private
  public
    function Apply(level:TLevel; herox,heroy:Integer; herodx:Integer; Ascene:TObject):Boolean ; override ;
  end;

  THeroActionJump = class(THeroAction)
  private
  public
    function Apply(level:TLevel; herox,heroy:Integer; herodx:Integer; Ascene:TObject):Boolean ; override ;
  end;

  THeroActionSpeedUp = class(THeroAction)
  private
    scene:TObject ;
  public
    function Apply(level:TLevel; herox,heroy:Integer; herodx:Integer; Ascene:TObject):Boolean ; override ;
    procedure Finish() ; override ;
  end;

  THeroActionShield = class(THeroAction)
  private
    scene:TObject ;
  public
    function Apply(level:TLevel; herox,heroy:Integer; herodx:Integer; Ascene:TObject):Boolean ; override ;
    procedure Finish() ; override ;
  end;

  THeroActionStun = class(THeroAction)
  private
    scene:TObject ;
  public
    function Apply(level:TLevel; herox,heroy:Integer; herodx:Integer; Ascene:TObject):Boolean ; override ;
    procedure Finish() ; override ;
  end;

implementation
uses SysUtils,
  SceneGame, Monster ;

var nohero:THero ;
    codes:TStringList ;

const DIST_TELEPORT = 5 ;

{ THero }

constructor THero.Create(Acode: string);
begin
  code:=Acode ;
end;

function THero.createAction: THeroAction;
begin
  if code='pinkie' then Result:=THeroActionCrushWall.Create() else
  if code='rainbow' then Result:=THeroActionSpeedUp.Create() else
  if code='twily' then Result:=THeroActionJump.Create() else
  if code='applejack' then Result:=THeroActionBuildWall.Create() else
  if code='rarity' then Result:=THeroActionShield.Create() else
  if code='flatter' then Result:=THeroActionStun.Create() else
  Result:=THeroAction.Create() ;
end;

function THero.getCode: string;
begin
  Result:=code ;
end;

class function THero.getHeroCodes: TStringList;
begin
  Result:=codes ;
end;

class function THero.getNoHero: THero;
begin
  Result:=nohero ;
end;

function THero.isNoHero: Boolean;
begin
  Result:=code='' ;
end;

{ THeroActionCrushWall }

function THeroActionCrushWall.Apply(level: TLevel; herox, heroy,
  herodx: Integer; Ascene:TObject):Boolean;
begin
  if level.isBlockAt(herox+herodx,heroy) then begin
    level.clearCell(herox+herodx,heroy) ;
    Result:=True ;
  end
  else
    Result:=False ;
end;

{ THeroAction }

function THeroAction.Apply(level: TLevel; herox, heroy, herodx: Integer; Ascene:TObject):Boolean;
begin
  // null action
  Result:=False ;
  timeleft:=0 ;
end;

procedure THeroAction.Finish;
begin
  // null action
end;

function THeroAction.isTimeOut: Boolean;
begin
  Result:=timeleft<=0 ;
end;

procedure THeroAction.Update(dt: Single);
begin
  timeleft:=timeleft-dt ;
end;

{ THeroActionSpeedUp }

function THeroActionSpeedUp.Apply(level: TLevel; herox, heroy, herodx: Integer;
  Ascene: TObject): Boolean;
begin
  scene:=Ascene ;
  timeleft:=7 ;
  TSceneGame(scene).speedup:=True ;
  Result:=True ;
end;

procedure THeroActionSpeedUp.Finish;
begin
  TSceneGame(scene).speedup:=False ;
end;

{ THeroActionJump }

function THeroActionJump.Apply(level: TLevel; herox, heroy, herodx: Integer;
  Ascene: TObject): Boolean;
begin
  if not level.isBlockAt(herox+DIST_TELEPORT*herodx,heroy) then begin
    TSceneGame(Ascene).jumpHeroTo(herox+DIST_TELEPORT*herodx,heroy) ;
    Result:=True ;
  end
  else
    Result:=False ;
end;

{ THeroActionBuildWall }

function THeroActionBuildWall.Apply(level: TLevel; herox, heroy,
  herodx: Integer; Ascene: TObject): Boolean;
begin
  if not level.isBlockAt(herox+herodx,heroy) then begin
    level.setWall(herox+herodx,heroy) ;
    Result:=True ;
  end
  else
    Result:=False ;
end;

{ THeroActionShield }

function THeroActionShield.Apply(level: TLevel; herox, heroy, herodx: Integer;
  Ascene: TObject): Boolean;
begin
  scene:=Ascene ;
  timeleft:=7 ;
  TSceneGame(scene).shield:=True ;
  Result:=True ;
end;

procedure THeroActionShield.Finish;
begin
  TSceneGame(scene).shield:=False ;
end;

{ THeroActionStun }

function THeroActionStun.Apply(level: TLevel; herox, heroy, herodx: Integer;
  Ascene: TObject): Boolean;
var m:TMonster ;
begin
  scene:=Ascene ;
  timeleft:=10 ;
  for m in TSceneGame(scene).monsters do
    m.setStun(True) ;
  Result:=True ;
end;

procedure THeroActionStun.Finish;
var m:TMonster ;
begin
  for m in TSceneGame(scene).monsters do
    m.setStun(False) ;
end;

initialization

nohero:=THero.Create('') ;
codes:=TStringList.Create() ;
codes.CommaText:='pinkie,applejack,flatter,rainbow,rarity,twily' ;

finalization

nohero.Free ;
codes.Free ;

end.

