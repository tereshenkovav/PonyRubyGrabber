unit Monster;

interface
uses Types,
  Helpers, Level ;

type
  TMonster = class ;

  TMovingTactic = class
  protected
    monster:TMonster ;
  public
    constructor Create(Amonster:TMonster) ;
    function selectDirFromValids(dirs:TUniList<TPoint>; playerx,playery:Single):TPoint ; virtual ; abstract ;
  end;

  TMovingTacticRandom = class(TMovingTactic)
  public
    function selectDirFromValids(dirs:TUniList<TPoint>; playerx,playery:Single):TPoint ; override ;
  end;

  TMovingTacticGuard = class(TMovingTactic)
  public
    function selectDirFromValids(dirs:TUniList<TPoint>; playerx,playery:Single):TPoint ; override ;
  end;

  TMovingTacticFollow = class(TMovingTactic)
  public
    function selectDirFromValids(dirs:TUniList<TPoint>; playerx,playery:Single):TPoint ; override ;
  end;

  { TMonster }

  TMonster = class
  private
    typeid:Integer ;
    x:Single ;
    y:Single ;
    dx:Integer ;
    dy:Integer ;
    way:Single ;
    level:TLevel ;
    mt:TMovingTactic ;
    procedure genNextDir(playerx,playery:Single) ;
  public
    constructor Create(Atypeid:Integer; Ax,Ay:Single; Alevel:TLevel) ;
    destructor Destroy ; override ;
    function getTypeID():Integer ;
    procedure Update(dt:Single; playerx,playery:Single) ;
    function getX():Single ;
    function getY():Single ;
    function isMirrHorz():Boolean ;
  end;

implementation
uses Classes, SysUtils, Math ;

const SPEED = 2 ;
      STEPX: array[0..3] of Integer = (1,0,0,-1) ;
      STEPY: array[0..3] of Integer = (0,1,-1,0) ;

{ TMonster }

constructor TMonster.Create(Atypeid:Integer; Ax,Ay:Single; Alevel:TLevel);
begin
  typeid:=Atypeid ;
  x:=Ax ;
  y:=Ay ;
  dx:=0 ;
  dy:=0 ;
  way:=0 ;
  level:=Alevel ;
  case typeid of
    0: mt:=TMovingTacticRandom.Create(Self) ;
    1: mt:=TMovingTacticGuard.Create(Self) ;
    2: mt:=TMovingTacticFollow.Create(Self) ;
    else mt:=TMovingTacticRandom.Create(Self) ;
  end;
end;

destructor TMonster.Destroy;
begin
  mt.Free ;
  inherited Destroy;
end;

procedure TMonster.genNextDir(playerx,playery:Single);
var i,k:Integer ;
    dirs:TUniList<TPoint> ;
begin
   dirs:=TUniList<TPoint>.Create() ;
   for i := 0 to Length(STEPX)-1 do
     if level.isWayCorrect(Round(x)+STEPX[i],Round(y)+STEPY[i]) then
       dirs.Add(Point(STEPX[i],STEPY[i])) ;

   if dirs.Count=0 then begin
     dx:=0 ;
     dy:=0 ;
   end
   else
   if dirs.Count=1 then begin
     dx:=dirs[0].X ;
     dy:=dirs[0].Y ;
   end
   else
     with mt.selectDirFromValids(dirs,playerx,playery) do begin
       dx:=X ;
       dy:=Y ;
     end;
   dirs.Free ;
end;

function TMonster.getTypeID: Integer;
begin
  Result:=typeid ;
end;

function TMonster.getX(): Single;
begin
  Result:=x ;
end;

function TMonster.getY(): Single;
begin
  Result:=y ;
end;

function TMonster.isMirrHorz: Boolean;
begin
  Result:=dx<0 ;
end;

procedure TMonster.Update(dt: Single; playerx,playery:Single);
begin
  if (dx=0) and (dy=0) then genNextDir(playerx,playery) ;

  x:=x+SPEED*dx*dt ;
  y:=y+SPEED*dy*dt ;
  way:=way+SPEED*dt ;
  if (way>=1.0) then begin
    x:=Round(x) ;
    y:=Round(y) ;
    way:=0 ;
    genNextDir(playerx,playery) ;
  end;
end;

{ TMovingTactic }

constructor TMovingTactic.Create(Amonster:TMonster) ;
begin
  monster:=Amonster ;
end;

{ TMovingTacticRandom }

function TMovingTacticRandom.selectDirFromValids(
  dirs: TUniList<TPoint>; playerx,playery:Single): TPoint;
begin
  repeat
    Result:=dirs[Random(dirs.Count)] ;
  until not isDXDYRevers(monster.dx,monster.dy,Result.X,Result.Y) ;
end;

{ TMovingTacticGuard }

function TMovingTacticGuard.selectDirFromValids(
  dirs: TUniList<TPoint>; playerx,playery:Single): TPoint;
var i:Integer ;
begin
  i:=0 ;
  while i<dirs.Count do
    if dirs[i].Y<>0 then
      dirs.Delete(i)
    else
      Inc(i) ;

  if dirs.Count=0 then Exit(Point(0,0)) ;
  if dirs.Count=1 then Exit(dirs[0]) ;

  // Дублирование получения координат
  if Trunc(playery+0.5)=Trunc(monster.getY()+0.5) then begin
    for i := 0 to dirs.Count-1 do
      if Sign(playerx-monster.x)=Sign(dirs[i].X) then Exit(dirs[i]) ;
    Exit(dirs[0]) ;
  end
  else
    repeat
      Result:=dirs[Random(dirs.Count)] ;
    until not isDXDYRevers(monster.dx,monster.dy,Result.X,Result.Y) ;
end;

{ TMovingTacticFollow }

function TMovingTacticFollow.selectDirFromValids(
  dirs: TUniList<TPoint>; playerx,playery:Single): TPoint;
var i,tek,best,idx:Integer ;
begin
  best:=-1 ;
  idx:=-1 ;
  for i := 0 to dirs.Count-1 do begin
    tek:=0 ;
    if Sign(playerx-monster.x)=Sign(dirs[i].X) then Inc(tek) ;
    if Sign(playery-monster.y)=Sign(dirs[i].Y) then Inc(tek,2) ;
    if tek>best then begin
      best:=tek ;
      idx:=i ;
    end;
  end;
  if idx=-1 then Result:=dirs[0] else Result:=dirs[idx] ;
end;

end.

