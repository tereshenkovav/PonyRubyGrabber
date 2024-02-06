unit Monster;

interface
uses Level ;

type
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
    procedure genNextDir() ;
  public
    constructor Create(Atypeid:Integer; Ax,Ay:Single; Alevel:TLevel) ;
    function getTypeID():Integer ;
    procedure Update(dt:Single) ;
    function getX():Single ;
    function getY():Single ;
    function isMirrHorz():Boolean ;
  end;

implementation
uses Classes, SysUtils ;

const SPEED = 5 ;
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
end;

procedure TMonster.genNextDir();
var dirx:array[0..3] of Integer ;
    diry:array[0..3] of Integer ;
    dirc:Integer ;
    i,k:Integer ;
begin
   // Ищем направление, которое не совпадает с предыдущим
   dirc:=0 ;
   for i := 0 to 3 do begin
     if level.isWayCorrect(Round(x)+STEPX[i],Round(y)+STEPY[i]) and
       (not isDXDYRevers(dx,dy,STEPX[i],STEPY[i])) then begin
       dirx[dirc]:=STEPX[i] ;
       diry[dirc]:=STEPY[i] ;
       Inc(dirc) ;
     end;
   end;
   if dirc=0 then begin
     dx:=0 ;
     dy:=0 ;
   end
   else begin
     k:=Random(dirc) ;
     dx:=dirx[k] ;
     dy:=diry[k] ;
   end;
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

procedure TMonster.Update(dt: Single);
begin
  if (dx=0) and (dy=0) then genNextDir() ;

  x:=x+SPEED*dx*dt ;
  y:=y+SPEED*dy*dt ;
  way:=way+SPEED*dt ;
  if (way>=1.0) then begin
    x:=Round(x) ;
    y:=Round(y) ;
    way:=0 ;
    genNextDir() ;
  end;
end;

end.

