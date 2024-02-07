unit Spawner;

interface
uses Types,
  Level, Helpers, Monster ;

type
  { TSpawner }

  TSpawner = class
  private
    spawnseq:array of Integer ;
    x:Single ;
    y:Single ;
    spawntime:Integer ;
    level:TLevel ;
    nextspawn:Single ;
    spawnidx:Integer ;
    monsters:TUniList<TMonster> ;
  public
    constructor Create(Ax,Ay:Single; Alevel:TLevel; Amonsters:TUniList<TMonster>;
      Aspawnseq:string; Aspawntime:Integer) ;
    function getX():Single ;
    function getY():Single ;
    procedure Update(dt:Single) ;
  end;

implementation
uses Classes, SysUtils ;

{ TSpawner }

constructor TSpawner.Create(Ax,Ay:Single; Alevel:TLevel; Amonsters:TUniList<TMonster>;
  Aspawnseq:string; Aspawntime:Integer);
var i:Integer ;
begin
  SetLength(spawnseq,Length(Aspawnseq)) ;
  for i:=0 to Length(Aspawnseq)-1 do
    spawnseq[i]:=ord(Aspawnseq[i+1])-ord('0') ;

  x:=Ax ;
  y:=Ay ;
  spawntime:=Aspawntime ;
  level:=Alevel ;
  monsters:=Amonsters ;

  nextspawn:=spawntime ;
  spawnidx:=0 ;
end;

function TSpawner.getX: Single;
begin
  Result:=x ;
end;

function TSpawner.getY: Single;
begin
  Result:=y ;
end;

procedure TSpawner.Update(dt: Single);
begin
  nextspawn:=nextspawn-dt ;
  if nextspawn<=0 then begin
    nextspawn:=spawntime ;
    if spawnidx<Length(spawnseq) then begin
      monsters.Add(TMonster.Create(spawnseq[spawnidx],x,y,level)) ;
      Inc(spawnidx) ;
    end;
  end ;
end;

end.

