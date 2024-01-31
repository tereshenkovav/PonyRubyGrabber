unit Level;

interface
uses Types ;

type

  TCellType = (Free, Block, Stair, Crystall) ;

  { TLevel }

  TLevel = class
  private
    width:Integer ;
    height:Integer ;
    start:TPoint ;
    finish:TPoint ;
    map:array of array of TCellType ;
  public
    procedure LoadFromFile(filename:string) ;
    class function getMaxLevel(leveldir:string):Integer ;
    function isBlockAt(x,y:Integer):Boolean ;
    function isStairAt(x,y:Integer):Boolean ;
    function isCrystallAt(x,y:Integer):Boolean ;
    procedure clearCell(x,y:Integer) ;
    function getWidth():Integer ;
    function getHeight():Integer ;
    procedure fillStartXY(var x:single; var y:single);
    function isFinishAt(x,y:Integer):Boolean ;
    function getCrystallCount():Integer ;
  end;

implementation
uses Classes, SysUtils,
  Helpers ;

{ TLevel }

procedure TLevel.clearCell(x, y: Integer);
begin
  if (x<0) or (x>=width) or (y<0) or (y>=height) then Exit() ;
  map[x][y]:=TCellType.Free ;
end;

procedure TLevel.fillStartXY(var x, y: single);
begin
  x:=start.X ;
  y:=start.Y ;
end;

function TLevel.getCrystallCount: Integer;
var x,y:Integer ;
begin
  Result:=0 ;
  for y := 0 to height-1 do
    for x := 0 to width-1 do
      if map[x][y]=TCellType.Crystall then Inc(Result) ;
end;

function TLevel.getHeight: Integer;
begin
  Result:=height ;
end;

class function TLevel.getMaxLevel(leveldir: string): Integer;
begin
  Result:=-1 ;
  while FileExists(leveldir+PATH_SEP+'level'+IntToStr(Result+1)+'.dat') do
    Inc(Result) ;
end;

function TLevel.getWidth: Integer;
begin
  Result:=width ;
end;

function TLevel.isBlockAt(x, y: Integer): Boolean;
begin
  if (x<0) or (x>=width) or (y<0) or (y>=height) then Exit(True) ;
  Result:=map[x][y]=TCellType.Block ;
end;

function TLevel.isCrystallAt(x, y: Integer): Boolean;
begin
  if (x<0) or (x>=width) or (y<0) or (y>=height) then Exit(False) ;
  Result:=map[x][y]=TCellType.Crystall ;
end;

function TLevel.isFinishAt(x, y: Integer): Boolean;
begin
  if (x<0) or (x>=width) or (y<0) or (y>=height) then Exit(False) ;
  Result:=(x=finish.X)and(y=finish.Y) ;
end;

function TLevel.isStairAt(x, y: Integer): Boolean;
begin
  if (x<0) or (x>=width) or (y<0) or (y>=height) then Exit(False) ;
  Result:=map[x][y]=TCellType.Stair ;
end;

procedure TLevel.LoadFromFile(filename:string) ;
var list:TStringList ;
    i,x,y:Integer ;
    ct:TCellType ;
    str:string ;
    left,top:Integer ;
    dataw,datah:Integer ;
begin
  list:=TStringList.Create ;
  list.LoadFromFile(filename) ;
  for i := 0 to list.Count-1 do
    list[i]:=Trim(list[i]) ;
  dataw:=StrToIntWt0(list.Values['Width']) ;
  datah:=StrToIntWt0(list.Values['Height']) ;
  left:=StrToIntWt0(list.Values['Left']) ;
  top:=StrToIntWt0(list.Values['Top']) ;
  width:=dataw+left ;
  height:=datah+top ;
  SetLength(map,width) ;
  for x := 0 to width-1 do
    SetLength(map[x],height) ;

  for y := 0 to height-1 do
    for x := 0 to width-1 do
      map[x][y]:=TCellType.Free ;

  for y := 0 to datah-1 do begin
    str:=list.Values['Row'+IntToStr(y)] ;
    if str.Length<dataw then str:=str+StringOfChar(chr(32),dataw-str.Length) ;
    for x := 0 to dataw-1 do begin
      ct:=TCellType.Free ;
      if (x+y div 2)mod 2=0 then ct:=TCellType.Crystall ;
      if str[x+1]='#' then ct:=TCellType.Stair ;
      if str[x+1]='*' then ct:=TCellType.Block ;
      if str[x+1]='S' then begin
        start:=Point(x+left,y+top) ;
        ct:=TCellType.Free ; // Защита от спавна поверх алмаза
      end;
      if str[x+1]='F' then begin
        finish:=Point(x+left,y+top) ;
        ct:=TCellType.Free ; // Защита от портала поверх алмаза
      end;
      map[x+left][y+top]:=ct ;
    end ;
  end;
  list.Free ;
end ;

end.

