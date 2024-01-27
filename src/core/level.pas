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
    function isBlockAt(x,y:Integer):Boolean ;
    function isStairAt(x,y:Integer):Boolean ;
    function isCrystallAt(x,y:Integer):Boolean ;
    procedure clearCell(x,y:Integer) ;
    function getWidth():Integer ;
    function getHeight():Integer ;
    procedure fillStartXY(var x:single; var y:single);
    function isFinishAt(x,y:Integer):Boolean ;
  end;

implementation
uses Classes, SysUtils ;

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

function TLevel.getHeight: Integer;
begin
  Result:=height ;
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
    x,y:Integer ;
    ct:TCellType ;
begin
  list:=TStringList.Create ;
  list.LoadFromFile(filename) ;
  width:=StrToInt(list[0]) ;
  height:=StrToInt(list[1]) ;
  SetLength(map,width) ;
  for x := 0 to width-1 do
    SetLength(map[x],height) ;

  for y := 0 to height-1 do
    for x := 0 to width-1 do begin
      ct:=TCellType.Free ;
      if (x+y div 2)mod 2=0 then ct:=TCellType.Crystall ;
      if list[2+y][x+1]='#' then ct:=TCellType.Stair ;
      if list[2+y][x+1]='=' then ct:=TCellType.Block ;
      if list[2+y][x+1]='S' then begin
        start:=Point(x,y) ;
        ct:=TCellType.Free ; // Защита от спавна поверх алмаза
      end;
      if list[2+y][x+1]='F' then begin
        finish:=Point(x,y) ;
        ct:=TCellType.Free ; // Защита от портала поверх алмаза
      end;
      map[x][y]:=ct ;
    end ;
  list.Free ;
end ;

end.

