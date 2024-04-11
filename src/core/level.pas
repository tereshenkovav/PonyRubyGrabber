unit Level;

interface
uses Types, Classes,
  Helpers ;

type

  TCellType = (Free, Block, Stair, Crystall, HeroIcon) ;

  THeroIcon = record
    x:Integer ;
    y:Integer ;
    code:string ;
    class operator Equal(a: THeroIcon; b: THeroIcon): Boolean;
    constructor Create(Ax, Ay: Integer; ACode: string);
  end;

  { TLevel }

  TLevel = class
  private
    width:Integer ;
    height:Integer ;
    start:TPoint ;
    finish:TPoint ;
    textpos:Integer ;
    textdata:string ;
    map:array of array of TCellType ;
    map_texids:array of array of Integer ;
    icons:TUniList<THeroIcon>;
    list:TStringList ;
  public
    procedure LoadFromFile(filename:string) ;
    class function getMaxLevel(leveldir:string):Integer ;
    function isWayCorrect(x, y: Integer): Boolean;
    function isBlockAt(x,y:Integer):Boolean ;
    function isStairAt(x,y:Integer):Boolean ;
    function isCrystallAt(x,y:Integer):Boolean ;
    function isHeroIconAt(x,y:Integer):Boolean ;
    function getHeroIconAt(x,y:Integer):string ;
    procedure clearCell(x,y:Integer) ;
    procedure setWall(x,y:Integer) ;
    function getWidth():Integer ;
    function getHeight():Integer ;
    function getTextPos():Integer ;
    function getTextData():string ;
    function getTexId(x,y: Integer):Integer ;
    procedure fillStartXY(var x:single; var y:single);
    procedure fillMonsters(monsters:TUniList<TObject>) ;
    procedure fillSpawns(spawns:TUniList<TObject>; monsters:TUniList<TObject>) ;
    procedure fillHeroStorage(dict:TUniDictionary<string,Integer>) ;
    function isFinishAt(x,y:Integer):Boolean ;
    function getCrystallCount():Integer ;
  end;

function isDXDYRevers(dx1, dy1, dx2, dy2: Integer): Boolean;

implementation
uses SysUtils,
  Monster, Spawner, Hero ;

const
  STAIR_SYMS = '#$%^&' ;
  BLOCK_SYMS = '*()+/' ;

function isDXDYRevers(dx1, dy1, dx2, dy2: Integer): Boolean;
begin
  if (dy1=0)and(dy2=0) then Result:=dx1=-dx2 else
  if (dx1=0)and(dx2=0) then Result:=dy1=-dy2 else
  Result:=False ;
end;

{ TLevel }

function TLevel.isWayCorrect(x, y: Integer): Boolean;
begin
  Result:=False ;
  if x<0 then Exit ;
  if y<0 then Exit ;
  if x>=getWidth() then Exit ;
  if y>=getHeight() then Exit ;
  if isBlockAt(x,y) then Exit ;
  Result:=True ;
end;

procedure TLevel.clearCell(x, y: Integer);
begin
  if (x<0) or (x>=width) or (y<0) or (y>=height) then Exit() ;
  map[x][y]:=TCellType.Free ;
end;

procedure TLevel.setWall(x, y: Integer);
begin
  if (x<0) or (x>=width) or (y<0) or (y>=height) then Exit() ;
  map[x][y]:=TCellType.Block ;
end;

procedure TLevel.fillHeroStorage(dict: TUniDictionary<string, Integer>);
var code:string ;
begin
  dict.Clear() ;
  for code in THero.getHeroCodes() do
    dict.Add(code,StrToIntWt0(list.Values['Hero'+code])) ;
end;

procedure TLevel.fillMonsters(monsters: TUniList<TObject>);
var i:Integer ;
    left,top:Integer ;
begin
  left:=StrToIntWt0(list.Values['Left']) ;
  top:=StrToIntWt0(list.Values['Top']) ;
  for i := 0 to StrToIntWt0(list.Values['MonsterCount'])-1 do
    monsters.Add(TMonster.Create(StrToIntWt0(list.Values[Format('Monster%d_ID',[i])]),
      StrToIntWt0(list.Values[Format('Monster%d_X',[i])])+left,
      StrToIntWt0(list.Values[Format('Monster%d_Y',[i])])+top,Self)) ;
end;

procedure TLevel.fillSpawns(spawns: TUniList<TObject>; monsters:TUniList<TObject>);
var i:Integer ;
    left,top:Integer ;
begin
  left:=StrToIntWt0(list.Values['Left']) ;
  top:=StrToIntWt0(list.Values['Top']) ;
  spawns.Clear() ;
  for i := 0 to StrToIntWt0(list.Values['SpawnCount'])-1 do
    spawns.Add(TSpawner.Create(StrToIntWt0(list.Values[Format('Spawn%d_X',[i])])+left,
      StrToIntWt0(list.Values[Format('Spawn%d_Y',[i])])+top,
      Self,TUniList<TMonster>(monsters),
      list.Values[Format('Spawn%d_seq',[i])],
      StrToIntWt0(list.Values[Format('Spawn%d_time',[i])]))) ;
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

function TLevel.getHeroIconAt(x, y: Integer): string;
var hi:THeroIcon ;
begin
  if (x<0) or (x>=width) or (y<0) or (y>=height) then Exit('') ;
  Result:='' ;
  for hi in icons do
    if (hi.x=x)and(hi.y=y) then Result:=hi.code ;
end;

class function TLevel.getMaxLevel(leveldir: string): Integer;
begin
  Result:=-1 ;
  while FileExists(leveldir+PATH_SEP+'level'+IntToStr(Result+1)+'.dat') do
    Inc(Result) ;
end;

function TLevel.getTexId(x, y: Integer): Integer;
begin
  if (x<0) or (x>=width) or (y<0) or (y>=height) then Exit(0) ;
  Result:=map_texids[x][y] ;
end;

function TLevel.getTextData: string;
begin
  Result:=textdata ;
end;

function TLevel.getTextPos: Integer;
begin
  Result:=textpos ;
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

function TLevel.isHeroIconAt(x, y: Integer): Boolean;
begin
  if (x<0) or (x>=width) or (y<0) or (y>=height) then Exit(False) ;
  Result:=map[x][y]=TCellType.HeroIcon ;
end;

function TLevel.isStairAt(x, y: Integer): Boolean;
begin
  if (x<0) or (x>=width) or (y<0) or (y>=height) then Exit(False) ;
  Result:=map[x][y]=TCellType.Stair ;
end;

procedure TLevel.LoadFromFile(filename:string) ;
var i,x,y:Integer ;
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
  textpos:=StrToIntWt0(list.Values['TextPos']) ;
  textdata:=list.Values['TextData'] ;
  width:=dataw+left ;
  height:=datah+top ;
  SetLength(map,width) ;
  SetLength(map_texids,width) ;
  for x := 0 to width-1 do begin
    SetLength(map[x],height) ;
    SetLength(map_texids[x],height) ;
  end;

  for y := 0 to height-1 do
    for x := 0 to width-1 do
      map[x][y]:=TCellType.Free ;

  icons:=TUniList<THeroIcon>.Create() ;
  for y := 0 to datah-1 do begin
    str:=list.Values[Format('Row%d',[y])] ;
    if str='' then str:=list.Values[Format('Row%.2d',[y])] ;

    if str.Length<dataw then str:=str+StringOfChar(chr(32),dataw-str.Length) ;
    for x := 0 to dataw-1 do begin
      ct:=TCellType.Free ;
      if (x+y div 2)mod 2=0 then ct:=TCellType.Crystall ;
      if STAIR_SYMS.IndexOf(str[x+1])<>-1 then begin
        ct:=TCellType.Stair ;
        map_texids[x+left][y+top]:=STAIR_SYMS.IndexOf(str[x+1]) ;
      end;
      if BLOCK_SYMS.IndexOf(str[x+1])<>-1 then begin
        ct:=TCellType.Block ;
        map_texids[x+left][y+top]:=BLOCK_SYMS.IndexOf(str[x+1]) ;
      end;
      if str[x+1]='S' then begin
        start:=Point(x+left,y+top) ;
        ct:=TCellType.Free ; // Защита от спавна поверх алмаза
      end;
      if str[x+1]='F' then begin
        finish:=Point(x+left,y+top) ;
        ct:=TCellType.Free ; // Защита от портала поверх алмаза
      end;
      if str[x+1]='-' then begin
        ct:=TCellType.Free ; // Специальная зона, где нельзя спавнить алмазы
      end;
      if str[x+1] in ['0','1','2','3','4','5'] then begin
        ct:=TCellType.HeroIcon ;
        icons.Add(THeroIcon.Create(x+left,y+top,THero.getHeroCodes()[ord(str[x+1])-ord('0')])) ;
      end;
      // Защита от двери спавна монстров совместно с алмазом
      for i := 0 to StrToIntWt0(list.Values['SpawnCount'])-1 do
        if (StrToIntWt0(list.Values[Format('Spawn%d_X',[i])])=x)and
         (StrToIntWt0(list.Values[Format('Spawn%d_Y',[i])])=y) then ct:=TCellType.Free ;
      map[x+left][y+top]:=ct ;
    end ;
  end;
end ;

{ THeroIcon }

constructor THeroIcon.Create(Ax, Ay: Integer; ACode: string);
begin
  x:=Ax ;
  y:=Ay ;
  code:=Acode ;
end;

class operator THeroIcon.Equal(a, b: THeroIcon): Boolean;
begin
  Result:=(a.x=b.x)and(a.y=b.y)and(a.code=b.code) ;
end;

end.

