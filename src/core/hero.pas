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
  public
    function Apply(level:TLevel; herox,heroy:Integer; herodx:Integer):Boolean ; virtual ;
  end;

  THeroActionCrushWall = class(THeroAction)
  private
  public
    function Apply(level:TLevel; herox,heroy:Integer; herodx:Integer):Boolean ; override ;
  end;

implementation
uses SysUtils ;

var nohero:THero ;
    codes:TStringList ;

{ THero }

constructor THero.Create(Acode: string);
begin
  code:=Acode ;
end;

function THero.createAction: THeroAction;
begin
  if code='pinkie' then Result:=THeroActionCrushWall.Create() else
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
  herodx: Integer):Boolean;
begin
  if level.isBlockAt(herox+herodx,heroy) then begin
    level.clearCell(herox+herodx,heroy) ;
    Result:=True ;
  end
  else
    Result:=False ;
end;

{ THeroAction }

function THeroAction.Apply(level: TLevel; herox, heroy, herodx: Integer):Boolean;
begin
  // null action
  Result:=False ;
end;

initialization

nohero:=THero.Create('') ;
codes:=TStringList.Create() ;
codes.CommaText:='pinkie,applejack,flatter,rainbow,rarity,twily' ;

finalization

nohero.Free ;
codes.Free ;

end.

