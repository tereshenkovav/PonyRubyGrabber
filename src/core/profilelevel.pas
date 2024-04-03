unit ProfileLevel;

interface
uses Profile,
  IniFiles ;

type
  { TProfileLevel }

  TProfileLevel = class(TProfile)
  private
    availlevel:Integer ;
  protected
    procedure WriteExData(ini:TIniFile) ; override ;
    procedure ReadExData(ini:TIniFile) ; override ;
    procedure InitExData() ; override ;
  public
    function getAvailLevel():Integer ;
    procedure MarkLevelCompleted(level:Integer) ;
  end;

implementation
uses Classes, SysUtils,
  Helpers,
  Level ;

{ TProfileLevel }

procedure TProfileLevel.WriteExData(ini:TIniFile) ;
begin
  ini.WriteInteger('Profile','AvailLevel',availlevel) ;
end;

procedure TProfileLevel.InitExData() ;
begin
  availlevel:=0
end;

procedure TProfileLevel.ReadExData(ini:TIniFile) ;
begin
  availlevel:=ini.ReadInteger('Profile','AvailLevel',0) ;
end;

function TProfileLevel.getAvailLevel(): Integer;
begin
  Result:=availlevel ;
end;

procedure TProfileLevel.MarkLevelCompleted(level: Integer);
begin
  if (availlevel<level+1)and(level+1<=TLevel.getMaxLevel('levels')) then begin
    availlevel:=level+1 ;
    Save() ;
  end;
end;

end.

