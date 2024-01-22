program PonyRubyGrabberFPC;

{$APPTYPE GUI}

uses
  {$ifdef unix}cthreads,{$endif}
  SysUtils, Math, 
  main ;

begin
  {$ifdef unix}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  {$endif}

  with TMain.Create() do begin
    Run() ;
    Free ;
  end;
end.
