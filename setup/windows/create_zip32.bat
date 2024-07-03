if NOT "%~1" == "" goto mainproc

echo "Argument - lang code" 
exit

:mainproc
SmartZipBuilder.exe script.szb /LANGL=%1 /LANGH=%2
