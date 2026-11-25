@echo off
rem T103: PRUEBA del actualizador: lee el ERP y lo compara con la foto de PRODUCCION. No escribe nada.
rem Uso: actualizar_erp_prueba.bat [cliente]
rem Corre desde la carpeta del checkout de prod (la que tiene R\ y scripts\).
rem Lee el .Renviron del checkout principal; otra ruta: set HOPDESK_RENVIRON=...
setlocal
cd /d "%~dp0.."
set "CLIENTE=%~1"
if "%CLIENTE%"=="" set "CLIENTE=networks"
set "RENVIRON=%HOPDESK_RENVIRON%"
if "%RENVIRON%"=="" set "RENVIRON=%USERPROFILE%\Antiguedad_App\.Renviron"
set "HOPDESK_PRODUCTION=1"
echo.
echo Prueba del actualizador - cliente %CLIENTE% - solo lectura
echo Necesitas la VPN arriba.
echo.
"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" --vanilla scripts\actualizar_erp.R --cliente "%CLIENTE%" --produccion --prueba --renviron "%RENVIRON%"
set "CODIGO=%ERRORLEVEL%"
echo.
echo Codigo de salida: %CODIGO%
pause
exit /b %CODIGO%
