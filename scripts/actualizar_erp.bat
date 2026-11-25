@echo off
rem T103: actualiza los datos del ERP de un cliente en PRODUCCION, sin la app.
rem Uso: actualizar_erp.bat [cliente] [usuario]
rem Corre desde la carpeta del checkout de prod (la que tiene R\ y scripts\).
rem Lee el .Renviron del checkout principal; otra ruta: set HOPDESK_RENVIRON=...
setlocal
cd /d "%~dp0.."
set "CLIENTE=%~1"
if "%CLIENTE%"=="" set "CLIENTE=networks"
set "RENVIRON=%HOPDESK_RENVIRON%"
if "%RENVIRON%"=="" set "RENVIRON=%USERPROFILE%\Antiguedad_App\.Renviron"
set "HOPDESK_PRODUCTION=1"
set "USUARIO=%~2"
if "%USUARIO%"=="" set /p "USUARIO=Tu usuario de HopDesk: "
echo.
echo Actualizador del ERP - cliente %CLIENTE% - PRODUCCION
echo Necesitas la VPN arriba. No abras la app local mientras corre.
echo.
"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" --vanilla scripts\actualizar_erp.R --cliente "%CLIENTE%" --usuario "%USUARIO%" --produccion --renviron "%RENVIRON%"
set "CODIGO=%ERRORLEVEL%"
echo.
if "%CODIGO%"=="0" (echo Termino bien.) else (echo Termino con codigo %CODIGO%: lee los mensajes de arriba.)
pause
exit /b %CODIGO%
