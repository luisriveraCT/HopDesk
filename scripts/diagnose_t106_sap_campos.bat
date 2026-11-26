@echo off
rem T106 2c: que campo de SAP guarda el UUID del CFDI. SOLO LECTURA: imprime
rem nombres de campos, nunca valores; no escribe nada.
rem Uso: diagnose_t106_sap_campos.bat [cliente]   (dentro de la VPN)
rem Corre desde la carpeta del checkout de prod (la que tiene R\ y scripts\).
setlocal
cd /d "%~dp0.."
set "CLIENTE=%~1"
if "%CLIENTE%"=="" set "CLIENTE=networks"
set "RENVIRON=%HOPDESK_RENVIRON%"
if "%RENVIRON%"=="" set "RENVIRON=%USERPROFILE%\Antiguedad_App\.Renviron"
set "HOPDESK_PRODUCTION=1"
echo.
echo Campos fiscales en SAP - cliente %CLIENTE% - solo lectura, solo nombres
echo Necesitas la VPN arriba.
echo.
"C:\Program Files\R\R-4.5.2\bin\Rscript.exe" --vanilla scripts\diagnose_t106_sap_campos_readonly.R --cliente "%CLIENTE%" --produccion --renviron "%RENVIRON%"
set "CODIGO=%ERRORLEVEL%"
echo.
echo Codigo de salida: %CODIGO%
pause
exit /b %CODIGO%
