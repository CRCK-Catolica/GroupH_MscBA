@echo off
cd /d "%~dp0"
set "RS="
for /f "delims=" %%i in ('where Rscript 2^>nul') do if not defined RS set "RS=%%i"
if not defined RS for /d %%d in ("%ProgramFiles%\R\R-*") do set "RS=%%d\bin\Rscript.exe"
if not defined RS (echo Rscript.exe not found - run run_all_sql.R from RStudio instead & pause & exit /b 1)
echo Using "%RS%"
"%RS%" run_all_sql.R
echo.
pause
