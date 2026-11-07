@echo off
setlocal
cd /d "%~dp0"
where Rscript.exe >nul 2>nul
if not errorlevel 1 (
  Rscript.exe --vanilla "tools\start.R"
  goto :finished
)
for /f "delims=" %%R in ('dir /b /ad /o-n "%LOCALAPPDATA%\Programs\R\R-*" 2^>nul') do (
  if exist "%LOCALAPPDATA%\Programs\R\%%R\bin\Rscript.exe" (
    "%LOCALAPPDATA%\Programs\R\%%R\bin\Rscript.exe" --vanilla "tools\start.R"
    goto :finished
  )
)
for /f "delims=" %%R in ('dir /b /ad /o-n "%ProgramFiles%\R\R-*" 2^>nul') do (
  if exist "%ProgramFiles%\R\%%R\bin\Rscript.exe" (
    "%ProgramFiles%\R\%%R\bin\Rscript.exe" --vanilla "tools\start.R"
    goto :finished
  )
)
echo Rscript was not found. Install R 4.3 or later, then run tools/setup.R.
:finished
if errorlevel 1 pause
endlocal
