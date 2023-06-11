@echo off
setlocal

set "directory=%~dp0"

for %%F in ("%directory%\*.*") do (
    if /I not "%%~nxF"=="%~nx0" (
        start "" "%%F"
        ping -n 6 127.0.0.1 >nul
    )
)

endlocal