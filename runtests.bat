@echo OFF
setlocal

set FLAVOR=%1
if /I "%FLAVOR%" == "debug" (goto :FLAVOR_OK)
if /I "%FLAVOR%" == "release" (goto :FLAVOR_OK)

echo Usage: runtests.bat ^<debug^|release^> 
exit /b 1

:FLAVOR_OK
.\src\packages\xunit.runners.1.9.2\tools\xunit.console.clr4.exe .\src\tests\bin\Debug\tests.dll
