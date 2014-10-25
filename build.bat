@echo OFF
setlocal

where.exe msbuild > NUL 2> NUL 
if errorlevel 1 (
  echo Error: MSBUILD is not on the PATH
  exit /b 1
)

msbuild .\src\project.sln %*