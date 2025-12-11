@ECHO OFF
SETLOCAL EnableDelayedExpansion

REM If an argument is provided, use it as project folder
IF "%~1"=="" (
  REM No argument: use current folder name
  FOR %%I IN ("%CD%") DO SET project=%%~nI
) ELSE (
  SET project=%~1
  REM Navigate into the project folder
  IF NOT EXIST "%project%" (
    ECHO Folder %project% not found
    EXIT /B 1
  )
  CD "%project%"
)

REM Enter "build" subfolder
IF NOT EXIST build (
    ECHO build folder not found
    EXIT /B 1
)
CD build

REM Compile with make and test with ctest
make && ctest -V
