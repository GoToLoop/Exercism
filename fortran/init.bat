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

REM Replace '-' with '_' in the project name
SET project=%project:-=_%

REM Create a Fortran source file with the same name as the folder
IF NOT EXIST "%project%.f90" TYPE NUL > "%project%.f90"

REM Remove build directory if it exists
IF EXIST build RMDIR /S /Q build

REM Create build directory and move into it
MKDIR build & CD build

REM Run CMake with Unix Makefiles generator
cmake -G "Unix Makefiles" ..
