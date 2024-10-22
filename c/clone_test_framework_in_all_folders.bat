@ECHO OFF
SETLOCAL ENABLEDELAYEDEXPANSION

:: Subfolder name in root folder to be targeted by hard junctions:
SET FOLDER=test-framework

:: The corresponding full path for "test-framework":
SET FULL=%CD%\%FOLDER%

:: Loop over all subfolders to find all sub-subfolders named "test-framework":
FOR /D %%d IN (*) DO (
    REM Log current subfolder being processed:
    ECHO %%d

    REM Cache current sub-subfolder path:
    SET SUBFOLDER=%%d\%FOLDER%

    REM Check if current subfolder contains a "test-framework" inside it:
    IF EXIST %SUBFOLDER% (
        REM Check if it's a hard junction folder type:
        DIR /AL %%d | FINDSTR /I "JUNCTION" >NUL

        REM If it isn't a junction, check if it's perhaps a softlink instead:
        IF ERRORLEVEL 1 POWERSHELL -C "if (Test-Path %SUBFOLDER%) {^
            Get-Item %SUBFOLDER% |^
            Where-Object { $_.LinkType -eq 'SymbolicLink' } }" ^|
            FINDSTR /I "SymbolicLink" >NUL

        REM If we still have an error status, it means it's a regular folder:
        IF ERRORLEVEL 1 (
            REM So we have to delete it along its content before proceeding:
            RMDIR /S /Q %SUBFOLDER%

            REM And then replace it w/ its corresponding junction mirror:
            CALL :CreateJunction %SUBFOLDER%
        )

    REM If it doesn't, just add a junction if it's not the same folder:
    ) ELSE IF NOT %CD%\%%d==%FULL% CALL :CreateJunction %SUBFOLDER%
)

:CreateJunction
    :: Create a junction in the given location targeting "test-framework":
    MKLINK /J %1 %FULL%
    EXIT /B

ENDLOCAL
