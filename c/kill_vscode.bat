@ECHO OFF
SETLOCAL

SET VSCODE=.vscode
SET /A COUNT=0

FOR /D %%d IN (*) DO (
    IF EXIST %%d\%VSCODE% (
        ECHO Deleting %VSCODE% in %%d...
        RMDIR /S /Q %%d\%VSCODE%
        SET /A COUNT+=1
    )
)

ECHO All %VSCODE% folders have been deleted.
ECHO Total folders deleted: %COUNT%

ENDLOCAL
