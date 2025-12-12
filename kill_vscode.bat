@ECHO OFF
SETLOCAL

SET VSCode=.vscode
SET /A count=0

FOR /D %%d IN (*) DO (
    IF EXIST %%d\%VSCode% (
        ECHO Deleting %VSCode% in %%d...
        RMDIR /S /Q %%d\%VSCode%
        SET /A count+=1
    )
)

ECHO All %VSCode% folders have been deleted.
ECHO Total folders deleted: %count%

ENDLOCAL
