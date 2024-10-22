@ECHO OFF
SETLOCAL

SET LOCK_FILE=pnpm-lock.yaml
SET NODE_MODULES=node_modules
SET /A COUNT=0

IF EXIST %LOCK_FILE% DEL /Q %LOCK_FILE%

IF EXIST %NODE_MODULES% (
    ECHO Deleting %NODE_MODULES% in current directory:
    CD
    RMDIR /S /Q %NODE_MODULES%
    SET /A COUNT+=1
)

FOR /D %%d IN (*) DO (
    IF EXIST %%d\%LOCK_FILE% DEL /Q %%d\%LOCK_FILE%

    iF EXIST %%d\%NODE_MODULES% (
        ECHO Deleting %NODE_MODULES% in %%d...
        RMDIR /S /Q %%d\%NODE_MODULES%
        SET /A COUNT+=1
    )
)

ECHO All %NODE_MODULES% folders have been deleted.
ECHO Total folders deleted: %COUNT%

ENDLOCAL
