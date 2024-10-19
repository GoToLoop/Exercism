@ECHO OFF

FOR /D %%d IN (*) DO (
    ECHO.
    ECHO Working on directory: %%d
    pnpm -C %%d i
)
