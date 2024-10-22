@ECHO OFF

ECHO Working on current directory:
pnpm i

FOR /D %%d IN (*) DO (
    ECHO.
    ECHO Working on directory: %%d
    pnpm -C %%d i
)
