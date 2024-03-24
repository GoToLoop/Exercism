@echo off

FOR /D %%G IN (".\*") DO (
    jsdoc "%%G" -c jsdoc.json -d docs/jsdoc/%%~nG
)
