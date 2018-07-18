@echo off

title extraction des lignes de couleur

set currentDir="%cd%"

set R_HOME="%cd%\lib\R\R-3.1.2"
set R_LIBS_USER="%R_HOME%\library"

if  %PROCESSOR_ARCHITECTURE%==x86 (set bindir=i386) else (set bindir=x64)

::set R_MAX_MEM_SIZE=1GB
::--max-vsize=500M

chcp 1252 > NUL

%R_HOME%\bin\%binDir%\Rscript lib\myRScripts\extraction.R %currentDir%

pause