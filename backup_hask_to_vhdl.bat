@echo off 
REM This script backs up the hask-to-vhdl directory.
REM This script is make-before-break in the sense that the hard drive is never allowed 
REM to be the only copy. Therefore, this script renames the previous backup before
REM copying the new backup. 

REM Before renaming hask-to-vhdl to hask-to-vhd01, we first have to delete the previous
REM hask-to-vhd01:
rmdir /q /s D:\hask-to-vhdl_backups\hask-to-vhd01

REM Now we can rename the most recent backup:
ren D:\hask-to-vhdl_backups\hask-to-vhdl hask-to-vhd01

REM Now copy the latest directory:
Xcopy /E /I C:\Users\jstrieter\hask-to-vhdl D:\hask-to-vhdl_backups\hask-to-vhdl
