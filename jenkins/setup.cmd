
@echo off
SETLOCAL ENABLEEXTENSIONS

set arch=%1
set compiler=%2
set mode=%3
set version=$4
set release=$5

if "%arch%"=="x86_64" (
	echo Windows, ifx defaults to 64 bit
	rem call "C:\Program Files (x86)\Intel\Composer XE\bin\compilervars.bat" intel64
	call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat"
) else (
	echo Bad value, ARCH=%arch%
)

echo "ARCH = " %arch%
echo "COMPILER = " %compiler%
echo "MODE = " %mode%
echo "VERSION = " %version%
echo "RELEASE = " %release%

bash jenkins/dobuild.sh

