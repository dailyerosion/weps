
@echo off
SETLOCAL ENABLEEXTENSIONS

set arch=%1
set compiler=%2
set mode=%3

if "%arch%"=="x86" (
	echo Calling ia32 windows
	rem call "C:\Program Files (x86)\Intel\Composer XE\bin\compilervars.bat" ia32
	call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\bin\compilervars.bat" ia32
) else if "%arch%"=="x86_64" (
	echo Calling intel64 windows
	rem call "C:\Program Files (x86)\Intel\Composer XE\bin\compilervars.bat" intel64
	call "C:\Program Files (x86)\IntelSWTools\compilers_and_libraries\windows\bin\compilervars.bat" intel64
) else (
	echo Bad value, ARCH=%arch%
)

echo "ARCH = " %arch%
echo "COMPILER = " %compiler%
echo "MODE = " %mode%

bash jenkins/dobuild.sh

