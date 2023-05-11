#!/bin/bash

# the required command arguments from the jenkins environment in 1-2-3-4 order
os=$1
arch=$2
compiler=$3
mode=$4

echo "OS:" $1
echo "ARCH:" $2
echo "COMPILER:" $3
echo "MODE:" $4

case $compiler in
	"ifort" )
		case $os in
			"linux" )
				case $arch in
					"x86" )
						echo "Calling ia32"
						source /opt/intel/oneapi/setvars.sh ia32
						;;
					"x86_64" )
						echo "Calling intel64"
						source /opt/intel/oneapi/setvars.sh intel64
						;;
					* )
						echo "Bad value, ARCH=$arch";
						;;
				esac
				source jenkins/dobuild.sh;
				;;
			"windows" )
				# this then must call dobuild.sh as a subprocess
				# to preserve the environment needed to do the build
				echo "WINDOWS";
				cmd.exe /c jenkins\\setup.cmd $arch $compiler $mode;
				;;
		esac
		;;
	* )
		source jenkins/dobuild.sh;
		;;
esac
exit 0
