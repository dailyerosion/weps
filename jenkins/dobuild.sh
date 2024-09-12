#!/bin/bash

# the required command arguments from the jenkins environment
echo "ARCH is set to:" $arch
echo "COMPILER is set to:" $compiler
echo "MODE is set to:" $mode
echo "VERSION is set to:" $version
echo "RELEASE is set to:" $release

echo Ready to build...
echo BUILD_NUMBER : $BUILD_NUMBER
echo EXECUTOR_NUMBER : $EXECUTOR_NUMBER
echo NODE_NAME : $NODE_NAME
echo WORKSPACE : $WORKSPACE
echo JENKINS_HOME : $JENKINS_HOME
echo BUILD_URL : $BUILD_URL
echo SVN_URL : $SVN_URL
echo  
#exit 0

#remove any old artifacts
rm -rf dist
rm -rf config.cook

#mangle the compiler to allow 32bit cross compiles on 64bit machines

case $arch in
     "x86" )
           bit=32;
           ;;
     "x86_64" )
           bit=64;
           ;;
     * )
           bit=$arch;
           ;;
esac 

case $compiler in
     "gfortran" )
           compiler=$compiler$bit;
           ;;
     "g95" )
           compiler=$compiler$bit;
           ;;
     "ifort" )
           compiler=$compiler$bit;           
           ;;
esac   

cook -NL configure fc=$compiler mode=$mode
cook -NL clean
cook -NL version=$version release=$release show weps sweep
exit 0

