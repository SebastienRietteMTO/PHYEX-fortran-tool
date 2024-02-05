#!/bin/bash

set -e
set -o pipefail #abort if left command on a pipe fails

fxtran_version=d89af8c67cf2e134ed43b5e689d639a9e07215ff

#This script automatically clones and compiles the fxtran tool.
#In addition a symbolic link to the fxtran binary is put in the bin directory.
#Call the script with the -h option to get more information.

################################
#### COMMAND LINE ARGUMENTS ####
################################

function usage {
  echo "Usage: $0 [-h] [--clean] [--ssh]"
  echo "--clean         remove the fxtran installation instead of installing it"
  echo "--ssh           use the ssh protocol to clone the fxtran repository instead of https"
  echo ""
  echo "Without option, this script automatically clones and compiles the fxtran tool."
  echo "If fxtran is already there, it is updated."
  echo "In addition a symbolic link to the fxtran binary is put in the bin directory."
  echo ""
  echo "With the --clean option, the script removes the fxtran compilation."
}

clean=0
ssh=0
while [ -n "$1" ]; do
  case "$1" in
    '-h') usage; exit;;
    '--clean') clean=1;;
    '--ssh') ssh=1;;
    *) echo "The option '$1' does not exist for this script"; exit 2;;
  esac
  shift
done

#################################
#### INSTALLATION / CLEANING ####
#################################

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ $clean -eq 1 ]; then
  rm -f $DIR/fxtran
  rm -rf $DIR/../site/fxtran
else
  cd $DIR/../site/
  compilationNeeded=0
  if [ ! -d fxtran ]; then
    if [ $ssh -eq 1 ]; then
      git clone git@github.com:pmarguinaud/fxtran.git
    else
      git clone https://github.com/pmarguinaud/fxtran
    fi
    compilationNeeded=1
  else
    cd fxtran
    if [ $(git rev-parse HEAD^{commit}) != $(git rev-parse ${fxtran_version}^{commit}) ]; then
      rm -f $DIR/fxtran
      make clean
      git checkout ${fxtran_version}
      make clean
      compilationNeeded=1
    fi
  fi
  if [ $compilationNeeded -eq 1 ]; then
    cd fxtran
    set +e
    make all
    set -e
    
    if [ -f bin/fxtran ]; then
      echo ""
      echo "Don't be afraid if you see some errors; on some systems the build step is known to produce errors."
      echo "In your case everything seems to have gone smoothly, as the executable file exists."
      echo ""
      echo "To use pyft independently, you need to source the '$DIR/env.sh' file."
      cd $DIR
      ln -s $DIR/../site/fxtran/bin/fxtran .
      exit 0
    else
      echo "Something goes wrong, the bin/fxtran executable file does not exist"
      exit 1
    fi
  fi
fi
