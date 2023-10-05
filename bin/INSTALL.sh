#!/bin/bash

set -e

#Without option, this script automatically clones and compiles the fxtran tool.
#In addition a symbolic link to the fxtran binary is put in the bin directory.

#With the --clean option, the script removes the fxtran compilation.

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ "$1" == "--clean" ]; then
  rm -f $DIR/fxtran
  rm -rf $DIR/../site/fxtran

elif [ "$1" == "-h" ]; then
  echo "Usage: $0 [-h] [--clean]"
  echo ""
  echo "Without option, this script automatically clones and compiles the fxtran tool."
  echo "In addition a symbolic link to the fxtran binary is put in the bin directory."
  echo ""
  echo "With the --clean option, the script removes the fxtran compilation."

elif [ "$1" != "" ]; then
  echo "The option '$1' does not exist for this script"
  exit 2

else
  cd $DIR/../site/
  git clone https://github.com/pmarguinaud/fxtran
  cd fxtran
  set +e
  make all
  set -e
  
  if [ -f bin/fxtran ]; then
    echo ""
    echo "Don't be afraid if you see some errors; on some systems the build step is known to produce errors."
    echo "In your case everything seems to have gone smoothly, as the executable file exists."
    echo ""
    echo "You must source the '$DIR/env.sh' file."
    cd $DIR
    ln -s $DIR/../site/fxtran/bin/fxtran .
    exit 0
  else
    echo "Something goes wrong, the bin/fxtran executable file does not exist"
    exit 1
  fi
fi
