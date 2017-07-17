#!/bin/bash

if [ $# != 1 ]
then
    echo "Argument needed: path to concrete/thrift."
    exit 1
fi

mkdir -p thrift

cp ${1}/*thrift thrift

echo > concrete.thrift
for x in thrift/*thrift
do
    y=`basename $x`
    echo "include \"${y}\"" >> concrete.thrift
done

thrift -out src/ -r --gen hs -I thrift concrete.thrift
rm -rf thrift concrete.thrift
mkdir -p src/Data/Concrete/Services

echo -e "module Data.Concrete (\n      module Data.Concrete" > src/Data/Concrete.hs

for x in src/*_Types.hs
do
    y=`basename $x .hs`
    echo "    , module Data.Concrete.${y}" >> src/Data/Concrete.hs
done

for x in src/*_Client.hs src/*_Iface.hs src/*Service.hs
do
    y=`basename $x .hs`
    cat ${x} |perl -pe "\$_=~s/module ${y}/module Data.Concrete.Services.${y}/;" > src/Data/Concrete/Services/${y}.hs
done

echo -e "    ) where" >> src/Data/Concrete.hs

for x in src/*_Types.hs
do
    y=`basename $x .hs`
    echo "import $y as Data.Concrete.${y}" >> src/Data/Concrete.hs
done

for x in src/*_Client.hs src/*_Iface.hs
do
    y=`basename $x .hs`
    echo "import qualified $y as Data.Concrete.${y}" >> src/Data/Concrete.hs
done
