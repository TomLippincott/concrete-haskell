#!/bin/bash

if [ $# != 2 ]
then
    echo "Two arguments needed: path to concrete/thrift, and concrete-services/thrift."
    exit 1
fi

mkdir -p thrift

cp ${1}/*thrift thrift
cp ${2}/*thrift thrift

echo > concrete.thrift
for x in thrift/*thrift
do
    y=`basename $x`
    echo "include \"${y}\"" >> concrete.thrift
done

thrift -out src/ -r --gen hs -I thrift concrete.thrift

rm -rf thrift concrete.thrift

cat src/Structure_Types.hs | perl -pe '$_=~s/\-1,/\(\-1\),/g;' > temp.hs
mv temp.hs src/Structure_Types.hs

mkdir -p src/Data

echo -e "module Data.Concrete (\n      module Data.Concrete" > src/Data/Concrete.hs
for x in src/*_Types.hs
do
    y=`basename $x .hs`
    echo "    , module $y" >> src/Data/Concrete.hs
done
echo -e "    ) where" >> src/Data/Concrete.hs
for x in src/*_Types.hs
do
    y=`basename $x .hs`
    echo "import $y" >> src/Data/Concrete.hs
done
