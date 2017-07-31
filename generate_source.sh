#!/bin/bash

if [ $# != 1 ]
then
    echo "Argument needed: path to concrete/thrift."
    exit 1
fi

TEMPPATH=temp/

rm -f src/Data/Concrete/Autogen/*

# Create a single Thrift file that includes all Concrete files
mkdir -p ${TEMPPATH}/thrift
cp ${1}/*thrift ${TEMPPATH}/thrift
echo > ${TEMPPATH}/concrete.thrift
for x in ${TEMPPATH}/thrift/*thrift
do
    y=`basename $x`
    echo "include \"${y}\"" >> ${TEMPPATH}/concrete.thrift
done

# Invoke Thrift to auto-generate code
mkdir -p src/Data/Concrete/Autogen
thrift -out ${TEMPPATH} -r --gen hs -I ${TEMPPATH}/thrift ${TEMPPATH}/concrete.thrift
mkdir -p src/Data/Concrete/Services

#echo -e "module Data.Concrete where" > src/Data/Concrete.hs
echo -e "module Data.Concrete (\n      module Data.Concrete" > src/Data/Concrete.hs

# Client, Iface, bare
for x in ${TEMPPATH}/*Iface.hs
do
    n=`basename $x _Iface.hs`
    y=`basename $x .hs`
    #echo $y
    #echo "    , module Data.Concrete.${y}" >> src/Data/Concrete.hs
    cat ${TEMPPATH}/${n}_Iface.hs|perl -pe '$_=~s/module (\S+) where/module Data.Concrete.Autogen.\1 where/;'|perl -pe '$_=~s/import qualified (\S+)_Types/import Data.Concrete.Autogen.\1_Types as \1_Types/g;'|perl -pe '$_=~s/import ([^\s\.]+)_Types/import Data.Concrete.Autogen.\1_Types as \1_Types/g;'|perl -pe '$_=~s/import ([^\s\.]+)_Iface/import Data.Concrete.Autogen.\1_Iface as \1_Iface/g;' > src/Data/Concrete/Autogen/${n}_Iface.hs
    cat ${TEMPPATH}/${n}_Client.hs|perl -pe '$_=~s/module (\S+) where/module Data.Concrete.Autogen.\1 where/;'|perl -pe '$_=~s/import qualified (\S+)_Types/import Data.Concrete.Autogen.\1_Types as \1_Types/g;'|perl -pe '$_=~s/import ([^\s\.]+)_Types/import Data.Concrete.Autogen.\1_Types as \1_Types/g;'|perl -pe "\$_=~s/import ${n}/import Data.Concrete.Autogen.${n}/;" | perl -pe '$_=~s/import ([^\s\.]+)_Client/import Data.Concrete.Autogen.\1_Client as \1_Client/g;' > src/Data/Concrete/Autogen/${n}_Client.hs
    cat ${TEMPPATH}/${n}.hs|perl -pe '$_=~s/module (\S+) where/module Data.Concrete.Autogen.\1 where/;'|perl -pe '$_=~s/import qualified (\S+)_Types/import Data.Concrete.Autogen.\1_Types as \1_Types/g;'|perl -pe '$_=~s/import ([^\s\.]+)_Types/import Data.Concrete.Autogen.\1_Types as \1_Types/g;'|perl -pe '$_=~s/import qualified (\S+) as Iface/import qualified Data.Concrete.Autogen.\1 as Iface/;'|perl -pe '$_=~s/import qualified Service/import qualified Data.Concrete.Autogen.Service as Service/;' > src/Data/Concrete/Autogen/${n}.hs
done

# Types, Consts
for x in ${TEMPPATH}/*_Types.hs
do
    n=`basename $x _Types.hs`
    tf="${n}_Types.hs"
    cf="${n}_Consts.hs"
    echo "    , module Data.Concrete.${n}_Types" >> src/Data/Concrete.hs
    cat ${TEMPPATH}/${tf}|perl -pe '$_=~s/module (\S+) where/module Data.Concrete.Autogen.\1 where/;'|perl -pe '$_=~s/import qualified (\S+)_Types/import Data.Concrete.Autogen.\1_Types as \1_Types/g;'|perl -pe '$_=~s/import ([^\s\.]+)_Types/import Data.Concrete.Autogen.\1_Types as \1_Types/g;' > src/Data/Concrete/Autogen/${tf}
    cat ${TEMPPATH}/${cf}|perl -pe '$_=~s/module (\S+) where/module Data.Concrete.Autogen.\1 where/;'|perl -pe '$_=~s/import qualified (\S+)_Types/import Data.Concrete.Autogen.\1_Types as \1_Types/g;'|perl -pe '$_=~s/import ([^\s\.]+)_Types/import Data.Concrete.Autogen.\1_Types as \1_Types/g;' > src/Data/Concrete/Autogen/${cf}
done

#for x in src/*_Client.hs src/*_Iface.hs src/*Service.hs
#do
#    y=`basename $x .hs`
#    cat ${x} |perl -pe "\$_=~s/module ${y}/module Data.Concrete.Services.${y}/;" > src/Data/Concrete/Services/${y}.hs
#done

echo -e "    ) where" >> src/Data/Concrete.hs

for x in src/Data/Concrete/Autogen/*_Types.hs
do
   y=`basename $x .hs`
   echo "import Data.Concrete.Autogen.${y} as Data.Concrete.${y}" >> src/Data/Concrete.hs
   #echo "import Data.Concrete.Autogen.${y}" >> src/Data/Concrete.hs   
done

for x in src/Data/Concrete/Autogen/*_Client.hs src/Data/Concrete/Autogen/*_Iface.hs
do
    y=`basename $x .hs`
    echo "import Data.Concrete.Autogen.${y} as Data.Concrete.${y}" >> src/Data/Concrete.hs
    #echo "import qualified Data.Concrete.Autogen.${y} as Data.Concrete.${y}" >> src/Data/Concrete.hs
done

# Remove temporary directory
rm -rf ${TEMPPATH}
