#!/bin/bash

set -e

BIN_DIR_REL=`dirname "${BASH_SOURCE[0]}"`
BIN_DIR=`readlink -f ${BIN_DIR_REL}`

MCR30_DIR=${BIN_DIR}/../../extjwnl-data-mcr30
SPA_DIR=${MCR30_DIR}/lang-spa/src/main/resources/net/sf/extjwnl/data/mcr30/spa
ALIGN_DIR=${MCR30_DIR}/alignment/src/main/resources/net/sf/extjwnl/data/mcr30/alignment

MAPPING_JSON=${BIN_DIR}/mapping_wordnet.json
ALIGN_CSV=${ALIGN_DIR}/wn31-30.csv

pushd ${BIN_DIR}
wget https://github.com/ozendelait/wordnet-to-json/raw/master/mapping_wordnet.json
wget https://sourceforge.net/projects/extjwnl/files/extjwnl-2.0.3.zip
unzip extjwnl-2.0.3.zip
cp ${SPA_DIR}/res_properties.xml ./res_properties.xml
cp ./res_properties.xml ./ewn.xml
sed -i "s|/net|${MCR30_DIR}/lang-spa/src/main/resources/net|g" ewn.xml
sed -i "s|PrincetonResourceDictionaryFile|PrincetonRandomAccessDictionaryFile|g" ewn.xml
wget https://github.com/lingeringsocket/wn-mcr-transform/archive/master.zip
unzip master.zip wn-mcr-transform-master/wordnet_spa.tar.gz
pushd ${SPA_DIR}
rm -f *
tar xzvf ${BIN_DIR}/wn-mcr-transform-master/wordnet_spa.tar.gz
rm -f README.txt
cp ${BIN_DIR}/res_properties.xml ./res_properties.xml
popd
extjwnl-2.0.3/bin/ewn -script supplemental_spa.txt
awk '/synset-mapping/{flag=1;next}/synset-missing/{exit}flag' ${MAPPING_JSON} | head -n -1 | tr -s '":, ' ' ' | awk '{$1=$1};1' | sort -k 2 | tr -s ' ' ',' > ${ALIGN_CSV}
rm -rf master.zip wn-mcr-transform-master extjwnl-2.0.3.zip extjwnl-2.0.3 ewn.xml ${MAPPING_JSON} res_properties.xml
popd
