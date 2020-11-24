#!/bin/bash
#
# morphala:  word morphology for Scala
# Copyright 2020-2020 John V. Sichi
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -e

data_dir_rel=`dirname "${BASH_SOURCE[0]}"`
data_dir=`readlink -f ${data_dir_rel}`
jehle_url="https://github.com/ghidinelli/fred-jehle-spanish-verbs"
jehle_hash="b9c93b4b01fdbee2085b5154f911f55e41cd5b57"
lemarios_url="https://github.com/olea/lemarios"
lemarios_hash="441c352ffa7478b7498c081ce48c79d5d9c926c7"

pushd ${data_dir}

wget ${jehle_url}/raw/${jehle_hash}/jehle_verb_database.csv
dos2unix jehle_verb_database.csv
wget ${lemarios_url}/raw/${lemarios_hash}/verbos-espanol.txt
wget ${lemarios_url}/raw/${lemarios_hash}/verbos-espanol-conjugaciones.txt

popd
