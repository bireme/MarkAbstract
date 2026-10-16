#!/bin/bash

if [ -z "$JAVA_HOME_25" ]; then
  JAVA_HOME_25="/home/users/operacao/.cache/coursier/arc/https/github.com/graalvm/graalvm-ce-builds/releases/download/jdk-25.0.1/graalvm-community-jdk-25.0.1_linux-x64_bin.tar.gz/graalvm-community-openjdk-25.0.1+8.1"
fi

PATH=$JAVA_HOME_25/bin:$PATH

java -version

if [ "$#" -lt "4" ]
  then
    echo 'MarkAbstract shell takes a list of xml documents and add to them the'
    echo 'the field <ab_mark> or <ab_*_mark> where every occurrence of the text'
    echo 'xxx: yyy of the field <ab> or <ab_*> is replaced by <h2>xxx</h2>: yyy.'
    echo
    echo 'usage: MarkAbstract'
    echo '   <prefixFile> - file having some words allowed in the abstract tag. For ex, "Results":'
    echo '   <inDir> - directory having the input files used to created the marked ones.'
    echo '   <xmlFileRegexp> - regular expression to filter input files that follow the pattern <word>..<word>:'
    echo '   <outDir> - the directory into where the output files will be written.'
    echo '   [-days=<days>] - if present only marks the files that are'
    echo '                    changed in the last <days>. If absent marks all filtered files.'
    echo '   [-deCSPath=<path>] - Path to the DeCS Isis database.'
    echo '                        If present, mark DeCS descriptors and synonyms in the abstract.'
    exit 1
fi

cd /home/javaapps/sbt-projects/MarkAbstract || exit

NOW=$(date +"%Y%m%d%H%M%S")

echo
echo "prefixFile=$1"
echo "inDir=$2"
echo "xmlFileRegexp=$3"
echo "outDir=$4"
echo "$5"
echo

#export JAVA_TOOL_OPTIONS="-Djdk.xml.totalEntitySizeLimit=10000000 -Djdk.xml.maxGeneralEntitySizeLimit=10000000"
export JAVA_TOOL_OPTIONS="-Djdk.xml.totalEntitySizeLimit=300000 -Djdk.xml.maxGeneralEntitySizeLimit=300000"

time java  -Xms1g -Xmx12g -cp target/scala-3.3.7/MarkAbstract-assembly-2.16.1.jar:lib/decshighlighter_3-0.1.jar org.bireme.ma.MarkAbstract $1 $2 $3 $4 $5 $6 &>logs/MarkAbstract_$NOW.log
MA="$?"

if [ $MA -ne 0 ]; then
  sendemail -f appofi@bireme.org -u "Mark Abstract Service - Xmls creation ERROR - $(date '+%Y%m%d')" -m "Mark Abstract Service - Erro na criacao dos xmls marcados - $1 $2 $3 $4 $5 $6" -t barbieri@paho.org -cc appofi@bireme.org -s esmeralda.bireme.br -a logs/MarkAbstract_$NOW.log
  SM="$?"
  [ $SM -ne 0 ] && echo "Falha no envio de e-mail"
  exit 1
else
  [ -f logs/MarkAbstract_${NOW}.log ] && rm logs/MarkAbstract_${NOW}.log
fi

# Email avisando que o processamento finalizou corretamente
#sendemail -f appofi@bireme.org -u "Mark Abstract Service - Processing finished - `date '+%Y%m%d'`" -m "Mark Abstract Service Service - Fim do processamento" -t ofi@paho.org -s esmeralda.bireme.br

cd -

exit 0
