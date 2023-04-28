#!/bin/bash

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
    echo '   [-deCSPath=<path>] - Path to the Decs Isis database.'
    echo '                        If present, mark DeCS descriptors and synonyms in the abstract.'
    exit 1
fi

#JAVA_HOME=/usr/local/java8
#JAVA_HOME=/home/users/operacao/jdk-19.0.1
#JAVA_HOME=/usr/local/java11
#JAVA_HOME=/usr/local/java8
#PATH=$JAVA_HOME/bin:$PATH

cd /home/javaapps/sbt-projects/MarkAbstract || exit

NOW=$(date +"%Y%m%d%H%M%S")

export SBT_OPTS="-Xms1g -Xmx12g"

#/usr/local/sbt/bin/sbt "runMain org.bireme.ma.MarkAbstract $1 $2 $3 $4 $5 $6"
java  -Xms1g -Xmx12g -cp target/scala-2.13/MarkAbstract-assembly-2.0.0.jar org.bireme.ma.MarkAbstract $1 $2 $3 $4 $5 $6
if [ "$?" -ne 0 ]; then
  echo "DEU ERRO!"
  exit 1
else
  [ -f logs/MarkAbstract_${NOW}.log ] && rm logs/MarkAbstract_$NOW.log
fi

cd -

exit 0

