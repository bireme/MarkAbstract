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

cd /home/javaapps/sbt-projects/MarkAbstract

sbt "runMain org.bireme.ma.MarkAbstract $1 $2 $3 $4 $5 $6"
if [ "$?" -ne 0 ]; then
  sendemail -f appofi@bireme.org -u "Mark Abstract Service - Xmls creation ERROR - `date '+%Y%m%d'`" -m "Mark Abstract Service - Erro na criação dos xmls marcados" -t barbieri@paho.org -cc ofi@bireme.org -s esmeralda.bireme.br
  exit 1
fi

# Email avisando que o processamento finalizou corretamente
sendemail -f appofi@bireme.org -u "Mark Abstract Service - Processing finished - `date '+%Y%m%d'`" -m "Mark Abstract Service Service - Fim do processamento" -t barbieri@paho.org -cc mourawil@paho.org -s esmeralda.bireme.br

cd -

