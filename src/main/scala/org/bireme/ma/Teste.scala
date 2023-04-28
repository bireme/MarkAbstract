package org.bireme.ma

import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{IndexSearcher, Query, ScoreDoc}
import org.apache.lucene.store.{FSDirectory, MMapDirectory}

import java.nio.file.Paths

object Teste extends App {
  private val analyzer: StandardAnalyzer = new StandardAnalyzer()
  private val indexPath = Paths.get("decs/decs")
  //private val directory: FSDirectory = FSDirectory.open(indexPath)
  private val directory: FSDirectory = new MMapDirectory(indexPath)

  // Now search the index:
  private val ireader: DirectoryReader = DirectoryReader.open(directory)
  private val isearcher: IndexSearcher = new IndexSearcher(ireader)

  // Parse a simple query that searches for "text":
  private val parser: QueryParser = new QueryParser("term_normalized", analyzer)
  private val query: Query = parser.parse("periodontoses")
  private val hits: Array[ScoreDoc] = isearcher.search(query, 10).scoreDocs

  assert (hits.length==1)

  // Iterate through the results:
  for (i <-  hits.indices) {
    val hitDoc = isearcher.doc(hits(i).doc)
    assert("periodontoses".equals(hitDoc.get("term_normalized")))
  }

  ireader.close()
  directory.close()

  println("FIM!")
}
