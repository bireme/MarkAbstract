package org.bireme.ma

import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{IndexSearcher, Query, ScoreDoc}
import org.apache.lucene.store.{FSDirectory, MMapDirectory}

import java.nio.file.Paths

object Teste {
  def main(args: Array[String]): Unit = {
    val analyzer: StandardAnalyzer = new StandardAnalyzer()
    val indexPath = Paths.get("decs/decs")
    //val directory: FSDirectory = FSDirectory.open(indexPath)
    val directory: FSDirectory = new MMapDirectory(indexPath)

    // Now search the index:
    val ireader: DirectoryReader = DirectoryReader.open(directory)
    val isearcher: IndexSearcher = new IndexSearcher(ireader)

    // Parse a simple query that searches for "text":
    val parser: QueryParser = new QueryParser("term_normalized", analyzer)
    val query: Query = parser.parse("periodontoses")
    val hits: Array[ScoreDoc] = isearcher.search(query, 10).scoreDocs

    assert(hits.length == 1)

    // Iterate through the results:
    for (i <- hits.indices) {
      val hitDoc = isearcher.storedFields().document(hits(i).doc)
      assert("periodontoses".equals(hitDoc.get("term_normalized")))
    }

    ireader.close()
    directory.close()

    println("FIM!")
  }
}
