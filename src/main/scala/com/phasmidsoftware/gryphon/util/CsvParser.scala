package com.phasmidsoftware.gryphon.util

import com.phasmidsoftware.gryphon.applications.mst.EdgeDataMST
import com.phasmidsoftware.gryphon.core._
import com.phasmidsoftware.parse._
import com.phasmidsoftware.table.{HeadedTable, Header, Table}
import scala.util.Try
import scala.util.matching.Regex

object CsvParser {

    def parseUndirectedEdgeList[V: Ordering : CellParser, E: Ordering : CellParser](resource: String): Try[Iterable[UndirectedOrderedEdge[V, E]]] = {

        object EdgeDataMSTParser extends CellParsers {

            implicit val edgeDataMSTParser: CellParser[EdgeDataMST[V, E]] = cellParser3(EdgeDataMST[V, E])

            implicit object EdgeDataMSTConfig extends DefaultRowConfig {
                override val listEnclosure: String = ""
                override val delimiter: Regex = """\s+""".r
                override val string: Regex = """[^ "]*""".r

            }

            val parser: StandardRowParser[EdgeDataMST[V, E]] = StandardRowParser.create[EdgeDataMST[V, E]]
        }

        trait EdgeDataMSTTableParser extends StringTableParser[Table[EdgeDataMST[V, E]]] {
            type Row = EdgeDataMST[V, E]

            val maybeFixedHeader: Option[Header] = Some(Header.create("vertex1", "vertex2", "edge"))

            val headerRowsToRead: Int = 0

            override val forgiving: Boolean = false

            val rowParser: RowParser[EdgeDataMST[V, E], String] = EdgeDataMSTParser.parser

            protected def builder(rows: Iterable[EdgeDataMST[V, E]], header: Header): Table[EdgeDataMST[V, E]] = HeadedTable(rows, header)
        }

        implicit object EdgeDataMSTTableParser extends EdgeDataMSTTableParser

        val dty: Try[Table[EdgeDataMST[V, E]]] = Table.parseResource[Table[EdgeDataMST[V, E]]](resource)

        val esy: Try[Iterable[UndirectedOrderedEdgeCase[V, E]]] =
            for (et <- dty) yield et.map(e => UndirectedOrderedEdgeCase(e.vertex1, e.vertex2, e.edge)).rows

        esy
    }
}
