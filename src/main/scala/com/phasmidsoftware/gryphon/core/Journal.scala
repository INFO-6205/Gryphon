package com.phasmidsoftware.gryphon.core

import java.io.FileWriter
import scala.collection.immutable.Queue

/**
 * Trait to define the behavior of a Journal of V elements.
 * It is used in the Visitor trait as something which can receive the V elements.
 * J Journal does not support an iterator, since many journals might simply be a log file of some sort.
 *
 * @tparam J the journal type.
 * @tparam V the underlying type of the journal.
 */
trait Journal[J, V] {

    /**
     * An empty journal.
     */
    def empty: J

    /**
     * Method to append a V value to a journal.
     *
     * @param j the journal to be appended to.
     * @param v an instance of V to be appended to the journal j.
     * @return a new journal.
     */
    def append(j: J, v: V): J
}

/**
 * Trait which combines the behaviors of Journal and HasIterator.
 *
 * @tparam J the (iterable) journal type for this IterableJournal (J must be a sub-class of Iterable[V]).
 * @tparam V the underlying type of the journal.
 */
trait IterableJournal[J <: Iterable[V], V] extends Journal[J, V] with HasIterator[J, V]

trait IterableJournalQueue[V] extends IterableJournal[Queue[V], V] {
    val empty: Queue[V] = Queue.empty

    def append(q: Queue[V], v: V): Queue[V] = q.enqueue(v)
}

trait IterableJournalStack[V] extends IterableJournal[List[V], V] {
    val empty: List[V] = List.empty

    def append(q: List[V], v: V): List[V] = v :: q
}

object Journal {
    implicit object StringBuilderJournalInt$$ extends Journal[StringBuilder, Int] {
        def empty: StringBuilder = new StringBuilder()

        def append(j: StringBuilder, v: Int): StringBuilder = j.append(s"$v\n")
    }

    implicit object FileWriterJournalInt$$ extends Journal[FileWriter, Int] {
        /**
         * This method is used only when no explicit Journal is defined for a Visitor[FileWriter, Int].
         * @return a new FileWriter based on the file called "output.txt".
         */
        def empty: FileWriter = new FileWriter("output.txt")

        def append(j: FileWriter, v: Int): FileWriter = { j.append(s"$v\n"); j }
    }

    implicit object IterableJournalQueueInt$$ extends IterableJournalQueue[Int]

    implicit object IterableJournalStackInt$$ extends IterableJournalStack[Int]

    implicit object IterableJournalQueueString$$ extends IterableJournalQueue[String]
}
