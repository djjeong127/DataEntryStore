/**
 * DataEntryStore.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: djjeong
 * Person#: 50270181
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa1

import cse250.objects.EmbeddedListNode

class DataEntryStore[A >: Null <: AnyRef](private val capacity: Int = 100)
  extends collection.mutable.Seq[A] {
  private val dataArray = Array.fill[EmbeddedListNode[A]](capacity)(new EmbeddedListNode[A])
  private var headIndex = -1
  private var tailIndex = -1
  private var numStored = 0

  /** Inserts element to tail of list. */
  def insert(elem: A): Unit = {
    if (numStored < capacity) {
      if (numStored == 0) {
        headIndex = 0
        tailIndex = 0
        dataArray(headIndex).value = elem
        numStored += 1
      }
      else {
        var count: Boolean = true
        for (i <- 0 until capacity) {
          if (dataArray(i).value == null && count) {
            dataArray(i).value = elem
            dataArray(i).prev = tailIndex
            dataArray(tailIndex).next = i

            tailIndex = i
            numStored += 1
            count = false
          }
        }
      }
    }
    else {
      val nextIndex = dataArray(headIndex).next
      dataArray(headIndex).value = elem
      dataArray(nextIndex).prev = -1
      dataArray(headIndex).next = -1
      dataArray(headIndex).prev = tailIndex
      dataArray(tailIndex).next = headIndex
      tailIndex = headIndex
      headIndex = nextIndex
    }

  }

  /** Removes all copies of the given element. */
  def remove(elem: A): Boolean = {
    var TF: Boolean = false
    var amount = countEntry(elem)
    var count = 0
    for (i <- 0 until capacity) {
      if (numStored > 0 && count < amount) {
        if (dataArray(i).value == elem && elem != null) {
          if (numStored == 1) {
            dataArray(i).value = null
            headIndex = -1
            tailIndex = -1
          }
          else if (i == headIndex) {
            headIndex = dataArray(i).next
            dataArray(dataArray(i).next).prev = -1
            dataArray(i).value = null
            dataArray(i).next = -1
          }
          else if (i == tailIndex) {
            tailIndex = dataArray(i).prev
            dataArray(dataArray(i).prev).next = -1
            dataArray(i).value = null
            dataArray(i).prev = -1
          }
          else {
            dataArray(dataArray(i).prev).next = dataArray(i).next
            dataArray(dataArray(i).next).prev = dataArray(i).prev
            dataArray(i).value = null
            dataArray(i).prev = -1
            dataArray(i).next = -1
          }
          TF = true
          numStored -= 1
          count += 1
        }
      }
    }

    TF
  }

  /** Returns the count of nodes containing given entry. */
  def countEntry(entry: A): Int = {
    var count = 0
    for (i <- 0 until capacity) {
      if (dataArray(i).value == entry) {
        count += 1
      }
    }
    count
  }

  /** Gets the element at the specified index. */
  override def apply(idx: Int): A = {
    var node = dataArray(headIndex)
    for (i <- 0 until idx) {
      node = dataArray(node.next)
    }
    node.value
  }

  /** Replaces element at given index with a new value. */
  override def update(idx: Int, elem: A): Unit = {
    var node = dataArray(headIndex)
    for (i <- 0 until idx) {
      node = dataArray(node.next)
    }
    node.value = elem
  }

  /** Returns an Iterator that can be used only once. */
  def iterator: Iterator[A] = new Iterator[A] {
    private var current = headIndex

    override def hasNext: Boolean = current != -1

    override def next(): A = {
      val prev = current
      current = dataArray(current).next
      dataArray(prev).value
    }
  }

  /** Returns the length of the stored list. */
  override def length: Int = numStored

  override def toString: String = if (numStored == 0) "" else this.iterator.addString(new StringBuilder, "\n").result()
}
