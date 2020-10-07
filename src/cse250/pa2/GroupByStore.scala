/**
 * GroupByStore.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: motazkha
 * Person#: 50231669
 *
 * Citations:
 * PA1 Iterator offered by Prof. Hughes
 * https://www.scala-lang.org/api/current/scala/collection/mutable/ArrayBuffer.html
 * https://alvinalexander.com/scala/arraybuffer-class-methods-syntax-examples-reference
 * https://alvinalexander.com/source-code/how-to-create-scala-arraybuffer-syntax
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa2

import cse250.objects.{DNode, TaxEntry}
import collection.mutable.ArrayBuffer
import util.control.Breaks._

class GroupByStore {
  // Feel free to change the default value of groupings and modify it to val/var.
  private var groupings: ArrayBuffer[DNode[TaxEntry]] = new ArrayBuffer[DNode[TaxEntry]]
  private var groupingAttribute = "STREET"
  private var numStored = 0
  private var numStored2 = 0

  /** Inserts element to head of corresponding grouping list. */
  def insert(taxEntry: TaxEntry): Unit = {    var TaxNode = new DNode[TaxEntry](taxEntry, null, null)
    if (groupings.size == 0){
      groupings += TaxNode
      numStored = numStored.+(1)
    } else {
      breakable {
        for (i <- 0 to groupings.size) {
          if (i == groupings.size) {
            groupings += TaxNode
            numStored = numStored.+(1)
            break()
          } else {
            if (groupings(i).value.infoMap.getOrElse(groupingAttribute, "") == taxEntry.infoMap.getOrElse(groupingAttribute, "")) {
              TaxNode.next = groupings(i)
              groupings(i).prev = TaxNode
              groupings(i) = TaxNode
              numStored = numStored.+(1)
              break()
            } else if (taxEntry.infoMap.getOrElse(groupingAttribute, "") < groupings(i).value.infoMap.getOrElse(groupingAttribute,"")) {
              groupings.insert(i, TaxNode)
              numStored = numStored.+(1)
              break()
            }
          }

        }
      }
    }
  }

  /** Regroup . */
  def regroup(attribute: String): Unit = {
    groupingAttribute = attribute
    var groupings2 = groupings.clone()
    groupings.clear()
    for (i <- 0 until groupings2.size) {
      var currentnode: DNode[TaxEntry] = groupings2(i)
      while (currentnode != null) {
        this.insert(currentnode.value)
        currentnode = currentnode.next
      }
    }
  }


  /*
      groupingAttribute = attribute
      var groupings2: ArrayBuffer[DNode[TaxEntry]] = new ArrayBuffer[DNode[TaxEntry]]
      var again = this.iterator
      while(again.hasNext){

        var TaxNode = new DNode[TaxEntry](again.next(), null, null)
        if (groupings2.size == 0){
          groupings2 += TaxNode
          numStored2 = numStored2.+(1)
        } else {
          breakable {
            for (i <- 0 to groupings2.size) {
              if (i == groupings2.size) {
                groupings2 += TaxNode
                numStored2 = numStored2.+(1)
                break()
              } else {
                if (groupings2(i).value.infoMap.getOrElse(groupingAttribute, "") == TaxNode.value.infoMap.getOrElse(groupingAttribute, "")) {
                  TaxNode.next = groupings2(i)
                  groupings2(i).prev = TaxNode
                  groupings2(i) = TaxNode
                  numStored2 = numStored2.+(1)
                  break()
                } else if (TaxNode.value.infoMap.getOrElse(groupingAttribute, "") < groupings2(i).value.infoMap.getOrElse(groupingAttribute,"")) {
                  groupings2.insert(i, TaxNode)
                  numStored2 = numStored2.+(1)
                  break()
                }
              }

            }
          }
        }

      groupings = groupings2
    }
    */
  /*
  while (currentnode.next != null){
    var TaxNode = new DNode[TaxEntry](currentnode.value, null, null)
    if (numStored == 0){
      groupings2 += TaxNode
      numStored = numStored.+(1)
    } else {
      breakable {
        for (i <- 0 to groupings2.size) {
          if (i == groupings2.size) {
            groupings2 += TaxNode
            numStored = numStored.+(1)
            break()
          } else {
            if (groupings2(i).value.infoMap.getOrElse(groupingAttribute, "") == currentnode.value.infoMap.getOrElse(groupingAttribute, "")) {
              TaxNode.next = groupings2(i)
              groupings2(i).prev = TaxNode
              groupings2(i) = TaxNode
              numStored = numStored.+(1)
              break()
            } else if (groupings2(i).value.infoMap.getOrElse(groupingAttribute, "") < currentnode.value.infoMap.getOrElse(groupingAttribute, "")) {
              groupings2.insert(i, TaxNode)
              numStored = numStored.+(1)
              break()
            }
          }

        }
      }
    }
    currentnode = currentnode.next
  }
  */
  // }

  // groupings = groupings2
  //}

  /** Returns an Iterator to all entries that can be used only once. */
  def iterator: Iterator[TaxEntry] = new Iterator[TaxEntry] {
    private var Jumper = 0
    private var current: DNode[TaxEntry] = null
    if (groupings.size > 0){
      current = groupings(0)
    }

    override def hasNext: Boolean = {
      Jumper != groupings.size
    }

    override def next(): TaxEntry = {

      val prev = current
      if (current != null) {

        if (current.next != null) {
          current = current.next
        } else if (current.next == null) {
          Jumper = Jumper.+(1)
          if (this.hasNext){
            //   if (Jumper < groupings.size) {
            current = groupings(Jumper)
          }
        }

      }
      prev.value
    }
  }

  /** Returns an Iterator to only the entries with matching values on the grouping attribute that can be used only once. */
  def iterator(value: String): Iterator[TaxEntry] = new Iterator[TaxEntry] {
    var j = 0
    var exists = false
    for ( i <- 0 until groupings.length){
      if (groupings(i).value.infoMap.getOrElse(groupingAttribute, "") == value){
        j = i
        exists = true
      }
    }
    private var current: DNode[TaxEntry] = null
    if (exists){
      current = groupings(j)
    }
    override def hasNext: Boolean = {
      current != null
    }

    override def next(): TaxEntry = {
      val prev = current
      if (current != null) {
        current = current.next
      }
      prev.value
    }
  }

  def length: Int = numStored
  def size: Int = groupings.length

  override def toString: String = if (numStored == 0) "" else this.iterator.addString(new StringBuilder, "\n").result()
}
