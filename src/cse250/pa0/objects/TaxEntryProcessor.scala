/**
 * TaxEntryProcessor.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:motazkha
 * Person#:50231669
 *citations:
 * understanding maps:
 * https://docs.scala-lang.org/overviews/collections-2.13/maps.html
 * https://www.scala-lang.org/api/current/scala/collection/concurrent/Map.html
 * https://alvinalexander.com/scala/how-to-create-maps-class-examples-scala-cookbook
 * textbook maps section
 * Understanding Strings:
 * https://www.scala-lang.org/api/current/scala/package$$$hash$colon$colon$.html#toString():String
 * https://www.scala-lang.org/api/2.12.3/scala/collection/immutable/StringOps.html
 * https://alvinalexander.com/scala/scala-count-number-occurrences-character-in-string
 * Understanding rrays and Arraybuffers:
 * https://www.scala-lang.org/api/current/scala/collection/mutable/ArrayBuffer.html
 * https://alvinalexander.com/scala/arraybuffer-class-methods-syntax-examples-reference
 * https://www.scala-lang.org/api/current/scala/collection/ArrayOps.html
 * https://docs.scala-lang.org/overviews/collections-2.13/arrays.html
 * Understanding Classes:
 * https://docs.scala-lang.org/tour/classes.html
 * https://alvinalexander.com/scala/scala-class-examples-constructors-case-classes-parameters
 * Understanding do-while loops:
 * https://alvinalexander.com/scala/while-do-while-loops-syntax-examples
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa0.objects

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import scala.io.Source
import cse250.assignments.objects.TaxEntry

import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer

object TaxEntryProcessor {
  def sanitizeData(filename: String): Unit = {
    // For opening files, look at Scala Cookbook File I/O Excerpt
    val inputFile = scala.io.Source.fromFile(filename)
    // Note: lines is an iterator to the file. This is only valid as long as the file is open.
    //       Ensure you do not close the file prior to finishing the file usage.


    var skipfirstline = 0 //used to skip first line in file
    var map = collection.mutable.Map("PRINT KEY" -> ArrayBuffer[String](), "FRONT" -> ArrayBuffer[String](), "DEPTH" -> ArrayBuffer[String](), "PROPERTY CLASS" -> ArrayBuffer[String](), "PROP CLASS DESCRIPTION" -> ArrayBuffer[String](), "HOUSE NUMBER" -> ArrayBuffer[String](), "STREET" -> ArrayBuffer[String](), "ADDRESS" -> ArrayBuffer[String](), "CITY" -> ArrayBuffer[String](), "STATE" -> ArrayBuffer[String](), "ZIP CODE (5-DIGIT)" -> ArrayBuffer[String](), "DEED DATE" -> ArrayBuffer[String](), "LAND VALUE" -> ArrayBuffer[String](), "TOTAL VALUE" -> ArrayBuffer[String](), "SALE PRICE" -> ArrayBuffer[String](), "YEAR BUILT" -> ArrayBuffer[String](), "TOTAL LIVING AREA" -> ArrayBuffer[String](), "OVERALL CONDITION" -> ArrayBuffer[String](), "# OF FIREPLACES" -> ArrayBuffer[String](), "# OF BEDS" -> ArrayBuffer[String](), "# OF BATHS" -> ArrayBuffer[String](), "COUNCIL DISTRICT" -> ArrayBuffer[String](), "POLICE DISTRICT" -> ArrayBuffer[String](), "NEIGHBORHOOD" -> ArrayBuffer[String](), "LATITUDE" -> ArrayBuffer[String](), "LONGITUDE" -> ArrayBuffer[String](), "LOCATION" -> ArrayBuffer[String]())
    /*
    var map = collection.mutable.Map(
      "PRINT KEY" -> ArrayBuffer[String](),
      "FRONT" -> ArrayBuffer[String](),
      "DEPTH" -> ArrayBuffer[String](),
      "PROPERTY CLASS" -> ArrayBuffer[String](),
      "PROP CLASS DESCRIPTION" -> ArrayBuffer[String](),
      "HOUSE NUMBER" -> ArrayBuffer[String](),
      "STREET" -> ArrayBuffer[String](),
      "ADDRESS" -> ArrayBuffer[String](),
      "CITY" -> ArrayBuffer[String](),
      "STATE" -> ArrayBuffer[String](),
      "ZIP CODE (5-DIGIT)" -> ArrayBuffer[String](),
      "DEED DATE" -> ArrayBuffer[String](),
      "LAND VALUE" -> ArrayBuffer[String](),
      "TOTAL VALUE" -> ArrayBuffer[String](),
      "SALE PRICE" -> ArrayBuffer[String](),
      "YEAR BUILT" -> ArrayBuffer[String](),
      "TOTAL LIVING AREA" -> ArrayBuffer[String](),
      "OVERALL CONDITION" -> ArrayBuffer[String](),
      "# OF FIREPLACES" -> ArrayBuffer[String](),
      "# OF BEDS" -> ArrayBuffer[String](),
      "# OF BATHS" -> ArrayBuffer[String](),
      "COUNCIL DISTRICT" -> ArrayBuffer[String](),
      "POLICE DISTRICT" -> ArrayBuffer[String](),
      "NEIGHBORHOOD" -> ArrayBuffer[String](),
      "LATITUDE" -> ArrayBuffer[String](),
      "LONGITUDE" -> ArrayBuffer[String](),
      "LOCATION" -> ArrayBuffer[String]())
    */

    val restricted = Array(1,2,8,9,10,11,12,13,14,21,22,23,25,32,33,34,40,41,42)
    val restrictedindex = restricted.map(_.-(1))

    for(line <- inputFile.getLines){ //iterate through each line
      if(skipfirstline ==0 ){ // skipping first line which is header
        skipfirstline = skipfirstline.+(1)
      } else { //it is second line
        var firstcopy = line //copy it to a string
        val first = firstcopy.split(',') // split it as usual by commas


/*
        print("\n")
        val tester = ArrayBuffer[String]()
        for (i <-0 until first.size){
          tester += first(i)
        }
        print(tester)
        print("\n")
        print(tester(7))
*/


        val second = first.map(_.indexOf("\"")).map(_+1) //create another array of ints, each entry represents the index a quotation mark exists in a string, and adds 1 to this num
        val third = ArrayBuffer[String]() // create a final sequence that represents each line comma separated that will be added as values to correspondent keys

        for (i<-0 until second.size ){ //iterating through this array of quotation marks indecees

          if(second(i) > 1 || first(i) == "\"") { //if quotation mark is not at the beginning
/*
            print("first: ")
            print(first(i))
            print("\n")
*/
            var j = i.-(1) //set this to the index right before this entry representing string before it

            do { //search for j ==1 which means quotation mark is at the beginning
              if (second(j) == 1) { //if found
/*
                print("second: ")
                print(first(j))
                print("\n")
*/

                var added = first.slice(j, i.+(1)).mkString(",")
  //              print(added)
                third += added //sums up these strings and adds to the collection representing everything
                j = -1//exist this small loop
              } else {
/*
                print("searching: ")
                print(first(j))
                print("\n")
*/
                j = j.-(1)
                third.trimEnd(1)//go down further
              }
            } while (j != -1) //do all this while j != -1
          }

            else if (second(i) ==0){ //if there is no quotation marks at all, add as usual
              third += first(i) //add to third collection
            } else if(first(i).count(_ == '\"') % 2 == 0){
              third += first(i)
          }
          }


        val thirdsize = third.size //fixing commas at end of line
        for (i<- thirdsize until 46){
          third += ""
        }
      /*  print("\n")
        print(third)
        print("\n")
        print("6: supposed to be VE : ")
        print(third(6))
        print("\n")
        print("7: supposed to be: ")
        print(third(7))
        print("\n")
        */
        val fourth = ArrayBuffer[String]() //cleaned up csv
        for(i<- 0 until third.size) {
          if (restrictedindex.contains(i) == false) {
            fourth += third(i)
          }
        }


          if (fourth(10) != "") { //making sure zipcode exists
            map.getOrElse("PRINT KEY", ArrayBuffer[String]()) += fourth(0)
            map.getOrElse("FRONT", ArrayBuffer[String]()) += fourth(1)
            map.getOrElse("DEPTH", ArrayBuffer[String]()) += fourth(2)
            map.getOrElse("PROPERTY CLASS", ArrayBuffer[String]()) += fourth(3)
            map.getOrElse("PROP CLASS DESCRIPTION", ArrayBuffer[String]()) += fourth(4)
            map.getOrElse("HOUSE NUMBER", ArrayBuffer[String]()) += fourth(5)
            map.getOrElse("STREET", ArrayBuffer[String]()) += fourth(6)
            map.getOrElse("ADDRESS", ArrayBuffer[String]()) += fourth(7)
            map.getOrElse("CITY", ArrayBuffer[String]()) += fourth(8)
            map.getOrElse("STATE", ArrayBuffer[String]()) += fourth(9)
            map.getOrElse("ZIP CODE (5-DIGIT)", ArrayBuffer[String]()) += fourth(10)
            map.getOrElse("DEED DATE", ArrayBuffer[String]()) += fourth(11)
            map.getOrElse("LAND VALUE", ArrayBuffer[String]()) += fourth(12)
            map.getOrElse("TOTAL VALUE", ArrayBuffer[String]()) += fourth(13)
            map.getOrElse("SALE PRICE", ArrayBuffer[String]()) += fourth(14)
            map.getOrElse("YEAR BUILT", ArrayBuffer[String]()) += fourth(15)
            map.getOrElse("TOTAL LIVING AREA", ArrayBuffer[String]()) += fourth(16)
            map.getOrElse("OVERALL CONDITION", ArrayBuffer[String]()) += fourth(17)
            map.getOrElse("# OF FIREPLACES", ArrayBuffer[String]()) += fourth(18)
            map.getOrElse("# OF BEDS", ArrayBuffer[String]()) += fourth(19)
            map.getOrElse("# OF BATHS", ArrayBuffer[String]()) += fourth(20)
            map.getOrElse("COUNCIL DISTRICT", ArrayBuffer[String]()) += fourth(21)
            map.getOrElse("POLICE DISTRICT", ArrayBuffer[String]()) += fourth(22)
            map.getOrElse("NEIGHBORHOOD", ArrayBuffer[String]()) += fourth(23)
            map.getOrElse("LATITUDE", ArrayBuffer[String]()) += fourth(24)
            map.getOrElse("LONGITUDE", ArrayBuffer[String]()) += fourth(25)
            map.getOrElse("LOCATION", ArrayBuffer[String]()) += fourth(26)
          }
          //add fourth's as values to the map
        }
      }
    inputFile.close()



    val outputFile = new BufferedWriter(new FileWriter( new File(filename + "-updated")))
    var headers =
        "PRINT KEY" + "," +
        "FRONT" + "," +
        "DEPTH" + "," +
        "PROPERTY CLASS" + "," +
    "PROP CLASS DESCRIPTION" + "," +
    "HOUSE NUMBER" + "," +
    "STREET" + "," +
    "ADDRESS" + "," +
    "CITY" + "," +
    "STATE" + "," +
    "ZIP CODE (5-DIGIT)" + "," +
    "DEED DATE" + "," +
    "LAND VALUE" + "," +
    "TOTAL VALUE" + "," +
    "SALE PRICE" + "," +
    "YEAR BUILT" + "," +
    "TOTAL LIVING AREA" + "," +
    "OVERALL CONDITION" + "," +
    "# OF FIREPLACES" + "," +
    "# OF BEDS" + "," +
    "# OF BATHS" + "," +
    "COUNCIL DISTRICT" + "," +
    "POLICE DISTRICT" + "," +
    "NEIGHBORHOOD" + "," +
    "LATITUDE" + "," +
    "LONGITUDE" + "," +
    "LOCATION" + "\n"
    outputFile.write(headers)
    var mapsize = map.getOrElse("PRINT KEY", ArrayBuffer[String]()).size
    for (i <- 0 until mapsize){
      var data =
      map.getOrElse("PRINT KEY", ArrayBuffer[String]())(i) + "," +
      map.getOrElse("FRONT", ArrayBuffer[String]())(i) + "," +
      map.getOrElse("DEPTH", ArrayBuffer[String]())(i) +  "," +
      map.getOrElse("PROPERTY CLASS", ArrayBuffer[String]())(i) + "," +
      map.getOrElse("PROP CLASS DESCRIPTION", ArrayBuffer[String]())(i) + "," +
      map.getOrElse("HOUSE NUMBER", ArrayBuffer[String]())(i) + "," +
      map.getOrElse("STREET", ArrayBuffer[String]())(i) + "," +
      map.getOrElse("ADDRESS", ArrayBuffer[String]())(i) + "," +
      map.getOrElse("CITY", ArrayBuffer[String]())(i) + "," +
      map.getOrElse("STATE", ArrayBuffer[String]())(i) + "," +
      map.getOrElse("ZIP CODE (5-DIGIT)", ArrayBuffer[String]())(i) + "," +
      map.getOrElse("DEED DATE", ArrayBuffer[String]())(i) + "," +
      map.getOrElse("LAND VALUE", ArrayBuffer[String]())(i) + "," +
      map.getOrElse("TOTAL VALUE", ArrayBuffer[String]())(i) + "," +
      map.getOrElse("SALE PRICE", ArrayBuffer[String]()) (i) + "," +
      map.getOrElse("YEAR BUILT", ArrayBuffer[String]()) (i) + "," +
      map.getOrElse("TOTAL LIVING AREA", ArrayBuffer[String]()) (i) + "," +
      map.getOrElse("OVERALL CONDITION", ArrayBuffer[String]()) (i) + "," +
      map.getOrElse("# OF FIREPLACES", ArrayBuffer[String]()) (i) + "," +
      map.getOrElse("# OF BEDS", ArrayBuffer[String]()) (i) + "," +
      map.getOrElse("# OF BATHS", ArrayBuffer[String]()) (i) + "," +
      map.getOrElse("COUNCIL DISTRICT", ArrayBuffer[String]()) (i) + "," +
      map.getOrElse("POLICE DISTRICT", ArrayBuffer[String]()) (i) + "," +
      map.getOrElse("NEIGHBORHOOD", ArrayBuffer[String]()) (i) + "," +
      map.getOrElse("LATITUDE", ArrayBuffer[String]()) (i) + "," +
      map.getOrElse("LONGITUDE", ArrayBuffer[String]()) (i) + "," +
      map.getOrElse("LOCATION", ArrayBuffer[String]()) (i) + "\n"

      outputFile.write(data)
    }
    // Without the '\n' character, all output will be written as one long line.
    // Process the lines.


    // Close the files at the end.

    outputFile.close()
  }

  def computeMostExpensiveEntry(filename: String): TaxEntry = {
    val inputFile = scala.io.Source.fromFile(filename)
    // Note: lines is an iterator to the file. This is only valid as long as the file is open.
    //       Ensure you do not close the file prior to finishing the file usage.


    var skipfirstline = 0 //used to skip first line in file
    var map = collection.mutable.Map("PRINT KEY" -> ArrayBuffer[String](), "FRONT" -> ArrayBuffer[String](), "DEPTH" -> ArrayBuffer[String](), "PROPERTY CLASS" -> ArrayBuffer[String](), "PROP CLASS DESCRIPTION" -> ArrayBuffer[String](), "HOUSE NUMBER" -> ArrayBuffer[String](), "STREET" -> ArrayBuffer[String](), "ADDRESS" -> ArrayBuffer[String](), "CITY" -> ArrayBuffer[String](), "STATE" -> ArrayBuffer[String](), "ZIP CODE (5-DIGIT)" -> ArrayBuffer[String](), "DEED DATE" -> ArrayBuffer[String](), "LAND VALUE" -> ArrayBuffer[String](), "TOTAL VALUE" -> ArrayBuffer[String](), "SALE PRICE" -> ArrayBuffer[String](), "YEAR BUILT" -> ArrayBuffer[String](), "TOTAL LIVING AREA" -> ArrayBuffer[String](), "OVERALL CONDITION" -> ArrayBuffer[String](), "# OF FIREPLACES" -> ArrayBuffer[String](), "# OF BEDS" -> ArrayBuffer[String](), "# OF BATHS" -> ArrayBuffer[String](), "COUNCIL DISTRICT" -> ArrayBuffer[String](), "POLICE DISTRICT" -> ArrayBuffer[String](), "NEIGHBORHOOD" -> ArrayBuffer[String](), "LATITUDE" -> ArrayBuffer[String](), "LONGITUDE" -> ArrayBuffer[String](), "LOCATION" -> ArrayBuffer[String]())



    for(line <- inputFile.getLines){ //iterate through each line
      if(skipfirstline ==0 ){ // skipping first line which is header
        skipfirstline = skipfirstline.+(1)
      } else { //it is second line
        var firstcopy = line //copy it to a string
        val first = firstcopy.split(',') // split it as usual by commas
        val second = first.map(_.indexOf("\"")).map(_+1) //create another array of ints, each entry represents the index a quotation mark exists in a string, and adds 1 to this num
        val third = ArrayBuffer[String]() // create a final sequence that represents each line comma separated that will be added as values to correspondent keys

        for (i<-0 until second.size ){ //iterating through this array of quotation marks indecees

          if(second(i) > 1 || first(i) == "\"") { //if quotation mark is not at the beginning
            /*
            print("first: ")
            print(first(i))
            print("\n")
            */
            var j = i.-(1) //set this to the index right before this entry representing string before it

            do { //search for j ==1 which means quotation mark is at the beginning
              if (second(j) == 1) { //if found
                /*
                print("second: ")
                print(first(j))
                print("\n")

                 */
                var added = first.slice(j, i.+(1)).mkString(",")
                // print(added)
                third += added //sums up these strings and adds to the collection representing everything
                j = -1//exist this small loop
              } else {
                /*
                print("searching: ")
                print(first(j))
                print("\n")
                 */
                j = j.-(1)
                third.trimEnd(1)//go down further
              }
            } while (j != -1) //do all this while j != -1
          }

          else if (second(i) ==0){ //if there is no quotation marks at all, add as usual
            third += first(i) //add to third collection
          } else if(first(i).count(_ == '\"') % 2 == 0){
            third += first(i)
          }
        }


        val thirdsize = third.size //fixing commas at end of line
        for (i<- thirdsize until 27){
          third += ""
        }

        map.getOrElse("PRINT KEY", ArrayBuffer[String]()) += third(0)
        map.getOrElse("FRONT", ArrayBuffer[String]()) += third(1)
        map.getOrElse("DEPTH", ArrayBuffer[String]()) += third(2)
        map.getOrElse("PROPERTY CLASS", ArrayBuffer[String]()) += third(3)
        map.getOrElse("PROP CLASS DESCRIPTION", ArrayBuffer[String]()) += third(4)
        map.getOrElse("HOUSE NUMBER", ArrayBuffer[String]()) += third(5)
        map.getOrElse("STREET", ArrayBuffer[String]()) += third(6)
        map.getOrElse("ADDRESS", ArrayBuffer[String]()) += third(7)
        map.getOrElse("CITY", ArrayBuffer[String]()) += third(8)
        map.getOrElse("STATE", ArrayBuffer[String]()) += third(9)
        map.getOrElse("ZIP CODE (5-DIGIT)", ArrayBuffer[String]()) += third(10)
        map.getOrElse("DEED DATE", ArrayBuffer[String]()) += third(11)
        map.getOrElse("LAND VALUE", ArrayBuffer[String]()) += third(12)
        map.getOrElse("TOTAL VALUE", ArrayBuffer[String]()) += third(13)
        map.getOrElse("SALE PRICE", ArrayBuffer[String]()) += third(14)
        map.getOrElse("YEAR BUILT", ArrayBuffer[String]()) += third(15)
        map.getOrElse("TOTAL LIVING AREA", ArrayBuffer[String]()) += third(16)
        map.getOrElse("OVERALL CONDITION", ArrayBuffer[String]()) += third(17)
        map.getOrElse("# OF FIREPLACES", ArrayBuffer[String]()) += third(18)
        map.getOrElse("# OF BEDS", ArrayBuffer[String]()) += third(19)
        map.getOrElse("# OF BATHS", ArrayBuffer[String]()) += third(20)
        map.getOrElse("COUNCIL DISTRICT", ArrayBuffer[String]()) += third(21)
        map.getOrElse("POLICE DISTRICT", ArrayBuffer[String]()) += third(22)
        map.getOrElse("NEIGHBORHOOD", ArrayBuffer[String]()) += third(23)
        map.getOrElse("LATITUDE", ArrayBuffer[String]()) += third(24)
        map.getOrElse("LONGITUDE", ArrayBuffer[String]()) += third(25)
        map.getOrElse("LOCATION", ArrayBuffer[String]()) += third(26)

        //add fourth's as values to the map
      }
    }
    inputFile.close()

    var newone = map.getOrElse("TOTAL VALUE", ArrayBuffer[String]()).map(_.toInt) //getting array of totalvalues but converted to int
    var maximum = newone.max //find max in it
    var word = maximum.toString //convert back to string
    var index = map.getOrElse("TOTAL VALUE", ArrayBuffer[String]()).indexOf(word)
    var mostexpensive = new TaxEntry //create entry
    mostexpensive.infoMap += ("PRINT KEY" -> map.getOrElse("PRINT KEY", ArrayBuffer[String]())(index))
    mostexpensive.infoMap += ("FRONT" -> map.getOrElse("FRONT", ArrayBuffer[String]())(index))
    mostexpensive.infoMap += ("DEPTH" -> map.getOrElse("DEPTH", ArrayBuffer[String]())(index))
    mostexpensive.infoMap += ("PROPERTY CLASS" -> map.getOrElse("PROPERTY CLASS", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("PROP CLASS DESCRIPTION" -> map.getOrElse("PROP CLASS DESCRIPTION", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("HOUSE NUMBER" -> map.getOrElse("HOUSE NUMBER", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("STREET" -> map.getOrElse("STREET", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("ADDRESS" -> map.getOrElse("ADDRESS", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("CITY" -> map.getOrElse("CITY", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("STATE" -> map.getOrElse("STATE", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("ZIP CODE (5-DIGIT)" -> map.getOrElse("ZIP CODE (5-DIGIT)", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("DEED DATE" -> map.getOrElse("DEED DATE", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("LAND VALUE" -> map.getOrElse("LAND VALUE", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("TOTAL VALUE" -> map.getOrElse("TOTAL VALUE", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("SALE PRICE" -> map.getOrElse("SALE PRICE", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("YEAR BUILT" -> map.getOrElse("YEAR BUILT", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("TOTAL LIVING AREA" -> map.getOrElse("TOTAL LIVING AREA", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("OVERALL CONDITION" -> map.getOrElse("OVERALL CONDITION", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("# OF FIREPLACES" -> map.getOrElse("# OF FIREPLACES", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("# OF BEDS" -> map.getOrElse("# OF BEDS", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("# OF BATHS" -> map.getOrElse("# OF BATHS", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("COUNCIL DISTRICT" -> map.getOrElse("COUNCIL DISTRICT", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("POLICE DISTRICT" -> map.getOrElse("POLICE DISTRICT", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("NEIGHBORHOOD" -> map.getOrElse("NEIGHBORHOOD", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("LATITUDE" -> map.getOrElse("LATITUDE", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("LONGITUDE" -> map.getOrElse("LONGITUDE", ArrayBuffer[String]())(index))
      mostexpensive.infoMap += ("LOCATION" -> map.getOrElse("LOCATION", ArrayBuffer[String]())(index))
     mostexpensive
  }


  def computeOldestEntry(filename: String): TaxEntry = {
    val inputFile = scala.io.Source.fromFile(filename)
     // Note: lines is an iterator to the file. This is only valid as long as the file is open.
    //       Ensure you do not close the file prior to finishing the file usage.


    var skipfirstline = 0 //used to skip first line in file
    var map = collection.mutable.Map("PRINT KEY" -> ArrayBuffer[String](), "FRONT" -> ArrayBuffer[String](), "DEPTH" -> ArrayBuffer[String](), "PROPERTY CLASS" -> ArrayBuffer[String](), "PROP CLASS DESCRIPTION" -> ArrayBuffer[String](), "HOUSE NUMBER" -> ArrayBuffer[String](), "STREET" -> ArrayBuffer[String](), "ADDRESS" -> ArrayBuffer[String](), "CITY" -> ArrayBuffer[String](), "STATE" -> ArrayBuffer[String](), "ZIP CODE (5-DIGIT)" -> ArrayBuffer[String](), "DEED DATE" -> ArrayBuffer[String](), "LAND VALUE" -> ArrayBuffer[String](), "TOTAL VALUE" -> ArrayBuffer[String](), "SALE PRICE" -> ArrayBuffer[String](), "YEAR BUILT" -> ArrayBuffer[String](), "TOTAL LIVING AREA" -> ArrayBuffer[String](), "OVERALL CONDITION" -> ArrayBuffer[String](), "# OF FIREPLACES" -> ArrayBuffer[String](), "# OF BEDS" -> ArrayBuffer[String](), "# OF BATHS" -> ArrayBuffer[String](), "COUNCIL DISTRICT" -> ArrayBuffer[String](), "POLICE DISTRICT" -> ArrayBuffer[String](), "NEIGHBORHOOD" -> ArrayBuffer[String](), "LATITUDE" -> ArrayBuffer[String](), "LONGITUDE" -> ArrayBuffer[String](), "LOCATION" -> ArrayBuffer[String]())



    for(line <- inputFile.getLines){ //iterate through each line
      if(skipfirstline ==0 ){ // skipping first line which is header
        skipfirstline = skipfirstline.+(1)
      } else { //it is second line
        var firstcopy = line //copy it to a string
        val first = firstcopy.split(',') // split it as usual by commas
        val second = first.map(_.indexOf("\"")).map(_+1) //create another array of ints, each entry represents the index a quotation mark exists in a string, and adds 1 to this num
        val third = ArrayBuffer[String]() // create a final sequence that represents each line comma separated that will be added as values to correspondent keys

        for (i<-0 until second.size ){ //iterating through this array of quotation marks indecees

          if(second(i) > 1 || first(i) == "\"") { //if quotation mark is not at the beginning
            /*
            print("first: ")
            print(first(i))
            print("\n")
            */
            var j = i.-(1) //set this to the index right before this entry representing string before it

            do { //search for j ==1 which means quotation mark is at the beginning
              if (second(j) == 1) { //if found
                /*
                print("second: ")
                print(first(j))
                print("\n")

                 */
                var added = first.slice(j, i.+(1)).mkString(",")
                // print(added)
                third += added //sums up these strings and adds to the collection representing everything
                j = -1//exist this small loop
              } else {
                /*
                print("searching: ")
                print(first(j))
                print("\n")
                 */
                j = j.-(1)
                third.trimEnd(1)//go down further
              }
            } while (j != -1) //do all this while j != -1
          }

          else if (second(i) ==0){ //if there is no quotation marks at all, add as usual
            third += first(i) //add to third collection
          } else if(first(i).count(_ == '\"') % 2 == 0){
            third += first(i)
          }
        }

        val thirdsize = third.size //fixing commas at end of line
        for (i<- thirdsize until 27){
          third += ""
        }

        map.getOrElse("PRINT KEY", ArrayBuffer[String]()) += third(0)
        map.getOrElse("FRONT", ArrayBuffer[String]()) += third(1)
        map.getOrElse("DEPTH", ArrayBuffer[String]()) += third(2)
        map.getOrElse("PROPERTY CLASS", ArrayBuffer[String]()) += third(3)
        map.getOrElse("PROP CLASS DESCRIPTION", ArrayBuffer[String]()) += third(4)
        map.getOrElse("HOUSE NUMBER", ArrayBuffer[String]()) += third(5)
        map.getOrElse("STREET", ArrayBuffer[String]()) += third(6)
        map.getOrElse("ADDRESS", ArrayBuffer[String]()) += third(7)
        map.getOrElse("CITY", ArrayBuffer[String]()) += third(8)
        map.getOrElse("STATE", ArrayBuffer[String]()) += third(9)
        map.getOrElse("ZIP CODE (5-DIGIT)", ArrayBuffer[String]()) += third(10)
        map.getOrElse("DEED DATE", ArrayBuffer[String]()) += third(11)
        map.getOrElse("LAND VALUE", ArrayBuffer[String]()) += third(12)
        map.getOrElse("TOTAL VALUE", ArrayBuffer[String]()) += third(13)
        map.getOrElse("SALE PRICE", ArrayBuffer[String]()) += third(14)
        map.getOrElse("YEAR BUILT", ArrayBuffer[String]()) += third(15)
        map.getOrElse("TOTAL LIVING AREA", ArrayBuffer[String]()) += third(16)
        map.getOrElse("OVERALL CONDITION", ArrayBuffer[String]()) += third(17)
        map.getOrElse("# OF FIREPLACES", ArrayBuffer[String]()) += third(18)
        map.getOrElse("# OF BEDS", ArrayBuffer[String]()) += third(19)
        map.getOrElse("# OF BATHS", ArrayBuffer[String]()) += third(20)
        map.getOrElse("COUNCIL DISTRICT", ArrayBuffer[String]()) += third(21)
        map.getOrElse("POLICE DISTRICT", ArrayBuffer[String]()) += third(22)
        map.getOrElse("NEIGHBORHOOD", ArrayBuffer[String]()) += third(23)
        map.getOrElse("LATITUDE", ArrayBuffer[String]()) += third(24)
        map.getOrElse("LONGITUDE", ArrayBuffer[String]()) += third(25)
        map.getOrElse("LOCATION", ArrayBuffer[String]()) += third(26)

        //add fourth's as values to the map
      }
    }
    inputFile.close()

    var newone = map.getOrElse("YEAR BUILT", ArrayBuffer[String]()) //getting array of yearbuilt but converted to int
   // print(newone)
    var fixed = ArrayBuffer[Int]()
    for(i <- 0 until newone.size){
      if(newone(i) != "") {
        if (newone(i).toInt > 1000) {
          fixed += newone(i).toInt
        }
      }
    }
    var minimum = fixed.min //find min in it
    var word = minimum.toString //convert to string
    var OldestEntry = new TaxEntry //create entry
    var index = map.getOrElse("YEAR BUILT", ArrayBuffer[String]()).indexOf(word)
    var Oldest = new TaxEntry //create entry
    Oldest.infoMap += ("PRINT KEY" -> map.getOrElse("PRINT KEY", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("FRONT" -> map.getOrElse("FRONT", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("DEPTH" -> map.getOrElse("DEPTH", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("PROPERTY CLASS" -> map.getOrElse("PROPERTY CLASS", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("PROP CLASS DESCRIPTION" -> map.getOrElse("PROP CLASS DESCRIPTION", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("HOUSE NUMBER" -> map.getOrElse("HOUSE NUMBER", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("STREET" -> map.getOrElse("STREET", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("ADDRESS" -> map.getOrElse("ADDRESS", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("CITY" -> map.getOrElse("CITY", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("STATE" -> map.getOrElse("STATE", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("ZIP CODE (5-DIGIT)" -> map.getOrElse("ZIP CODE (5-DIGIT)", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("DEED DATE" -> map.getOrElse("DEED DATE", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("LAND VALUE" -> map.getOrElse("LAND VALUE", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("TOTAL VALUE" -> map.getOrElse("TOTAL VALUE", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("SALE PRICE" -> map.getOrElse("SALE PRICE", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("YEAR BUILT" -> map.getOrElse("YEAR BUILT", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("TOTAL LIVING AREA" -> map.getOrElse("TOTAL LIVING AREA", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("OVERALL CONDITION" -> map.getOrElse("OVERALL CONDITION", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("# OF FIREPLACES" -> map.getOrElse("# OF FIREPLACES", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("# OF BEDS" -> map.getOrElse("# OF BEDS", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("# OF BATHS" -> map.getOrElse("# OF BATHS", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("COUNCIL DISTRICT" -> map.getOrElse("COUNCIL DISTRICT", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("POLICE DISTRICT" -> map.getOrElse("POLICE DISTRICT", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("NEIGHBORHOOD" -> map.getOrElse("NEIGHBORHOOD", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("LATITUDE" -> map.getOrElse("LATITUDE", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("LONGITUDE" -> map.getOrElse("LONGITUDE", ArrayBuffer[String]())(index))
    Oldest.infoMap += ("LOCATION" -> map.getOrElse("LOCATION", ArrayBuffer[String]())(index))
    Oldest
  }
}
