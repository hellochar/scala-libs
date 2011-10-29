package org.zhang

import java.io.{PrintWriter, FileWriter}

/**
 * Created by IntelliJ IDEA.
 * User: hellochar
 * Date: 7/29/11
 * Time: 1:13 PM
 */

package object io {
  /**
   * Used for reading/writing to database, files, etc.
   * Code From the book "Beginning Scala"
   * http://www.amazon.com/Beginning-Scala-David-Pollak/dp/1430219890
   */
  private def using[A <: {def close() : Unit}, B](param: A)(f: A => B): B =
    try {
      f(param)
    } finally {
      param.close()
    }

  /**
   * Write the given data to the filename. Creates the file if it doesn't exist. Overwrites anything else that was previously there.
   */
  def writeToFile(fileName: String, data: String) =
    using(new FileWriter(fileName)) {
      fileWriter => fileWriter.write(data)
    }

  def appendToFile(fileName: String, textData: String) =
    using(new FileWriter(fileName, true)) {
      fileWriter => using(new PrintWriter(fileWriter)) {
        printWriter => printWriter.println(textData)
      }
    }
}