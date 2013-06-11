import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.IOException
import java.util.zip.ZipEntry
import java.util.zip.ZipOutputStream

object zip {
  def createFileList(file: File, outputFilename: String): List[String] = {
    file match {
      case file if file.isFile => {
        if (file.getName != outputFilename) List(file.getAbsoluteFile.toString)
        else List()
      }
      case file if file.isDirectory => {
        file.list.toList.foldLeft(List[String]()) ((pathList: List[String], path: String) =>
          pathList ++ createFileList(new File(file, path), outputFilename))
      }
      case _ => {
        println("Invalid Path")
        sys.exit(2)
      }
    }
  }
 
  def createZip(filesList: List[String], outputFilename: String) = {
    try {
      println("Zipping...")
      val fileOutputStream = new FileOutputStream(outputFilename)
      val zipOutputStream = new ZipOutputStream(fileOutputStream)

      var buffer = new Array[Byte](1024)
      filesList.foreach((name: String) => { 
        println("adding " + name)
        val zipEntry = new ZipEntry(name)
        zipOutputStream.putNextEntry(zipEntry)
        val inputStream = new FileInputStream(name)

        var len = 1
        while (len > 0) {
          len = inputStream.read(buffer)
          if (len > 0)
          zipOutputStream.write(buffer, 0, len)
        }
    
        inputStream.close()
      })

    zipOutputStream.closeEntry()
    zipOutputStream.close()

    } catch {
      case e: IOException => { 
        e.printStackTrace
      }
    }
  }
  
  def main(args: Array[String]) = {
    if (args.length != 2) { 
      println("Wrong number of args. <source> <output>")
      sys.exit(1)
    }
    val path = args(0)
    val outputFilename = args(1)
    val file = new File(path)
    val filesList = createFileList(file, outputFilename)
    createZip(filesList, outputFilename)
    println("Finished!")
  }
}
