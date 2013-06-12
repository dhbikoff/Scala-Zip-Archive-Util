import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.IOException
import java.net.URI
import java.util.zip.ZipEntry
import java.util.zip.ZipOutputStream

object zipUtil {

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
      case _ => throw new IOException("Bad path. No file or directory found.")
    }
  }

  def addFileToZipEntry(filename: String, parentPath: String, filePathsCount: Int): ZipEntry = {
    if (filePathsCount <= 1) new ZipEntry(new File(filename).getName)
    // use relative path to avoid adding absolute path directories
    else new ZipEntry(new URI(parentPath).relativize(new URI(filename)).toString)
  }

  def createZip(filePaths: List[String], outputFilename: String, parentPath: String) = {
    try {
      println("Zipping...")
      val fileOutputStream = new FileOutputStream(outputFilename)
      val zipOutputStream = new ZipOutputStream(fileOutputStream)

      var buffer = new Array[Byte](1024)
      filePaths.foreach((name: String) => { 
        println("adding " + name)
        val zipEntry = addFileToZipEntry(name, parentPath, filePaths.size)
        zipOutputStream.putNextEntry(zipEntry)
        val inputStream = new FileInputStream(name)

        // read file data into the input stream, then write it
        // to the zip output stream
        var len = 1
        while ({var len = inputStream.read(buffer); len > 0}) {
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
    if (args.length != 2) throw new IllegalArgumentException(
      "Wrong number of arguments. Usage: <source> <dest>")
    val path = args(0)
    val outputFilename = args(1)
    val file = new File(path)
    val filePaths = createFileList(file, outputFilename)
    createZip(filePaths, outputFilename, path)
    println("Finished!")
  }
}
