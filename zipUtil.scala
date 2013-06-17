import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.InputStream
import java.io.IOException
import java.io.OutputStream
import java.net.URI
import java.util.zip.ZipEntry
import java.util.zip.ZipFile
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

  def copyStream(in: InputStream, out: OutputStream): Unit = {
    var buffer = new Array[Byte](1024)
    var len = 1
    while ({len = in.read(buffer); len > 0}) {
      out.write(buffer, 0, len)
    }
  }

  def createZip(filePaths: List[String], outputFilename: String, parentPath: String) = {
    try {
      println("Zipping...")
      val fileOutputStream = new FileOutputStream(outputFilename)
      val zipOutputStream = new ZipOutputStream(fileOutputStream)

      filePaths.foreach((name: String) => { 
        println("adding " + name)
        val zipEntry = addFileToZipEntry(name, parentPath, filePaths.size)
        zipOutputStream.putNextEntry(zipEntry)
        val inputStream = new FileInputStream(name)
        copyStream(inputStream, zipOutputStream)
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
  
  def unzip(file: File): Unit = {
    val basename = file.getName.substring(0, file.getName.lastIndexOf("."))
    val todir = new File(file.getParentFile, basename)
    todir.mkdirs

    val zip = new ZipFile(file)
    val entries = zip.entries
    while(entries.hasMoreElements){
      val entry = entries.nextElement

      /**
      // check if entry is in a subdirectory
      val path = entry.getName.split(File.separator)
      if (path.size >= 2) {
        val subdir = path(path.size-2)
        
      }
      */

      val entryPath = {
        if (entry.getName.startsWith(basename)) entry.getName.substring(basename.length) 
        else entry.getName
      }

      // create output directory if it doesn't exist already
      if (entry.getName.split(File.separator).size >= 2) {
        val entryRelativePath = entry.getName.substring(0,entry.getName.lastIndexOf(File.separator))
        val entryDirPath = todir.getAbsolutePath + File.separator + entryRelativePath
        val entryOutputDir = new File(entryDirPath)
        if (!entryOutputDir.exists) entryOutputDir.mkdir
      }

      println("Extracting to " + todir + File.separator + entryPath)
      val istream = zip.getInputStream(entry)
      val ostream = new FileOutputStream(new File(todir, entryPath))
      copyStream(istream, ostream)
      ostream.close
      istream.close
    }
  }

  def main(args: Array[String]) = {
    if (args.length == 1 && args(0).split('.').last == "zip") {
      unzip(new File(args(0)))
    } else if (args.length == 2) { 
      val path = args(0)
      val outputFilename = args(1)
      val file = new File(path)
      val filePaths = createFileList(file, outputFilename)
      createZip(filePaths, outputFilename, path)
      println("Finished!")
    }
    else throw new IllegalArgumentException
  }
}
