import scala.collection.JavaConversions._
import scala.io._
import java.io.{File, FileInputStream, FileOutputStream, InputStream, IOException, OutputStream}
import java.net.URI
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

object ZipArchiveUtil {

  def createFileList(file: File, outputFilename: String): List[String] = {
    file match {
      case file if file.isFile => {
        if (file.getName != outputFilename) List(file.getAbsoluteFile.toString)
        else List()
      }
      case file if file.isDirectory => {
        file.list.foldLeft(List[String]()) ((pathList: List[String], path: String) =>
          pathList ++ createFileList(new File(file, path), outputFilename))
      }
      case _ => throw new IOException("Bad path. No file or directory found.")
    }
  }

  def addFileToZipEntry(filename: String, parentPath: String, filePathsCount: Int): ZipEntry = {
    if (filePathsCount <= 1) new ZipEntry(new File(filename).getName)
    // use relative path to avoid adding absolute path directories
    else {
      val relative = new File(parentPath).toURI.relativize(new File(filename).toURI).getPath
      println(relative)
      new ZipEntry(relative)
    }
  }

  def createZip(filePaths: List[String], outputFilename: String, parentPath: String) = {
    try {
      val fileOutputStream = new FileOutputStream(outputFilename)
      val zipOutputStream = new ZipOutputStream(fileOutputStream)

      filePaths.foreach((name: String) => { 
        println("adding " + name)
        val zipEntry = addFileToZipEntry(name, parentPath, filePaths.size)
        zipOutputStream.putNextEntry(zipEntry)
        val inputSrc = new BufferedSource(new FileInputStream(name))(Codec.ISO8859)
        inputSrc foreach { c: Char => zipOutputStream.write(c) }
        inputSrc.close
      })
    
    zipOutputStream.closeEntry
    zipOutputStream.close
    fileOutputStream.close

    } catch {
      case e: IOException => { 
        e.printStackTrace
      }
    }
  }
  
  def unzip(file: File): Unit = {
    println("Unzipping = " + file.getName)
    val basename = file.getName.substring(0, file.getName.lastIndexOf("."))
    val todir = new File(file.getParentFile, basename)
    todir.mkdirs

    val zip = new ZipFile(file)
    zip.entries foreach { entry =>
      println("PATH " + entryPath)
      val entryPath = {
        if (entry.getName.startsWith(basename)) entry.getName.substring(basename.length) 
        else entry.getName
      }
      // create output directory if it doesn't exist already
      val splitPath = entry.getName.split(File.separator)
      if (splitPath.size >= 2) {
        val entryRelativePath = entry.getName.substring(0,entry.getName.lastIndexOf(File.separator))
        val entryDirPath = todir.getAbsolutePath + File.separator + entryRelativePath
        val entryOutputDir = new File(entryDirPath)
        if (!entryOutputDir.exists) entryOutputDir.mkdir
      }
      println("Extracting to " + todir + File.separator + entryPath)
      val inputSrc = new BufferedSource(zip.getInputStream(entry))(Codec.ISO8859)
      val ostream = new FileOutputStream(new File(todir, entryPath))
      inputSrc foreach { c: Char => ostream.write(c) }
      inputSrc.close
      ostream.close
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
    else throw new IllegalArgumentException("\nZip Usage: <source> <dest>" + "\n" + "Unzip Usage: <file.zip>")
  }
}
