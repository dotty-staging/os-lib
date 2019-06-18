package os

import scala.language.implicitConversions

import java.io._
import java.nio.charset.{Charset, StandardCharsets}
import java.util.concurrent.TimeUnit

import scala.annotation.tailrec

/**
  * Represents a spawn subprocess that has started and may or may not have
  * completed.
  */
class SubProcess(val wrapped: java.lang.Process,
                 val inputPumperThread: Option[Thread],
                 val outputPumperThread: Option[Thread],
                 val errorPumperThread: Option[Thread]) extends java.lang.AutoCloseable{
  val stdin: SubProcess.InputStream = new SubProcess.InputStream(wrapped.getOutputStream)
  val stdout: SubProcess.OutputStream = new SubProcess.OutputStream(wrapped.getInputStream)
  val stderr: SubProcess.OutputStream = new SubProcess.OutputStream(wrapped.getErrorStream)

  /**
    * The subprocess' exit code. Conventionally, 0 exit code represents a
    * successful termination, and non-zero exit code indicates a failure.
    *
    * Throws an exception if the subprocess has not terminated
    */
  def exitCode(): Int = wrapped.exitValue()

  /**
    * Returns `true` if the subprocess is still running and has not terminated
    */
  def isAlive(): Boolean = wrapped.isAlive

  /**
    * Attempt to destroy the subprocess (gently), via the underlying JVM APIs
    */
  def destroy(): Unit = wrapped.destroy()

  /**
    * Force-destroys the subprocess, via the underlying JVM APIs
    */
  def destroyForcibly(): Unit = wrapped.destroyForcibly()

  /**
    * Alias for [[destroy]]
    */
  def close() = wrapped.destroy()

  /**
    * Wait up to `millis` for the subprocess to terminate, by default waits
    * indefinitely. Returns `true` if the subprocess has terminated by the time
    * this method returns
    */
  def waitFor(millis: Long = -1): Boolean =
    if(millis == -1) {
      wrapped.waitFor()
      true
    } else {
      wrapped.waitFor(millis, TimeUnit.MILLISECONDS)
    }

}


object SubProcess{

  /**
    * A [[BufferedWriter]] with the underlying [[java.io.OutputStream]] exposed
    *
    * Note that all writes that occur through this class are thread-safe and
    * synchronized. If you wish to perform writes without the synchronization
    * overhead, you can use the underlying [[wrapped]] stream directly
    */
  class InputStream(val wrapped: java.io.OutputStream)
    extends java.io.OutputStream with java.io.DataOutput{

    private[this] val data = new DataOutputStream(wrapped)

    override def write(b: Int) = synchronized{ wrapped.write(b) }
    override def write(b: Array[Byte]): Unit = synchronized{ wrapped.write(b) }
    override def write(b: Array[Byte], offset: Int, len: Int): Unit = synchronized{
      wrapped.write(b, offset, len)
    }


    def write(s: String,
              charSet: Charset = StandardCharsets.UTF_8): Unit = synchronized{
      val writer = new OutputStreamWriter(wrapped, charSet)
      writer.write(s)
      writer.flush()
    }
    def writeLine(s: String,
                  charSet: Charset = StandardCharsets.UTF_8): Unit = synchronized{
      val writer = new OutputStreamWriter(wrapped, charSet)
      writer.write(s)
      writer.write("\n")
      writer.flush()
    }

    override def flush() = synchronized{ wrapped.flush() }
    override def close() = synchronized{ wrapped.close() }

    override def writeBoolean(v: Boolean) = synchronized{ data.writeBoolean(v) }
    override def writeByte(v: Int) = synchronized{ data.writeByte(v) }
    override def writeShort(v: Int) = synchronized{ data.writeShort(v) }
    override def writeChar(v: Int) = synchronized{ data.writeChar(v) }
    override def writeInt(v: Int) = synchronized{ data.writeInt(v) }
    override def writeLong(v: Long) = synchronized{ data.writeLong(v) }
    override def writeFloat(v: Float) = synchronized{ data.writeFloat(v) }
    override def writeDouble(v: Double) = synchronized{ data.writeDouble(v) }
    override def writeBytes(s: String) = synchronized{ data.writeBytes(s) }
    override def writeChars(s: String) = synchronized{ data.writeChars(s) }
    override def writeUTF(s: String) = synchronized{ data.writeUTF(s) }
  }

  /**
    * A combination [[BufferedReader]] and [[java.io.InputStream]], this allows
    * you to read both bytes and lines, without worrying about the buffer used
    * for reading lines messing up your reading of bytes.
    *
    * Note that all reads that occur through this class are thread-safe and
    * synchronized. If you wish to perform writes without the synchronization
    * overhead, you can use the underlying [[wrapped]] stream directly
    */
  class OutputStream(val wrapped: java.io.InputStream,
                     bufferSize: Int = 8192)
    extends java.io.InputStream with java.io.DataInput with StreamValue {

    private[this] val data = new DataInputStream(wrapped)
    // We maintain our own buffer internally, in order to make readLine
    // efficient by avoiding reading character by character. As a consequence
    // all the other read methods have to check the buffer for data and using
    // that before going and reading from the wrapped input stream.
    private[this] var bufferOffset = 0
    private[this] var bufferEnd = 0
    private[this] val buffer = new Array[Byte](bufferSize)
    // Keep track if the last readLine() call terminated on a \r, since that
    // means any subsequence readLine() that starts with a \n should ignore the
    // leading character
    private[this] var lastSeenSlashR = false

    /**
      * Read all bytes from this pipe from the subprocess, blocking until it is
      * complete, and returning it as a byte array
      */
    def bytes: Array[Byte] = synchronized{
      val out = new ByteArrayOutputStream()
      Internals.transfer(this, out)
      out.toByteArray
    }

    override def read() = synchronized{
      lastSeenSlashR = false
      if (bufferOffset < bufferEnd){
        val res = buffer(bufferOffset)
        bufferOffset += 1
        res
      }else{
        wrapped.read()
      }
    }

    override def read(b: Array[Byte]) = synchronized{
      lastSeenSlashR = false
      this.read(b, 0, b.length)
    }
    override def read(b: Array[Byte], offset: Int, len: Int) = synchronized{
      lastSeenSlashR = false
      val bufferedCount = bufferEnd - bufferOffset
      if (bufferedCount > len){
        bufferOffset += len
        System.arraycopy(buffer, bufferEnd, b, offset, len)
        len
      }else{
        bufferOffset = bufferEnd
        System.arraycopy(buffer, bufferEnd, b, offset, bufferedCount)
        wrapped.read(b, bufferedCount, len - bufferedCount) + bufferedCount
      }
    }

    /**
      * Read a single line from the stream, as a string. A line is ended by \n,
      * \r or \r\n. The returned string does *not* return the trailing
      * delimiter.
      */
    def readLine(charSet: Charset = StandardCharsets.UTF_8): String = synchronized{
      val output = new ByteArrayOutputStream()
      // Reads the buffer for a newline, returning the index of the newline
      // (if any). Returns the length of the buffer if no newline is found
      @tailrec def recChar(i: Int, skipFirst: Boolean): (Int, Boolean) = {
        if (i == bufferEnd) (i, skipFirst) // terminate tailrec
        else if (buffer(i) == '\n') {
          if (lastSeenSlashR) {
            lastSeenSlashR = false
            recChar(i + 1, true)
          } else (i, skipFirst)

        } else if (buffer(i) == '\r'){
          lastSeenSlashR = true
          (i, skipFirst)
        } else{
          lastSeenSlashR = false
          recChar(i + 1, skipFirst)
        }
      }

      // Reads what's currently in the buffer trying to find a newline. If no
      // newline is found in the whole buffer, load another batch of bytes into
      // the buffer from the input stream and try again
      @tailrec def recBuffer(didSomething: Boolean): Boolean = {
        val (newLineIndex, skipFirst) = recChar(bufferOffset, false)
        val skipOffset = if (skipFirst) 1 else 0
        if (newLineIndex < bufferEnd) { // Found a newline
          output.write(buffer, bufferOffset + skipOffset, newLineIndex - bufferOffset - skipOffset)
          bufferOffset = newLineIndex + 1
          true
        } else if (newLineIndex == bufferEnd){

          val start = bufferOffset + skipOffset
          val end = newLineIndex - bufferOffset - skipOffset
          output.write(buffer, start, end)
          val readCount = wrapped.read(buffer, 0, buffer.length)
          if (readCount != -1){ // End of buffer
            bufferOffset = 0
            bufferEnd = readCount
            recBuffer(didSomething || end > start)
          }else{ // End of input
            val didSomething2 = didSomething || newLineIndex != (bufferOffset + skipOffset)
            bufferOffset = 0
            bufferEnd = 0
            didSomething2
          }
        } else ???
      }

      val didSomething = recBuffer(false)

      if (didSomething) output.toString(charSet.name())
      else null
    }

    override def close() = synchronized{ wrapped.close() }

    override def readFully(b: Array[Byte], off: Int, len: Int) = synchronized{
      var n = 0
      while({
        if (len == n) false
        else wrapped.read(b, off + n, len - n) match{
          case -1 => throw new EOFException(s"Insufficient bytes, expected: $len, read: $n")
          case d =>
            n += d
            true
        }
      })()
    }
    override def readFully(b: Array[Byte]) = readFully(b, 0, b.length)
    override def skipBytes(n: Int) = synchronized{ data.skipBytes(n) }
    override def readBoolean() = synchronized{ data.readBoolean() }
    override def readByte() = synchronized{ data.readByte() }
    override def readUnsignedByte() = synchronized{ data.readUnsignedByte() }
    override def readShort() = synchronized{ data.readShort() }
    override def readUnsignedShort() = synchronized{ data.readUnsignedShort() }
    override def readChar() = synchronized{ data.readChar() }
    override def readInt() = synchronized{ data.readInt() }
    override def readLong() = synchronized{ data.readLong() }
    override def readFloat() = synchronized{ data.readFloat() }
    override def readDouble() = synchronized{ data.readDouble() }
    override def readUTF() = synchronized{ data.readUTF() }

    override def readLine() = this.readLine(StandardCharsets.UTF_8)
  }
}

/**
  * Represents the configuration of a SubProcess's input stream. Can either be
  * [[os.Inherit]], [[os.Pipe]], [[os.Path]] or a [[os.Source]]
  */
trait ProcessInput{
  def redirectFrom: ProcessBuilder.Redirect
  def processInput(stdin: => SubProcess.InputStream): Option[Runnable]
}
object ProcessInput{
  implicit def makeSourceInput[T](r: T)(implicit f: T => Source): ProcessInput = SourceInput(f(r))
  implicit def makePathRedirect(p: Path): ProcessInput = PathRedirect(p)
  case class SourceInput(r: Source) extends ProcessInput {
    def redirectFrom = ProcessBuilder.Redirect.PIPE

    def processInput(stdin: => SubProcess.InputStream): Option[Runnable] = Some{
      new Runnable{def run() = {
        os.Internals.transfer(r.getInputStream(), stdin)
        stdin.close()
      }}
    }
  }
}

/**
  * Represents the configuration of a SubProcess's output or error stream. Can
  * either be [[os.Inherit]], [[os.Pipe]], [[os.Path]] or a [[os.ProcessOutput]]
  */
trait ProcessOutput{
  def redirectTo: ProcessBuilder.Redirect
  def processOutput(out: => SubProcess.OutputStream): Option[Runnable]
}
object ProcessOutput{
  implicit def makePathRedirect(p: Path): ProcessOutput = PathRedirect(p)

  def apply(f: (Array[Byte], Int) => Unit, preReadCallback: () => Unit = () => ()) =
    CallbackOutput(f, preReadCallback)

  case class CallbackOutput(f: (Array[Byte], Int) => Unit, preReadCallback: () => Unit){
    def redirectTo = ProcessBuilder.Redirect.PIPE
    def processOutput(stdin: => SubProcess.OutputStream) = Some{
      new Runnable {def run(): Unit = os.Internals.transfer0(stdin, preReadCallback, f)}
    }
  }
}

/**
  * Inherit the input/output stream from the current process
  */
object Inherit extends ProcessInput with ProcessOutput {
  def redirectTo = ProcessBuilder.Redirect.INHERIT
  def redirectFrom = ProcessBuilder.Redirect.INHERIT
  def processInput(stdin: => SubProcess.InputStream) = None
  def processOutput(stdin: => SubProcess.OutputStream) = None
}

/**
  * Pipe the input/output stream to the current process to be used via
  * `java.lang.Process#{getInputStream,getOutputStream,getErrorStream}`
  */
object Pipe extends ProcessInput with ProcessOutput {
  def redirectTo = ProcessBuilder.Redirect.PIPE
  def redirectFrom = ProcessBuilder.Redirect.PIPE
  def processInput(stdin: => SubProcess.InputStream) = None
  def processOutput(stdin: => SubProcess.OutputStream) = None
}

case class PathRedirect(p: Path) extends ProcessInput with ProcessOutput{
  def redirectFrom = ProcessBuilder.Redirect.from(p.toIO)
  def processInput(stdin: => SubProcess.InputStream) = None
  def redirectTo = ProcessBuilder.Redirect.to(p.toIO)
  def processOutput(out: => SubProcess.OutputStream) = None
}
case class PathAppendRedirect(p: Path) extends ProcessOutput{
  def redirectTo = ProcessBuilder.Redirect.appendTo(p.toIO)
  def processOutput(out: => SubProcess.OutputStream) = None
}
