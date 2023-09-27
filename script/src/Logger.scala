import java.io.{FileInputStream, PrintWriter, StringWriter}
import java.nio.file.Paths

object WhereAmI:
  final class WhereAmI extends Exception("Where am I?")

  inline def whereAmI: String = whereAmI(new WhereAmI, None)

  inline def whereAmI(maxLines: Int): String = whereAmI(new WhereAmI, Some(maxLines))

  inline def whereAmI(throwable: Throwable): String = whereAmI(throwable, None)

  inline def whereAmI(throwable: Throwable, maxLines: Int): String = whereAmI(throwable, Some(maxLines))

  private def whereAmI(throwable: Throwable, maxLines: Option[Int]): String =
    val result = new StringWriter
    throwable.printStackTrace(new PrintWriter(result))
    maxLines match
      case None    => result.toString
      case Some(n) => result.toString.split('\n').take(n).mkString("\n")

private class Logger[T] private (tag: String, maxLines: Int):
  import java.util.logging.{Logger as JLogger, *}

  private val logger: JLogger = {
    LogManager.getLogManager.readConfiguration(new FileInputStream(Paths.get("logging.properties").toFile))
    JLogger.getLogger("LOG")
  }

  def log(level: Level, str: String): Unit =
    str.split("\n").take(maxLines).foreach { line => logger.log(level, s"$tag: $line") }

object Logger:
  import java.util.logging.Level

  private var instance: Option[Logger[_]] = None

  private var logLevel: Level = Level.ALL

  def setLogLevel(level: Level): Unit =
    logLevel = level

  def setSilent(): Unit = setLogLevel(Level.OFF)
  def isSilent: Boolean =
    logLevel == Level.OFF
  def setLoud(): Unit = setLogLevel(Level.ALL)

  def init[T](tag: String = "", maxLines: Int = 15): Unit =
    instance = Some(new Logger(tag, maxLines))

  def log(level: Level, str: String): Unit =
    if level.intValue >= logLevel.intValue then
      if instance.isEmpty then init()
      instance.foreach(_.log(level, str))

  def error(str: String): Unit = log(Level.SEVERE, str)
  def warn(str: String): Unit = log(Level.WARNING, str)
  def info(str: String): Unit = log(Level.INFO, str)

  def whereAmI(): Unit = info(WhereAmI.whereAmI)

  def error(t: Throwable): Unit =
    error(t.getMessage)
    error(WhereAmI.whereAmI(t))

