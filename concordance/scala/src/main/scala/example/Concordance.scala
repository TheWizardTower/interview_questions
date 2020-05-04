package concordance

import cats.Show
import cats.implicits._
import cats.effect._ // , cats.effect.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import fs2._
import java.nio.file.Path

object Concordance extends CommandIOApp(
  name = "scala-concordance",
  header = "Solution to the concordance problem, written in Scala, using cats, fs2, and decline.",
  version = "0.0.1"
) {
  implicit val showResult: Show[Vector[(Int, Set[String])]] = new Show[Vector[(Int, Set[String])]] {
    @SuppressWarnings(Array("org.wartremover.warts.Any"))
    def show(t: Vector[(Int, Set[String])]): String =
      t.map{ case (count: Int, wordSet: Set[String]) =>
        wordSet
          .toVector
          .sorted
          .map{ (word: String) => f"$count%7d $word\n" }.foldRight("")(_ |+| _)
      }
      .foldRight("")(_ |+| _)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  val bytesToTrimmedWords: Pipe[IO, Byte, String] = {
    _
      .through(text.utf8Decode)
      .map(s => s.map(c => if(Character.isWhitespace(c)) '\n' else c))
      .through(text.lines)
      .map(s => s.trim)
      .filter(s => !s.isEmpty)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  val toSortedResult: Pipe[IO, Map[String, Int], Vector[(Int, Set[String])]] = {
    _
      .map{
        (f: Map[String, Int]) => f.foldRight(Map.empty[Int, Set[String]]){
          case ((word: String, count: Int), acc: Map[Int, Set[String]]) => acc |+| Map(count->Set(word))
        }
      .toVector
      .sortWith(_._1 > _._1) // high to low
      }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def businessLogic(count: Int): Pipe[IO, Byte, Vector[(Int, Set[String])]] = {
    _
      .through(bytesToTrimmedWords)
      .foldMap{ word: String => Map(word->1)}
      .through(toSortedResult)
      .map{_.take(count)}
  }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def main: Opts[IO[ExitCode]] = {
    val filenameOpt = Opts.option[Path]("filename", short = "f", help = "Filename to read in and process. If blank, read from stdin.").orNone
    val wordcountOpt = Opts.option[Int]("number", short = "n", help = "Number of words to print to the screen. Defaults to 10.").withDefault(10)
    val options = (filenameOpt, wordcountOpt)
    options.mapN { (filename: Option[Path], count: Int) =>
      val stream = for {
       blocker <- Stream.resource(Blocker[IO])
       bufferSize = 4096
       _ <- filename.fold(io.stdin[IO](bufferSize, blocker)){path =>  io.file.readAll[IO](path, blocker, bufferSize) }
         .through(businessLogic(count))
         .through(io.stdoutLines(blocker))
      } yield ()
      stream
        .compile.drain.as(ExitCode.Success)
    }
  }

}
