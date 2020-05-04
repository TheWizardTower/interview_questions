package concordance

// import cats._, cats.implicits._
import fs2._
// import fs2.io._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
// import java.nio.file.Paths

@SuppressWarnings(Array("org.wartremover.warts.Any"))
class ConcordanceSpec extends AnyFlatSpec with Matchers {
  import concordance.Concordance._
    val generalStream = Stream.emits("I am the model of the modern major general".getBytes)
    val doomStream = Stream.emits("Doot Doot Doot".getBytes)
    "businessLogic" should "Get the most frequently used word." in {
      generalStream.through(businessLogic(1)) shouldEqual Vector((2, Set("the")))
    }
    "businessLogic" should "Handle requesting more words than are present gracefully." in {
      doomStream.through(businessLogic(2)) shouldEqual Vector((3, Set("Doot")))
    }
  // Stream.resource(Blocker[IO]).flatMap { blocker =>
  //   val bibleStream = io.file.readAll[IO](Paths.get("../kjv.txt"), blocker, 4096)
  // }
}
