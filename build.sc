import mill._, scalalib._, publish._

object TestUtil {
  val dottyVersion = Option(sys.props("dottyVersion"))
}

trait OsLibModule extends CrossScalaModule with PublishModule{
  def publishVersion = "0.6.3"
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "com.lihaoyi",
    url = "https://github.com/lihaoyi/os",
    licenses = Seq(License.MIT),
    scm = SCM(
      "git://github.com/lihaoyi/os.git",
      "scm:git://github.com/lihaoyi/os.git"
    ),
    developers = Seq(
      Developer("lihaoyi", "Li Haoyi","https://github.com/lihaoyi")
    )
  )

  def compileIvyDeps =
    if (crossScalaVersion.startsWith("2")) Agg(ivy"com.lihaoyi::acyclic:0.2.0")
    else Agg.empty[mill.scalalib.Dep]

  def scalacOptions =
    if (crossScalaVersion.startsWith("2")) Seq("-P:acyclic:force")
    else Nil

  def scalacPluginIvyDeps =
    if (crossScalaVersion.startsWith("2")) Agg(ivy"com.lihaoyi::acyclic:0.2.0")
    else Agg.empty[mill.scalalib.Dep]

  trait OsLibTestModule extends Tests{
    val sourcecodeVersion =
      if (crossScalaVersion.startsWith("2")) "0.1.7"
      else "0.1.8"

    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.3",
      ivy"com.lihaoyi::sourcecode::0.2.0"
    )

    def testFrameworks = Seq("utest.runner.Framework")
  }
}
object os extends Cross[OsModule]((List("2.12.7", "2.13.0") ++ TestUtil.dottyVersion): _*){
  object watch extends Cross[WatchModule]((List("2.12.7", "2.13.0") ++ TestUtil.dottyVersion): _*)
  class WatchModule(val crossScalaVersion: String) extends OsLibModule{
    def artifactName = "os-lib-watch"
    def moduleDeps = Seq(os())
    def ivyDeps = Agg(
      ivy"net.java.dev.jna:jna:5.0.0"
    )


    object test extends OsLibTestModule {
      def moduleDeps = super.moduleDeps ++ Seq(os().test)
    }
  }

}
class OsModule(val crossScalaVersion: String) extends OsLibModule{
  def artifactName = "os-lib"

  def ivyDeps =
    if (crossScalaVersion.startsWith("2"))
      Agg(ivy"com.lihaoyi::geny:0.5.0")
    else
      Agg(ivy"com.lihaoyi:geny_2.13:0.5.0")

  object test extends OsLibTestModule
}
