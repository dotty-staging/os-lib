import mill._, scalalib._, publish._

object os extends Cross[OsModule]("2.12.7", "2.13.0", "0.16.0-RC3")
class OsModule(val crossScalaVersion: String) extends CrossScalaModule with PublishModule{
  def artifactName = "os-lib"
  def publishVersion = "0.3.0"
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

  // def compileIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.2.0")
  // def scalacOptions = Seq("-P:acyclic:force")
  // def scalacPluginIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.2.0")

  def ivyDeps = Agg(ivy"com.lihaoyi:geny_2.12:0.1.8")

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.1",
      ivy"com.lihaoyi::sourcecode::0.1.7"
    )

    def testFrameworks = Seq("utest.runner.Framework")
  }
}
