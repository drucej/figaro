import sbt._
import Keys._
import sbt.Package.ManifestAttributes
import com.typesafe.sbteclipse.core.EclipsePlugin._
import sbtassembly.Plugin._
import AssemblyKeys._
import scoverage.ScoverageSbtPlugin._

  name := "figaro-root"

  lazy val figaroSettings = Seq(
    organization := "com.cra.figaro",
    description := "Figaro: a language for probablistic programming",
    version := "4.2.0.0",
    scalaVersion := "2.11.7",
    crossPaths := true,
    publishMavenStyle := true,
    retrieveManaged := true,
	excludeFilter := "ParticleBeliefPropagation.scala" || "SufficientStatisticsFactor.scala"||"Gibbs.scala"||"BeliefPropagation.scala"||"MarginalMAPBeliefPropagation.scala"||"CollapsedGibbs.scala"||"CollapsedProbQueryGibbs.scala"||"GibbsSolver.scala"||"BPSolver.scala"||"GeneralizedEM.scala"||"ProbEvidenceBeliefPropagation.scala"||"MPEBeliefPropagation.scala"||"InnerBPHandler.scala"||"MPEVariableElimination.scala"||"FactoredFrontier.scala"||"SufficientStatisticsVariableElimination.scala"||"Collapsers.scala"||"FlatGibbs.scala"||"FlatBP.scala"||"StructuredMPEBP.scala"||"StructuredMPEVE.scala"||"VEBPGibbsStrategy.scala"||"VEBPStrategy.scala"||"VEGibbsStrategy.scala"||"MarginalMAPVEStrategy.scala"||"StructuredGibbs.scala"||"StructuredBP.scala"||"StructuredVEBPGibbsChooser.scala"||"StructuredVEBPChooser.scala"||"StructuredVEGibbsChooser.scala"||"StructuredMarginalMAPVE.scala"||"ResultsGUI.scala"||"BlockSampler.scala"||"Distribution.scala"||"Histogram.scala"||"ResultsTable.scala",

    pomExtra :=
	<url>http://www.github.com/p2t2/figaro</url>
	<developers>
	  <developer>
	    <name>Avrom J. Pfeffer</name>
	    <email>apfeffer@cra.com</email>
	    <organization>Charles River Analytics, Inc.</organization>
	    <organizationUrl>http://www.cra.com</organizationUrl>
	  </developer>
	</developers>
	<licenses>
	  <license>
	    <name>Figaro License</name>
	    <url>https://github.com/p2t2/figaro/blob/master/LICENSE</url>
	  </license>
	</licenses>
	<scm>
	  <connection>scm:git:git@github.com:p2t2/figaro.git</connection>
	  <developerConnection>scm:git:git@github.com:p2t2/figaro.git</developerConnection>
	  <url>git@github.com:p2t2/figaro.git</url>
	</scm>
  )


  lazy val scalaMajorMinor = "2.11"
  

  // Read exisiting Figaro MANIFEST.MF from file
  lazy val figaroManifest = Using.fileInputStream(file("Figaro/META-INF/MANIFEST.MF")) { 
    in => new java.util.jar.Manifest(in)
  }

  // Read exisiting FigaroExamples MANIFEST.MF from file
  //lazy val examplesManifest = Using.fileInputStream(file("FigaroExamples/META-INF/MANIFEST.MF")) {
  //  in => new java.util.jar.Manifest(in)
  //}

  lazy val root = Project("root", file("."))
    .settings(figaroSettings)
    .settings(publishLocal := {})
    .settings(publish := {})
//    .dependsOn(figaro, examples)
//    .aggregate(figaro, examples)

  lazy val figaro = Project("Figaro", file("Figaro"))
    .settings(figaroSettings)
    .settings (scalacOptions ++= Seq(
	"-feature",
	"-language:existentials",
	"-deprecation",
	"-language:postfixOps"
    ))
    .settings(packageOptions := Seq(Package.JarManifest(figaroManifest)))
    .settings(libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "asm" % "asm" % "3.3.1",
      "org.apache.commons" % "commons-math3" % "3.3",
      "net.sf.jsci" % "jsci" % "1.2",
      "com.typesafe.akka" %% "akka-actor" % "2.3.14",
      "org.scalanlp" %% "breeze" % "0.10",
      "io.argonaut" %% "argonaut" % "6.0.4",
      "org.prefuse" % "prefuse" % "beta-20071021",
      "org.scala-lang.modules" %% "scala-swing" % "1.0.1",
      "com.storm-enroute" %% "scalameter" % "0.7" % "provided",
      "org.scalatest" %% "scalatest" % "2.2.4" % "provided, test"
    ))
    // Enable forking
    .settings(fork := true)
    // Increase max memory for JVM for both testing and runtime
    .settings(javaOptions in (Test,run) += "-Xmx6G")
    // test settings
//    .settings(parallelExecution in Test := false)
//    .settings(testOptions in Test += Tests.Argument("-oD"))
//    .configs(detTest)
//    .settings(inConfig(detTest)(Defaults.testTasks): _*)
//    .settings(testOptions in detTest := Seq(Tests.Argument("-l", "com.cra.figaro.test.nonDeterministic")))
//    .configs(nonDetTest)
//    .settings(inConfig(nonDetTest)(Defaults.testTasks): _*)
//    .settings(testOptions in nonDetTest := Seq(Tests.Argument("-n", "com.cra.figaro.test.nonDeterministic")))
    // sbt-assembly settings
    .settings(assemblySettings: _*)
    .settings(test in assembly := {})
    .settings(jarName in assembly := "figaro_" + scalaMajorMinor + "-" + version.value + "-fat.jar")
    .settings(assemblyOption in assembly ~= { _.copy(includeScala = false) })
    .settings(excludedJars in assembly := {
	val cp = (fullClasspath in assembly).value
	cp filter {_.data.getName == "arpack_combined_all-0.1-javadoc.jar"}
    })
    // ScalaMeter settings
    .settings(testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"))
    .settings(logBuffered := false)
    // SBTEclipse settings
    .settings(EclipseKeys.eclipseOutput := Some("target/scala-2.11/classes"))
      
//  lazy val examples = Project("FigaroExamples", file("FigaroExamples"))
//    .dependsOn(figaro)
//    .settings(figaroSettings)
//    .settings(packageOptions := Seq(Package.JarManifest(examplesManifest)))
    // SBTEclipse settings
//    .settings(EclipseKeys.eclipseOutput := Some("target/scala-2.11/classes"))
    // Copy all managed dependencies to \lib_managed directory
//    .settings(retrieveManaged := true)

//  lazy val detTest = config("det") extend(Test)
//  lazy val nonDetTest = config("nonDet") extend(Test)
