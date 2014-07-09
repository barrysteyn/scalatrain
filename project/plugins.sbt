addSbtPlugin("com.typesafe.sbt" % "sbt-scalariform" % "1.3.0")

// Actually IDE specific settings belong into ~/.sbt/,
// but in order to ease the setup for the training we put the following here:

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.4.0")
