/* This file is part of GauProS-Sandbox.
 *
 * GauProS is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GauProS is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GauProS.  If not, see <https://www.gnu.org/licenses/>.
 */

enablePlugins(ScalaJSPlugin)

import org.scalajs.core.tools.sem.Semantics.RuntimeClassNameMapper

name := "GauProS_Sandbox"
version := "0.1.0"
licenses := Seq("GPLv3" -> url("http://www.gnu.org/licenses/gpl-3.0.html"))

scalaVersion := "2.12.8"
scalaJSUseMainModuleInitializer := true

scalaJSStage  in Global := FullOptStage
scalacOptions in Global ++= Seq("-feature", "-deprecation")

scalaJSSemantics ~= { _.withStrictFloats(false) }
scalaJSSemantics ~= (_ withRuntimeClassNameMapper RuntimeClassNameMapper.discardAll() )

lazy val gauprosSandbox = Project( "GauProS_Sandbox", file(".") ) dependsOn gauprosJS
lazy val gauprosJS = ProjectRef( file("../GauProS"), "gauprosJS" )

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6"
libraryDependencies += "com.lihaoyi"  %%% "utest"       % "0.6.6" % "test"
testFrameworks += new TestFramework("utest.runner.Framework")