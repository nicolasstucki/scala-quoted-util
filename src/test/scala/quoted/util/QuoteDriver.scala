// QUICK FIX
// TODO remove this file in 0.7
// Copy of fix in https://github.com/lampepfl/dotty/pull/3871

package scala.quoted
package util

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._
import dotty.tools.io.{AbstractFile, Directory, PlainDirectory, VirtualDirectory}
import dotty.tools.repl.AbstractFileClassLoader

import scala.quoted.Expr
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets

class QuoteDriver extends Driver {

  def run[T](expr: Expr[T], settings: Runners.RunSettings): T = {
    val ctx: Context = initCtx.fresh
    ctx.settings.optimise.update(settings.optimise)(ctx)

    val outDir: AbstractFile = settings.outDir match {
      case Some(out) =>
        val dir = Directory(out)
        dir.createDirectory()
        new PlainDirectory(Directory(out))
      case None =>
        new VirtualDirectory("(memory)", None)
    }

    val driver = new dotty.tools.dotc.quoted.ExprCompiler(outDir)
    driver.newRun(ctx).compileExpr(expr)

    val classLoader = new AbstractFileClassLoader(outDir, this.getClass.getClassLoader)

    val clazz = classLoader.loadClass(driver.outputClassName.toString)
    val method = clazz.getMethod("apply")
    val instance = clazz.newInstance()

    method.invoke(instance).asInstanceOf[T]
  }

  def show(expr: Expr[_]): String = {
    val ctx: Context = initCtx.fresh
    ctx.settings.color.update("never")(ctx) // TODO support colored show
    val baos = new ByteArrayOutputStream
    var ps: PrintStream = null
    try {
      ps = new PrintStream(baos, true, "utf-8")

      new dotty.tools.dotc.quoted.ExprDecompiler(ps).newRun(ctx).compileExpr(expr)

      new String(baos.toByteArray, StandardCharsets.UTF_8)
    }
    finally if (ps != null) ps.close()
  }

  override def initCtx: Context = {
    val ictx = super.initCtx.fresh
    var classpath = System.getProperty("java.class.path")
    this.getClass.getClassLoader match {
      case cl: URLClassLoader =>
        classpath = cl.getURLs.map(_.getFile()).mkString("", ":", if (classpath == "") "" else ":" + classpath)
      case _ =>
    }
    ictx.settings.classpath.update(classpath)(ictx)
    ictx
  }

}
