package de.sciss.anemone

object Main extends App {
  def argErr(): Nothing = {
    Console.err.println(
      """Must choose a program:
        |
        |  convolve
        |  resample
        |  video
        |  stills
        |
        |""".stripMargin)
    sys.exit(1)
  }

  val program = args.headOption.getOrElse(argErr()) match {
    case "convolve" => Convolve        .main _
    case "resample" => Resample        .main _
    case "video"    => Video           .main _
    case "stills"   => MakeStills_xCoAx.main _
    case _          => argErr()
  }

  program(args.tail)
}
