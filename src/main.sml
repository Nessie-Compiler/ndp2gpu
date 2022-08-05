structure Main =
  struct

    val parse = Parser.parseFile

    val toFKL = Flatten.xform o parse

  end

