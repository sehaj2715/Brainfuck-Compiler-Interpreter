object M5a {

type Mem = Map[Int, Int]

import io.Source
import scala.util._

def load_bff(name: String) : String = {
  val source = Source.fromFile(name)
  val content = source.mkString
  source.close()
  content
}

def sread(mem: Mem, mp: Int) : Int = {
    mem.getOrElse(mp, 0)
  }

def write(mem: Mem, mp: Int, v: Int) : Mem = {
    mem.updated(mp, v)
  }

def jumpRight(prog: String, pc: Int, level: Int) : Int ={
    var currentLevel = level
    var currentPC = pc

    while (currentPC < prog.length && (prog(currentPC) != ']' || currentLevel > 0)) {
      if (prog(currentPC) == '[') {
        currentLevel += 1
      } else if (prog(currentPC) == ']') {
        currentLevel -= 1
      }
      currentPC += 1
    }

    currentPC + 1 
  }

def jumpLeft(prog: String, pc: Int, level: Int) : Int = {
    var currentLevel = level
    var currentPC = pc

    while (currentPC >= 0 && (prog(currentPC) != '[' || currentLevel > 0)) {
      if (prog(currentPC) == ']') {
        currentLevel += 1
      } else if (prog(currentPC) == '[') {
        currentLevel -= 1
      }
      currentPC -= 1
    }

    currentPC + 1
  }


// testcases
// M5a.jumpRight("""--[..+>--],>,++""", 3, 0)         // => 10
// M5a.jumpLeft("""--[..+>--],>,++""", 8, 0)          // => 3
// M5a.jumpRight("""--[..[+>]--],>,++""", 3, 0)       // => 12
// M5a.jumpRight("""--[..[[-]+>[.]]--],>,++""", 3, 0) // => 18
// M5a.jumpRight("""--[..[[-]+>[.]]--,>,++""", 3, 0)  // => 22 (outside)
// M5a.jumpLeft("""[******]***""", 7, 0)              // => -1 (outside)



def compute(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = {
    if (pc < 0 || pc >= prog.length) {
      mem
    } else {
      prog(pc) match {
        case '>' => compute(prog, pc + 1, mp + 1, mem)
        case '<' => compute(prog, pc + 1, mp - 1, mem)
        case '+' => compute(prog, pc + 1, mp, write(mem, mp, sread(mem, mp) + 1))
        case '-' => compute(prog, pc + 1, mp, write(mem, mp, sread(mem, mp) - 1))
        case '.' => {
          print(sread(mem, mp).toChar)
          compute(prog, pc + 1, mp, mem)
        }
        case ',' => {
          compute(prog, pc + 1, mp, mem)
        }
        case '[' => {
          if (sread(mem, mp) == 0) {
            val newPC = jumpRight(prog, pc + 1, 0)
            compute(prog, newPC, mp, mem)
          } else {
            compute(prog, pc + 1, mp, mem)
          }
        }
        case ']' => {
          if (sread(mem, mp) != 0) {
            val newPC = jumpLeft(prog, pc - 1, 0)
            compute(prog, newPC, mp, mem)
          } else {
            compute(prog, pc + 1, mp, mem)
          }
        }
        case _ => compute(prog, pc + 1, mp, mem)
      }
    }
  }

def run(prog: String, m: Mem = Map()) = {
    compute(prog, 0, 0, m)
  }

def generate(msg: List[Char]) : String = {
  msg.map(c => s"${"+" * c.toInt} .[-]").mkString
}

// clears the 0-cell
//run("[-]", Map(0 -> 100))    // Map will be 0 -> 0

// moves content of the 0-cell to 1-cell
//run("[->+<]", Map(0 -> 10))  // Map will be 0 -> 0, 1 -> 10

// copies content of the 0-cell to 2-cell and 4-cell
//run("[>>+>>+<<<<-]", Map(0 -> 42))    // Map(0 -> 0, 2 -> 42, 4 -> 42)

// prints out numbers 0 to 9
//run("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")

// hello world program 1
// run("""++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++
//       ..+++.>>.<-.<.+++.------.--------.>>+.>++.""")

// hello world program 2
//run("""++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>+
//       +.<<+++++++++++++++.>.+++.------.--------.>+.>.""")

// hello world program 3
//run("""+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..
//       +++.>-.------------.<++++++++.--------.+++.------.--------.>+.""")
 
// draws the Sierpinski triangle
//run(load_bff("sierpinski.bf"))

//outputs the square numbers up to 10000
// run("""++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+
//       >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]
//       <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]""")

// calculates 2 to the power of 6 
//run(""">>[-]>[-]++>[-]++++++><<<>>>>[-]+><>[-]<<[-]>[>+<<+>-]>[<+>-]
//       <><[-]>[-]<<<[>>+>+<<<-]>>>[<<<+>>>-][-]><<>>[-]>[-]<<<[>>[-]
//       <[>+>+<<-]>[<+>-]+>[[-]<-<->>]<<<-]>>[<<+>>-]<<[[-]>[-]<<[>+>
//       +<<-]>>[<<+>>-][-]>[-]<<<<<[>>>>+>+<<<<<-]>>>>>[<<<<<+>>>>>-]
//       <<>>[-]>[-]<<<[>>>+<<<-]>>>[<<[<+>>+<-]>[<+>-]>-]<<<>[-]<<[-]
//       >[>+<<+>-]>[<+>-]<><[-]>[-]<<<[>>+>+<<<-]>>>-[<<<+>>>-]<[-]>[-]
//       <<<[>>+>+<<<-]>>>[<<<+>>>-][-]><<>>[-]>[-]<<<[>>[-]<[>+>+<<-]>
//       [<+>-]+>[[-]<-<->>]<<<-]>>[<<+>>-]<<][-]>[-]<<[>+>+<<-]>>[<<+>
//       >-]<<<<<[-]>>>>[<<<<+>>>>-]<<<<><>[-]<<[-]>[>+<<+>-]>[<+>-]<>
//       <[-]>[-]>[-]<<<[>>+>+<<<-]>>>[<<<+>>>-]<<>>[-]>[-]>[-]>[-]>[-]>
//       [-]>[-]>[-]>[-]>[-]<<<<<<<<<<>>++++++++++<<[->+>-[>+>>]>[+[-<+
//       >]>+>>]<<<<<<]>>[-]>>>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<
//       ]>[-]>>[>++++++[-<++++++++>]<.<<+>+>[-]]<[<[->-<]++++++[->++++
//       ++++<]>.[-]]<<++++++[-<++++++++>]<.[-]<<[-<+>]<<><<<""")

// a Mandelbrot set generator in brainf*** written by Erik Bosman
//run(load_bff("mandelbrot.bf"))

// a benchmark program (counts down from 'Z' to 'A')
//run(load_bff("benchmark.bf"))

// calculates the Collatz series for numbers from 1 to 30
//run(load_bff("collatz.bf"))

}