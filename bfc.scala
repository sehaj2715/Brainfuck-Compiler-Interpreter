object M5b {

// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


// DEBUGGING INFORMATION FOR COMPILERS!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. One way to debug them is to run
// example programs ``unoptimised''; and then optimised. Does
// the optimised version still produce the same result?


// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}


type Mem = Map[Int, Int]

import io.Source
import scala.util._
// ADD YOUR CODE BELOW
//======================
// (6) 
def jtable(prog: String): Map[Int, Int] = {
  def processInstruction(pc: Int, mp: Int, jumpTable: Map[Int, Int]): Map[Int, Int] = {
    if (pc < 0 || pc >= prog.length) {
      jumpTable
    } else {
      prog(pc) match {
        case '>' => processInstruction(pc + 1, mp + 1, jumpTable + (pc -> (pc + 1)))
        case '<' => processInstruction(pc + 1, mp - 1, jumpTable + (pc -> (pc + 1)))
        case '+' => processInstruction(pc + 1, mp, jumpTable + (pc -> (pc + 1)))
        case '-' => processInstruction(pc + 1, mp, jumpTable + (pc -> (pc + 1)))
        case '.' => processInstruction(pc + 1, mp, jumpTable + (pc -> (pc + 1)))
        case '[' => {
          val newPC = M5a.jumpRight(prog, pc, 0)
          processInstruction(newPC, mp, jumpTable + (pc -> newPC))
        }
        case ']' => {
          val newPC = M5a.jumpLeft(prog, pc, 0)
          processInstruction(newPC, mp, jumpTable + (pc -> newPC))
        }
        case _ => processInstruction(pc + 1, mp, jumpTable)
      }
    }
  }

  processInstruction(0, 0, Map())
}


// testcase
//
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]"")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)


def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
    if (pc < 0 || pc >= pg.length) {
      mem
    } else {
      val newPC = tb.getOrElse(pc, pc) // Lookup in the jump table
      pg(pc) match {
        case '>' => compute2(pg, tb, newPC, mp + 1, mem)
        case '<' => compute2(pg, tb, newPC, mp - 1, mem)
        case '+' => compute2(pg, tb, newPC, mp, M5a.write(mem, mp, M5a.sread(mem, mp) + 1))
        case '-' => compute2(pg, tb, newPC, mp, M5a.write(mem, mp, M5a.sread(mem, mp) - 1))
        case '.' => {
          print(M5a.sread(mem, mp).toChar)
          compute2(pg, tb, newPC, mp, mem)
        }
        case ',' => compute2(pg, tb, newPC, mp, mem) // Assume read from input is not implemented
        case '[' => {
          if (M5a.sread(mem, mp) == 0) {
            compute2(pg, tb, tb.getOrElse(pc, pc), mp, mem)
          } else {
            compute2(pg, tb, newPC, mp, mem)
          }
        }
        case ']' => {
          if (M5a.sread(mem, mp) != 0) {
            compute2(pg, tb, tb.getOrElse(pc, pc), mp, mem)
          } else {
            compute2(pg, tb, newPC, mp, mem)
          }
        }
        case _ => compute2(pg, tb, newPC, mp, mem) // Ignore other characters
      }
    }
  }

def run2(pg: String, m: Mem = Map()) = {
    val tb = jtable(pg)
    compute2(pg, tb, 0, 0, m)
  }

// testcases
// time_needed(1, run2(load_bff("benchmark.bf")))
// time_needed(1, run2(load_bff("sierpinski.bf")))



def optimise(s: String): String = {
  
  // Remove dead code (non-BF commands)
  val cleanedProg = s.replaceAll("[^<>+\\-.\\\\[\\\\]]", "")
  
  // Replace [-] with 0
  val optimizedProg = cleanedProg.replaceAll("\\[-\\]", "0")
  optimizedProg
}

def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
    if (pc < 0 || pc >= pg.length) {
      mem
    } else {
      val newPC = tb.getOrElse(pc, pc) // Lookup in the jump table
      pg(pc) match {
        case '>' => compute3(pg, tb, newPC, mp + 1, mem)
        case '<' => compute3(pg, tb, newPC, mp - 1, mem)
        case '+' => compute3(pg, tb, newPC, mp, M5a.write(mem, mp, M5a.sread(mem, mp) + 1))
        case '-' => compute3(pg, tb, newPC, mp, M5a.write(mem, mp, M5a.sread(mem, mp) - 1))
        case '.' => {
          print(M5a.sread(mem, mp).toChar)
          compute3(pg, tb, newPC, mp, mem)
        }
        case ',' => compute3(pg, tb, newPC, mp, mem)
        case '[' => {
          if (M5a.sread(mem, mp) == 0) {
            compute3(pg, tb, M5a.jumpRight(pg, pc, 0), mp, mem)
          } else {
            compute3(pg, tb, newPC, mp, mem)
          }
        }
        case ']' => {
          if (M5a.sread(mem, mp) != 0) {
            compute3(pg, tb, M5a.jumpLeft(pg, pc, 0), mp, mem)
          } else {
            compute3(pg, tb, newPC, mp, mem)
          }
        }
        case '0' => compute3(pg, tb, newPC, mp, M5a.write(mem, mp, 0))
        case _ => compute3(pg, tb, newPC, mp, mem)
      }
    }
  }

def run3(pg: String, m: Mem = Map()) ={
    val jumpTable = M5b.jtable(pg)
    val optimizedProg = optimise(pg)
    compute3(optimizedProg, jumpTable, 0, 0, m)
  }

// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11205
// 
// time_needed(1, run3(load_bff("benchmark.bf")))



// (8)  
def combine(s: String) : String = {
    // Replace sequences of repeated increment and decrement commands by appropriate two-character commands
    s.foldLeft(("", 1, s.head, 0)) {
      case ((result, count, current, pc), c) =>
        if (c == current) {
          if (count == 25) {
            // Replace sequence with two-character command
            (result + s"$current{25}", 1, c, pc + 1)
          } else {
            (result, count + 1, c, pc + 1)
          }
        } else {
          // Append current character and reset count
          (result + (if (count > 1) s"$current$count" else current.toString), 1, c, pc + 1)
        }
    }._1
  }


// testcase
// combine(load_bff("benchmark.bf"))

def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
    if (pc < 0 || pc >= pg.length) {
      mem
    } else {
      val newPC = tb.getOrElse(pc, pc)
      val currentCmd = pg.substring(pc, pc + 2) // Get the current two-character command

      currentCmd match {
        case ">" => compute4(pg, tb, newPC + 2, mp + 1, mem) // Increase pc by 2 for two-character commands
        case "<" => compute4(pg, tb, newPC + 2, mp - 1, mem)
        case _ if currentCmd.matches("[+\\-]") =>
          // Handle combined increment and decrement commands
          val amount = currentCmd.last.toString.toInt
          compute4(pg, tb, newPC + 2, mp, M5a.write(mem, mp, M5a.sread(mem, mp) + (if (currentCmd.head == '+') amount else -amount)))
        case "." => {
          print(M5a.sread(mem, mp).toChar)
          compute4(pg, tb, newPC + 2, mp, mem)
        }
        case "," => compute4(pg, tb, newPC + 2, mp, mem) // Assume read from input is not implemented
        case "[" =>
          if (M5a.sread(mem, mp) == 0) {
            compute4(pg, tb, tb.getOrElse(pc, pc), mp, mem)
          } else {
            compute4(pg, tb, newPC + 2, mp, mem)
          }
        case "]" =>
          if (M5a.sread(mem, mp) != 0) {
            compute4(pg, tb, tb.getOrElse(pc, pc), mp, mem)
          } else {
            compute4(pg, tb, newPC + 2, mp, mem)
          }
        case _ => compute4(pg, tb, newPC + 2, mp, mem) // Ignore other characters
      }
    }
  }


// should call first optimise and then combine on the input string

def run4(pg: String, m: Mem = Map()) = {
    val jumpTable = M5b.jtable(pg)
    val optimizedProg = M5b.optimise(pg)
    val combinedProg = combine(optimizedProg)
    compute4(combinedProg, jumpTable, 0, 0, m)
  }


// testcases
// combine(optimise(load_bff("benchmark.bf"))) // => "">A+B[<A+M>A-A]<A[[.....""

// testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf")))
// time_needed(1, run4(load_bff("mandelbrot.bf")))


}