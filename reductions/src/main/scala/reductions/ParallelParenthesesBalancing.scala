package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {

    val until = chars.length

    @tailrec
    def recur(from: Int, diff: Int): Boolean = {
      if (from >= until) diff == 0
      else if (chars(from) == '(') recur(from + 1, diff + 1)
      else if (chars(from) == ')' && diff > 0) recur(from + 1, diff - 1)
      else if (chars(from) == ')' && diff <= 0) false
      else recur(from + 1, diff)
    }

    recur(from = 0, diff = 0)

  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    @tailrec
    def traverse(idx: Int, until: Int, minDepth: Int, total: Int): (Int, Int) = {
      if(idx >= until) (minDepth, total)
      else {
        val cTotal = if (chars(idx) == ')') total - 1 else if (chars(idx) == '(') total + 1 else total
        val cDepth = Math.min(minDepth, cTotal)
        traverse(idx + 1, until, cDepth, cTotal)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if(until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + (until - from) / 2

        val ((lDepth, lTotal), (rDepth, rTotal)) = parallel(reduce(from, mid), reduce(mid, until))

        (Math.min(lDepth, lTotal + rDepth), lTotal + rTotal)

      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
