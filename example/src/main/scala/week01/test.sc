
trait Task[A] {
  def join: A
}

implicit def applyJoin[A](task: Task[A]): A = task.join

def task[A](block: => A): Task[A]


object example {
  val t1 = task {
    1 + 1
  }

  val t2 = task {
    2 + 2
  }

  val result = (t1 + t2) / 2
}
