package problems

import java.time.Instant

trait TimingHelpers {
  def time[A](f: => A): Long = {
    val before = Instant.now
    f
    val after = Instant.now

    println("Finished executing...")
    after.toEpochMilli - before.toEpochMilli
  }
}
