package nl.hugo.redbook.ch5

import org.scalatest._

class Test5_13 extends WordSpec with Matchers {
  "A Stream" should {
    "return its mapped values" in {
      Stream(1, 2, 3, 4).mapViaUnfold(_.toString).toList should be(List("1", "2", "3", "4"))
    }
  }

  "An Empty" should {
    "return Empty when mapped" in {
      Empty.mapViaUnfold(_.toString) should be(Empty)
    }
  }

  "A Stream with four elements" should {
    "return Empty with no elements takeViaUnfoldn" in {
      Stream(1, 2, 3, 4).takeViaUnfold(0) should be(Empty)
    }

    "return two Elements with two elements takeViaUnfoldn" in {
      Stream(1, 2, 3, 4).takeViaUnfold(2).toList should be(List(1, 2))
    }

    "return four Elements with two elements takeViaUnfoldn" in {
      Stream(1, 2, 3, 4).takeViaUnfold(4).toList should be(List(1, 2, 3, 4))
    }

    "return four Elements with five elements takeViaUnfoldn" in {
      Stream(1, 2, 3, 4).takeViaUnfold(5).toList should be(List(1, 2, 3, 4))
    }
  }

  "An Empty" should {
    "return Empty with no elements takeViaUnfoldn" in {
      Empty.takeViaUnfold(0) should be(Empty)
    }

    "return Empty with one element takeViaUnfoldn" in {
      Empty.takeViaUnfold(1) should be(Empty)
    }
  }

  "A Stream with four elements" should {
    "take the elements matching the predicate" in {
      Stream(1, 1, 2, 2).takeWhileViaUnfold(_ == 1).toList should be(List(1, 1))
    }

    "return an empty list when no elements match the predicate" in {
      Stream(2, 2, 2, 2).takeWhileViaUnfold(_ == 1).toList should be(Nil)
    }

    "return all elements if they all match the predicate" in {
      Stream(1, 1, 1, 1).takeWhileViaUnfold(_ == 1).toList should be(List(1, 1, 1, 1))
    }

    "return all elements upto mismatching element, not the other matching elements" in {
      Stream(1, 1, 2, 1).takeWhileViaUnfold(_ == 1).toList should be(List(1, 1))
    }
  }

  "An Empty" should {
    "return an Empty" in {
      Empty.takeWhileViaUnfold(_ => true) should be(Empty)
    }
  }

  "A Stream" should {
    "zip a stream with another steam" in {
      Stream(1, 10, 100).zipWith(Stream(2, 3, 4))(_ + _).toList should be(List(3, 13, 104))
    }

    "zip a stream with and empty" in {
      Stream(1, 1, 1).zipWith(Empty)((_, _) => 2).toList should be(Nil)
    }

    "zip a stream with an shorter stream" in {
      Stream(1, 10, 100).zipWith(Stream(2, 3))(_ + _).toList should be(List(3, 13))
    }

    "zip a stream with a longer stream" in {
      Stream(1, 10).zipWith(Stream(200, 300, 400))(_ + _).toList should be(List(201, 310))
    }
  }

  "An Empty" should {
    "zip with a stream" in {
      Empty.zipWith(Stream(1))((_, _) => 2).toList should be(Nil)
    }

    "zip with an Empty" in {
      Empty.zipWith(Empty)((_, _) => 2).toList should be(Nil)
    }
  }

  "A Stream" should {
    "zip a stream with another steam" in {
      Stream(1, 2, 3).zipAll(Stream(4, 5, 6)).toList should be(List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6))))
    }

    "zip a stream with and empty" in {
      Stream(1, 2, 3).zipAll(Empty).toList should be(List((Some(1), None), (Some(2), None), (Some(3), None)))

    }

    "zip a stream with an shorter stream" in {
      Stream(1, 2, 3).zipAll(Stream(4, 5)).toList should be(List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), None)))
    }

    "zip a stream with a longer stream" in {
      Stream(1, 2).zipAll(Stream(4, 5, 6)).toList should be(List((Some(1), Some(4)), (Some(2), Some(5)), (None, Some(6))))
    }
  }

  "An Empty" should {
    "zip with a stream" in {
      Empty.zipAll(Stream(1)).toList should be(List((None, Some(1))))
    }

    "zip with an Empty" in {
      Empty.zipAll(Empty).toList should be(Nil)
    }
  }
}
