def foo():Unit = println ("hello")
def bar:Unit = println ("hello")

Array.fill(10){scala.util.Random.nextInt(5)}

//def nextInt(n: Int): Int = self.nextInt(n)

val i = 2
def f: Int = i + 1
def f1(): Int = i + 2
val f2 = () => i + 3

Array.fill(10)(f2)

def listOfDuplicates[A](x: A, length: Int): List[A] = {
  if (length < 1)
    Nil
  else
    x :: listOfDuplicates(x, length - 1)
}

println(listOfDuplicates(3, 4))