

package com.fola.kapitel2
object myApp extends App {
  
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    def go(a: A) = f(g(a))
    
    go
  }
  
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    def go(a: A, b: B) = {
      f(a)(b)
    }
    
    go
  }
  
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    def go(a: A) = {
      b: B => f(a, b)
    }
    
    go
  }
  
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(index: Int, acc: Boolean): Boolean = {
      if (!acc) acc
      else if (index + 1 >= as.length) acc
      else go(index + 1, ordered(as(index), as(index+1)))
    }
    
    go(0, true)
  }
  
  // 0, 1, 1, 2, 3, 5, 8, 13
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, acc: Int): Int = {
      if (a <= 0) 0
      else if (a == 1) 1
      else go(a, acc)
    }
    
    go(n, 0)
  }
  
}