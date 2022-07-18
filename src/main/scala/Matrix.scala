import java.util.concurrent.atomic.AtomicInteger
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArrayBuffer

object Matrix extends App {

  class DenseMatrix(data: ArrayBuffer[ArrayBuffer[Int]]) {

    private val mt: ArrayBuffer[ArrayBuffer[Int]] = data
    private val m = mt.length
    private val n = mt.head.length

    def +(that: DenseMatrix): DenseMatrix = {
      if n == that.n && m == that.m then
        val thatData = that.mt
        val newMatrix: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer.fill(m, n)(0)
        (0 until m).par.foreach(i => {
          (0 until n).par.foreach(j => newMatrix(i)(j) = mt(i)(j) + thatData(i)(j))
        })
        new DenseMatrix(newMatrix)
      else throw Exception("Can't be added!")

    }

    def *(that: DenseMatrix): DenseMatrix = {
      // check this.n == that.m
      if n == that.m then
        val thatMatrix = that.mt
        // (this.m * this.n) x (that.m * that.n) = this.m * that.n
        val newMatrix: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer.fill(m, that.n)(0)
        (0 until m).par.foreach(i => {
          (0 until n).par.foreach(j => {
            (0 until that.n).par.foreach(k => newMatrix(i)(j) += mt(i)(k) * thatMatrix(k)(j))
          })
        })
        new DenseMatrix(newMatrix)
      else throw Exception("Can't be multiplied!")
    }

    def ==(that: DenseMatrix): Boolean = {
      if n != that.n || m != that.m then false
      else
        val thatData = that.mt
        (0 until m).par.foreach(i => {
          (0 until n).par.foreach(j => if mt(i)(j) != thatData(i)(j) then return false)
        })
        true
    }

    def transpose: DenseMatrix = {
      val newMatrix: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer.fill(m, n)(0)
      (0 until m).par.foreach(i => {
        (0 until n).par.foreach(j => newMatrix(i)(j) = mt(j)(i))
      })
      new DenseMatrix(newMatrix)
    }

    def determinant: Int = ???

    def isSymmetric: Boolean = transpose == this

    def isSkew: Boolean = !(transpose == this)

    def entryAt(x: Int, y: Int): Int = mt(x)(y)

    def isInvertible: Boolean = determinant != 0

    def inverse: DenseMatrix = {
      if isInvertible then
        val dt = determinant
        val newMatrix: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer.fill(m, n)(0)
        (0 until m).par.foreach(i => {
          (0 until n).par.foreach(j => newMatrix(i)(j) = dt * mt(j)(i))
        })
        DenseMatrix(newMatrix)
      else
        throw Exception("Matrix is not invertible!")
    }

    def createIdentity(k: Int): DenseMatrix = {
      val res = ArrayBuffer.tabulate(k)(j => ArrayBuffer.tabulate(k)(i => if i == k then 1 else 0))
      DenseMatrix(res)
    }

    def getMatrixArray: ArrayBuffer[ArrayBuffer[Int]] = mt
  }

  class SparseMatrix(data: ArrayBuffer[ArrayBuffer[Int]]) {

    private val size = data.head.length
    private val mt = data

    def +(that: SparseMatrix): SparseMatrix = ???

    def *(that: SparseMatrix): SparseMatrix = ???

    def ==(that: SparseMatrix): Boolean = {
      if size != that.size then return false
      else
        (0 until size).par.foreach(i => {
          val thatMT = that.mt; val thisRow = mt(i)(0); val thisCol = mt(i)(1); val thisValue = mt(i)(2)
          val thatRow = thatMT(i)(0); val thatCol = thatMT(i)(1); val thatValue = thatMT(i)(2)
          if (thisRow != thatRow || thisCol != thatCol || thisValue != thatValue) return false
        })
      true
    }

    def determinant: Int = ???

    def transpose: SparseMatrix = ???

    def entryAt(x: Int, y: Int): Int = ???

    def isSymmetric: Boolean = transpose == this

    def isSkew: Boolean = !(transpose == this)

    def getMatrixArray: ArrayBuffer[ArrayBuffer[Int]] = mt

  }

  object SparseMatrix {

    def computeSparseMatrix(data: ArrayBuffer[ArrayBuffer[Int]]): ArrayBuffer[ArrayBuffer[Int]] = {
      val m = data.length
      val n = data.head.length
      val size = data.par.map(_.par.count(_ != 0)).sum
      val mt = ArrayBuffer.fill(3, size)(0)
      val counter = AtomicInteger(0)
      (0 until m).foreach(i => {
        (0 until n).foreach(j => {
          val elem = data(i)(j)
          if elem != 0 then
            val row = counter.get()
            mt(0)(row) = i
            mt(1)(row) = j
            mt(2)(row) = elem
            counter.getAndIncrement()
        })
      })
      mt
    }
  }


  val mt1 = ArrayBuffer.fill(2048, 2048)(1)
  val mt2 = ArrayBuffer.fill(2048,2048)(5)
  val mt3 = ArrayBuffer.tabulate(50)(i => ArrayBuffer.tabulate(30)(j => (i+1)*(j+2)))
  val mt4 = ArrayBuffer.tabulate(50)(i => ArrayBuffer.tabulate(30)(j => i+j))

  val m1 = new DenseMatrix(mt1)
  val m2 = new DenseMatrix(mt2)
  val m3 = new DenseMatrix(mt3)
  val m4 = new DenseMatrix(mt4)

  val sm1 = ArrayBuffer(ArrayBuffer(0,0,3,0,4),ArrayBuffer(0,0,5,7,0),ArrayBuffer(0,0,0,0,0),ArrayBuffer(0,2,6,0,0))

  val s1 = new SparseMatrix(sm1)

  println(sm1)
  println(s1.getMatrixArray)


  val start1 = System.nanoTime()
  val addition = m3 + m4
  val end1 = (System.nanoTime()-start1)/1e9d
  //println("Runtime: " + end1)
  //println(addition.getMatrixArray)


  //println(m3 == m1)
  //println(m3.determinant)



  //println((m2 * m1).getMatrixArray)


}
