import scala.collection.parallel.CollectionConverters._
import scala.collection.mutable.ArrayBuffer

object Matrix extends App {

  class DenseMatrix(data: ArrayBuffer[ArrayBuffer[Int]]) {

    private val mt: ArrayBuffer[ArrayBuffer[Int]] = data;
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

    def inverse: DenseMatrix = if isInvertible then this * createIdentity(mt.length) else this

    def createIdentity(n: Int): DenseMatrix = ???

    def getMatrixArray: ArrayBuffer[ArrayBuffer[Int]] = mt
  }

  class SparseMatrices(data: ArrayBuffer[ArrayBuffer[Int]]) {

    private val mt: ArrayBuffer[ArrayBuffer[Int]] = data;
    private val m = mt.length
    private val n = mt.head.length

  }

  val mt1 = ArrayBuffer.fill(2048,2048)(1)
  val mt2 = ArrayBuffer.fill(2048,2048)(5)
  val mt3 = ArrayBuffer(ArrayBuffer(1,2),ArrayBuffer(3,4))
  val mt4 = ArrayBuffer(ArrayBuffer(1,2),ArrayBuffer(3,4))

  val m1 = new DenseMatrix(mt1)
  val m2 = new DenseMatrix(mt2)
  val m3 = new DenseMatrix(mt3)
  val m4 = new DenseMatrix(mt4)


  val start1 = System.nanoTime()
  //val res = m2*m1
  val addition = m3 + m4
  val end1 = (System.nanoTime()-start1)/1e9d
  println("Runtime: " + end1)
  println(addition.getMatrixArray)
  println(m3 == m1)


  //println((m2 * m1).getMatrixArray)


}
