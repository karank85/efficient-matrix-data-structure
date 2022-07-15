import scala.collection.parallel.CollectionConverters._
import scala.collection.mutable.ArrayBuffer

object Matrix extends App {

  class Matrix(data: ArrayBuffer[ArrayBuffer[Int]]) {

    private val mt: ArrayBuffer[ArrayBuffer[Int]] = data;
    private val m = mt.length
    private val n = mt.head.length

    def +(that: Matrix): Matrix = ???

    def *(that: Matrix): Matrix = {
      // check this.n == that.m
      if n == that.m then
        val thatMatrix = that.mt
        // (this.m * this.n) x (that.m * that.n) = this.m * that.n
        val newMatrix: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer.fill(m, that.n)(0)
        (0 until m).par.foreach(i => {
          (0 until n).par.foreach(j => {
            (0 until that.n).par.foreach(k => {
              newMatrix(i)(j) += mt(i)(k) * thatMatrix(k)(j)
            })
          })
        })
        new Matrix(newMatrix)
      else throw Exception("Can't be multiplied!")
    }

    def ==(that: Matrix): Boolean = ???

    def transpose: Matrix = {
      val newMatrix: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer.fill(m, n)(0)
      (0 until m).par.foreach(i => {
        (0 until n).par.foreach(j => {
            newMatrix(i)(j) = mt(j)(i)
        })
      })
      new Matrix(newMatrix)
    }

    def determinant: Int = ???

    def isSymmetric: Boolean = transpose == this

    def isSkew: Boolean = !(transpose == this)

    def entryAt(x: Int, y: Int): Int = mt(x)(y)

    def isInvertible: Boolean = determinant != 0

    def inverse: Matrix = if isInvertible then this * createIdentity(mt.length) else this

    def createIdentity(n: Int): Matrix = ???

    def getMatrixArray: ArrayBuffer[ArrayBuffer[Int]] = mt
  }

  val mt1 = ArrayBuffer.fill(2048,2048)(1)
  val mt2 = ArrayBuffer.fill(2048,2048)(5)
  val mt3 = ArrayBuffer(ArrayBuffer(1,2),ArrayBuffer(3,4))

  val m1 = new Matrix(mt1)
  val m2 = new Matrix(mt2)
  val m3 = new Matrix(mt3)


  val start1 = System.nanoTime()
  //val res = m2*m1
  val tranposed = m3.transpose
  val end1 = (System.nanoTime()-start1)/1e9d
  println("Runtime: " + end1)
  println(tranposed.getMatrixArray)


  //println((m2 * m1).getMatrixArray)


}
