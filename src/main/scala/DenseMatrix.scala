import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.CollectionConverters.*

class DenseMatrix(data: ArrayBuffer[ArrayBuffer[Int]]) {

  private val mt: ArrayBuffer[ArrayBuffer[Int]] = data
  private val m = mt.length
  private val n = mt.head.length

  def +(that: DenseMatrix): DenseMatrix = {
    if n == that.n && m == that.m then
      val thatData = that.mt
      val newMatrix = (0 until m).par.map(i => {
        (0 until n).par.map(j => mt(i)(j) + thatData(i)(j)).to(ArrayBuffer)
      }).to(ArrayBuffer)
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
      (0 until m).par.forall(i => {
        (0 until n).par.forall(j => mt(i)(j) == thatData(i)(j))
      })
  }

  def transpose: DenseMatrix = {
    val newMatrix = (0 until m).par.map(i => {
      (0 until n).par.map(j => mt(j)(i)).to(ArrayBuffer)
    }).to(ArrayBuffer)
    new DenseMatrix(newMatrix)
  }

  def determinant: Int = {

    def gaussElim(): ArrayBuffer[ArrayBuffer[Int]] = {
      val copyMatrix = mt.par.map(s => s.par.map(k => k).to(ArrayBuffer)).to(ArrayBuffer)
      (0 until n).foreach(i => {
        ((i+1) until n).foreach(j => {
          if copyMatrix(i)(i) != 0 then
            val ratio = copyMatrix(j)(i)/copyMatrix(i)(i)
            copyMatrix(j)(i) = 0
            (i+1 until n).foreach(k => {
              copyMatrix(j)(k) -= ratio*copyMatrix(i)(k)
            })
        })
      })
      copyMatrix
    }

    if m != n then throw Exception("Has to be a square matrix!")
    else
      val ge = gaussElim()

      (0 until n).par.flatMap(i => {
        (0 until n).par.map(k => if i == k then ge(i)(k) else 1
        )
      }).product

  }

  def isSymmetric: Boolean = transpose == this

  def isSkew: Boolean = !(transpose == this)

  def entryAt(row: Int, col: Int): Int = mt(row)(col)

  def isInvertible: Boolean = determinant != 0

  def inverse: DenseMatrix = {
    if isInvertible then
      val dt = 1/determinant
      val newMatrix = (0 until m).par.map(i => {
        (0 until n).par.map(j => dt * mt(i)(j)).to(ArrayBuffer)
      }).to(ArrayBuffer)
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
