import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.CollectionConverters.*

class DenseMatrix(data: ArrayBuffer[ArrayBuffer[Int]]) {

  private val mt: ArrayBuffer[ArrayBuffer[Int]] = data
  private val m = mt.length
  private val n = mt.head.length

  /**
   * Add two dense matrices 
   * @param that another dense matrix that is going to be added to
   * @return dense matrix added together
   */
  def +(that: DenseMatrix): DenseMatrix = {
    if n == that.n && m == that.m then
      val thatData = that.mt
      val newMatrix = (0 until m).par.map(i => {
        (0 until n).par.map(j => mt(i)(j) + thatData(i)(j)).to(ArrayBuffer)
      }).to(ArrayBuffer)
      new DenseMatrix(newMatrix)
    else throw Exception("Can't be added!")

  }

  /**
   * Multiply two dense matrices
   * @param that another dense matrix that is going to be multiplied to
   * @return dense matrix multiplied together
   */
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

  /**
   * Check if two dense matrices are equal
   * @param that dense matrix being compared to
   * @return boolean that tells if the matrices are equal
   */
  def ==(that: DenseMatrix): Boolean = {
    if n != that.n || m != that.m then false
    else
      val thatData = that.mt
      (0 until m).par.forall(i => {
        (0 until n).par.forall(j => mt(i)(j) == thatData(i)(j))
      })
  }

  /**
   * Transpose a dense matrix
   * @return transposed dense matrix
   */
  def transpose: DenseMatrix = {
    val newMatrix = (0 until m).par.map(i => {
      (0 until n).par.map(j => mt(j)(i)).to(ArrayBuffer)
    }).to(ArrayBuffer)
    new DenseMatrix(newMatrix)
  }

  /**
   * Finds the determinant of dense matrix
   * @return determinant of dense matrix
   */
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

  /**
   * Check ifs a dense matrix is symmetric
   * @return boolean if dense is symmetric
   */
  def isSymmetric: Boolean = transpose == this

  /**
   * Check ifs a dense matrix is skew
   * @return boolean if dense is skew
   */
  def isSkew: Boolean = !(transpose == this)

  /**
   * Gets the element in the matrix with the specified row and column 
   * @param row the row 
   * @param col the column
   * @return the element in specified row and column
   */
  def entryAt(row: Int, col: Int): Int = mt(row)(col)

  /**
   * Checks if dense matrix is invertible
   * @return boolean is dense matrix is invertible
   */
  def isInvertible: Boolean = determinant != 0

  /**
   * Computes the inverse of the dense matrix
   * @return inverse of dense matrix
   */
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

  /** Returns the matrix data array */
  def getMatrix: ArrayBuffer[ArrayBuffer[Int]] = mt
}
