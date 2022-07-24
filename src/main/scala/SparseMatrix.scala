import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArrayBuffer

class SparseMatrix(data: Map[(Int,Int),Int], n: Int, m: Int) {

  private val size = data.size
  private val colSize = n
  private val rowSize = m
  private val mt = data

  /**
   * Adds two sparse matrix together
   * @param that the other sparse matrix being added to
   * @return added sparse matrix of two sparse matrices
   */
  def +(that: SparseMatrix): SparseMatrix = {
    val thatCol = that.colSize; val thatRow = that.rowSize; val thatSize = that.size; val thatMatrix = that.mt
    if colSize != thatCol || thatRow != rowSize || size != thatSize then throw Error("Can't add!")
    val lst = mt.par
    val addedDuplicates = lst.map(i =>
      val ((r, c), e) = i
      if thatMatrix.contains((r,c)) then
        (r,c) -> (e + thatMatrix((r,c)))
      else
        (r, c) -> e
    )
    val output = thatMatrix.par.filter((k,v) => !thatMatrix.contains(k))

    new SparseMatrix(Map() ++ output ++ addedDuplicates, n, m)
  }

  /**
   * Multiply two sparse matrix together
   * @param that the other sparse matrix being multiplied to
   * @return multiplied sparse matrix of two sparse matrices
   */
  def *(that: SparseMatrix): SparseMatrix = {
    if colSize != that.rowSize then
      throw Exception("Can't be multiplied!")
    else
      val thatMT = that.getMatrix
      val output = new ConcurrentHashMap[(Int,Int),AtomicInteger]()
      (0 until rowSize).par.foreach(i => {
        (0 until that.colSize).par.foreach(j => {
          (0 until that.rowSize).par.foreach(k => {
            if mt.contains((i,k)) && thatMT.contains((k,j)) then
              this.synchronized(output.put((i,j), AtomicInteger(output.getOrDefault((i,j),AtomicInteger(0)).addAndGet(mt(i,k)*thatMT(k,j)))))
          })
        })
      })

      SparseMatrix(Map() ++ output.asScala.keySet.par.map(s => s -> output.get(s).get()).toMap, rowSize, that.colSize)
  }

  /**
   * Checks if two sparse matrices are equal
   * @param that the other sparse matrix being compared two
   * @return boolean of if two sparse matrices are equal
   */
  def ==(that: SparseMatrix): Boolean = {
    val thatCol = that.colSize; val thatRow = that.rowSize; val thatSize = that.size; val thatMatrix = that.mt
    if colSize != thatCol || thatRow != rowSize || size != thatSize then false
    else mt == thatMatrix
  }

  /**
   * Gets the determinant of sparse matrices
   * @return determinant of sparse matrix
   */
  def determinant: Int = {
    val dm = (0 until rowSize).par.map(i => {
      (0 until colSize).par.map(j => {
        if mt.contains((i,j)) then mt((i,j)) else 0
      }).to(ArrayBuffer)
    }).to(ArrayBuffer)
    DenseMatrix(dm).determinant
  }

  /**
   * Find trace of the sparse matrix
   * @return trace of sparse matrix
   */
  def trace: Int = {
    if colSize != rowSize then throw Exception("Has to be square matrix")
    else
      (0 until n).par.flatMap(i => {
        (0 until n).par.map(k => if i == k && mt.contains((i,k)) then mt((i,k)) else 0
        )
      }).sum
  }

  /**
   * Transpose sparse matrices
   * @return transposed sparse matrix
   */
  def transpose: SparseMatrix = {
    val lst = mt.par
    val transposed = lst.map(i =>
      val ((r, c), e) = i
      (c, r) -> e
    )
    val output = Map() ++ transposed
    new SparseMatrix(output, rowSize, colSize)
  }

  /**
   * Gets the element in specified at a coordinate
   * @param row row coordinate
   * @param col col coordinate
   * @return element on the specific coordinate if its within the bounds
   *         else throw error
   */
  def entryAt(row: Int, col: Int): Int = {
    val tup = (row,col)
    val elem = mt.get(tup)
    elem match {
      case Some(i) => i;
      case _ =>
        if row < rowSize && col < colSize then 0
        else throw Error("Index out of bounds!")
    }
  }

  /**
   * Checks if the sparse matrix is symmetric
   * @return boolean if sparse matrix is symmetric
   */
  def isSymmetric: Boolean = transpose == this

  /**
   * Checks if the sparse matrix is skew
   * @return boolean if sparse matrix is skew
   */
  def isSkew: Boolean = !(transpose == this)

  /** Gets the sparse matrix data in DOK representation */
  def getMatrix: Map[(Int,Int),Int] = mt

}


