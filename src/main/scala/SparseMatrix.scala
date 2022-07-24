import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArrayBuffer

class SparseMatrix(data: mutable.Map[(Int,Int),Int], n: Int, m: Int) {

  private val size = data.size
  private val colSize = n
  private val rowSize = m
  private val mt = data

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

    new SparseMatrix(mutable.Map() ++ output ++ addedDuplicates, n, m)
  }

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

      SparseMatrix(mutable.Map() ++ output.asScala.keySet.par.map(s => s -> output.get(s).get()).toMap, rowSize, that.colSize)
  }

  def ==(that: SparseMatrix): Boolean = {
    val thatCol = that.colSize; val thatRow = that.rowSize; val thatSize = that.size; val thatMatrix = that.mt
    if colSize != thatCol || thatRow != rowSize || size != thatSize then false
    else mt == thatMatrix
  }

  def determinant: Int = {
    val dm = (0 until rowSize).par.map(i => {
      (0 until colSize).par.map(j => {
        if mt.contains((i,j)) then mt((i,j)) else 0
      }).to(ArrayBuffer)
    }).to(ArrayBuffer)
    DenseMatrix(dm).determinant
  }

  def transpose: SparseMatrix = {
    val lst = mt.par
    val transposed = lst.map(i =>
      val ((r, c), e) = i
      (c, r) -> e
    )
    val output = mutable.Map() ++ transposed
    new SparseMatrix(output, rowSize, colSize)
  }

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

  def isSymmetric: Boolean = transpose == this

  def isSkew: Boolean = !(transpose == this)

  def getMatrix: mutable.Map[(Int,Int),Int] = mt

}


