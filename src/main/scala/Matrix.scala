import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.collection.mutable.Map
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

        SparseMatrix(mutable.Map() ++ output.asScala.keySet.map(s => s -> output.get(s).get()).toMap, rowSize, that.colSize)
    }

    def ==(that: SparseMatrix): Boolean = {
      val thatCol = that.colSize; val thatRow = that.rowSize; val thatSize = that.size; val thatMatrix = that.mt
      if colSize != thatCol || thatRow != rowSize || size != thatSize then false
      else mt == thatMatrix
    }

    def determinant: Int = ???

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
        case _ => if row < rowSize && col < colSize then 0 else throw Error("Index out of bounds!")
      }
    }

    def isSymmetric: Boolean = transpose == this

    def isSkew: Boolean = !(transpose == this)

    def getMatrix: mutable.Map[(Int,Int),Int] = mt

  }

  object SparseMatrix {

    def computeSparseMatrix(data: ArrayBuffer[ArrayBuffer[Int]]): mutable.Map[(Int,Int), Int] = {
      val m = data.length
      val n = data.head.length
      val mp = mutable.Map[(Int,Int),Int]()
      (0 until m).foreach(i => {
        (0 until n).foreach(j => {
          val elem = data(i)(j)
          if elem != 0 then mp.addOne((i,j) -> elem)
        })
      })
      mp
    }
  }


  val mt1 = ArrayBuffer.fill(2048, 2048)(1)
  val mt2 = ArrayBuffer.fill(2048,2048)(5)
  val mt3 = ArrayBuffer.tabulate(50)(i => ArrayBuffer.tabulate(30)(j => (i+1)*(j+2)))
  val mt4 = ArrayBuffer.tabulate(50)(i => ArrayBuffer.tabulate(30)(j => i+j))
  val mt5 = ArrayBuffer(ArrayBuffer(1,2,3),ArrayBuffer(1,1,7),ArrayBuffer(1,12,3))
  val mt6 = ArrayBuffer(ArrayBuffer(1,0),ArrayBuffer(0,1))
  val mt7 = ArrayBuffer(ArrayBuffer(0,1),ArrayBuffer(1,0))

  val m1 = new DenseMatrix(mt1)
  val m2 = new DenseMatrix(mt2)
  val m3 = new DenseMatrix(mt3)
  val m4 = new DenseMatrix(mt4)
  val m5 = new DenseMatrix(mt5)
  val m6 = new DenseMatrix(mt6)
  val m7 = new DenseMatrix(mt7)

  val sm1 = ArrayBuffer(ArrayBuffer(1,1),ArrayBuffer(1,1))
  val sm2 = ArrayBuffer(ArrayBuffer(3),ArrayBuffer(4))

  val s1 = new SparseMatrix(SparseMatrix.computeSparseMatrix(sm1), sm1.head.length, sm1.length)
  val s2 = new SparseMatrix(SparseMatrix.computeSparseMatrix(sm2), sm2.head.length, sm2.length)

  println(s1.getMatrix)
  //println(s1.transpose.getMatrix)
  println(s2.getMatrix)
  //println(s2.transpose.getMatrix)
  //println((s1 + s2).getMatrix)


  val start1 = System.nanoTime()
  val addition = m6 == m7
  println(addition)
  val end1 = (System.nanoTime()-start1)/1e9d

  println((s1 * s2).getMatrix)
  //println("Runtime: " + end1)
  //println(addition.getMatrixArray)


  //println(m3 == m1)
  println(m5.determinant)



  //println((m2 * m1).getMatrixArray)


}
