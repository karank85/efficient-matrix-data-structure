import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
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

    def +(that: SparseMatrix): SparseMatrix = ???

    def *(that: SparseMatrix): SparseMatrix = ???

    def ==(that: SparseMatrix): Boolean = ???

    def determinant: Int = ???

    def transpose: SparseMatrix = {
      val lst = mt.par
      val transposed = lst.map(i => {
        val ((r, c), e) = i
        (c, r) -> e
      })
      val output = mutable.Map() ++ transposed
      new SparseMatrix(output, rowSize, colSize)
    }

    def entryAt(row: Int, col: Int): Int = {
      val tup = (row,col)
      val elem = mt.get(tup)
      elem match {
        case Some(i) => i;
        case _ => throw Error("Index doesn't exist")
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
          if data(i)(j) != 0 then mp.addOne((i,j) -> elem)
        })
      })
      mp
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

  val s1 = new SparseMatrix(SparseMatrix.computeSparseMatrix(sm1), sm1.head.length, sm1.length)

  println(sm1)
  println(s1.getMatrix)
  println(s1.transpose.getMatrix)


  val start1 = System.nanoTime()
  val addition = m3 + m4
  val end1 = (System.nanoTime()-start1)/1e9d
  //println("Runtime: " + end1)
  //println(addition.getMatrixArray)


  //println(m3 == m1)
  //println(m3.determinant)



  //println((m2 * m1).getMatrixArray)


}
