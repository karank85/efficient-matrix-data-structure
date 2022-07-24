import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArrayBuffer

object Matrix extends App {

  /**
   * Converts 2D array matrix into sparse matrix in DOK representation
   * @param data 2D array matrix
   * @return sparse matrix class
   */
  def computeSparseMatrix(data: ArrayBuffer[ArrayBuffer[Int]]): SparseMatrix = {
    val m = data.length
    val n = data.head.length
    val mp = mutable.Map[(Int,Int),Int]()
    (0 until m).foreach(i => {
      (0 until n).foreach(j => {
        val elem = data(i)(j)
        if elem != 0 then mp.addOne((i,j) -> elem)
      })
    })
    SparseMatrix(mp.toMap,n,m)
  }


  /**
   * Creates identity matrix with size k x k
   * @param k size for identity matrix
   * @return identity matrix in 2D array
   */
  def createIdentity(k: Int): ArrayBuffer[ArrayBuffer[Int]] = {
    ArrayBuffer.tabulate(k)(j => ArrayBuffer.tabulate(k)(i => if i == j then 1 else 0))
  }


  val mt1 = ArrayBuffer.fill(2048, 2048)(1)
  val mt2 = ArrayBuffer.fill(2048,2048)(5)
  val mt3 = ArrayBuffer.tabulate(50)(i => ArrayBuffer.tabulate(30)(j => (i+1)*(j+2)))
  val mt4 = ArrayBuffer.tabulate(50)(i => ArrayBuffer.tabulate(30)(j => i+j))
  val mt5 = ArrayBuffer(ArrayBuffer(1,2,3,5),ArrayBuffer(1,1,7,6),ArrayBuffer(1,12,3,7),ArrayBuffer(1,2,3,9))
  val mt6 = ArrayBuffer(ArrayBuffer(1,0),ArrayBuffer(0,1))
  val mt7 = ArrayBuffer(ArrayBuffer(0,1),ArrayBuffer(1,0))

  val m1 = new DenseMatrix(mt1)
  val m2 = new DenseMatrix(mt2)
  val m3 = new DenseMatrix(mt3)
  val m4 = new DenseMatrix(mt4)
  val m5 = new DenseMatrix(mt5)
  val m6 = new DenseMatrix(mt6)
  val m7 = new DenseMatrix(mt7)

  val sm1 = ArrayBuffer(ArrayBuffer(1,0),ArrayBuffer(0,1),ArrayBuffer(1,1))
  val sm2 = ArrayBuffer(ArrayBuffer(0,1),ArrayBuffer(1,0))

  val s1 = computeSparseMatrix(sm1)
  val s2 = computeSparseMatrix(sm2)

  println(s1.getMatrix)
  //println(s1.transpose.getMatrix)
  println(s2.getMatrix)
  println("Tace is: " + s2.trace)
  //println(s2.transpose.getMatrix)
  //println((s1 + s2).getMatrix)


  val start1 = System.nanoTime()
  val addition = m6 + m7
  println(addition.getMatrix)
  val end1 = (System.nanoTime()-start1)/1e9d

  println((s1 * s2).getMatrix)
  //println("Runtime: " + end1)
  //println(addition.getMatrixArray)

  println(Matrix.createIdentity(5))


  //println(m3 == m1)
  println(m5.determinant)
  println(m5.inverse.getMatrix)
  println(m5.transpose.getMatrix)
  println(m5.transpose.trace)

  //println((m2 * m1).getMatrixArray)


}
