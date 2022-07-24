import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.collection.mutable.Map
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArrayBuffer

object Matrix extends App {

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

  val s1 = new SparseMatrix(SparseMatrix.computeSparseMatrix(sm1), sm1.head.length, sm1.length)
  val s2 = new SparseMatrix(SparseMatrix.computeSparseMatrix(sm2), sm2.head.length, sm2.length)

  println(s1.getMatrix)
  //println(s1.transpose.getMatrix)
  println(s2.getMatrix)
  //println(s2.transpose.getMatrix)
  //println((s1 + s2).getMatrix)


  val start1 = System.nanoTime()
  val addition = m6 + m7
  println(addition.getMatrixArray)
  val end1 = (System.nanoTime()-start1)/1e9d

  println((s1 * s2).getMatrix)
  //println("Runtime: " + end1)
  //println(addition.getMatrixArray)


  //println(m3 == m1)
  println(m5.determinant)
  println(m5.inverse.getMatrixArray)
  println(m5.transpose.getMatrixArray)



  //println((m2 * m1).getMatrixArray)


}
