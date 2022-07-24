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

}
