import scala.collection.parallel.CollectionConverters._
import scala.collection.mutable.ArrayBuffer

object Matrix {

  class Matrix(data: ArrayBuffer[ArrayBuffer[Int]]) {

    private val mt: ArrayBuffer[ArrayBuffer[Int]] = data;

    def +(that: Matrix): Matrix = ???

    def *(that: Matrix): Matrix = ???

    def ==(that: Matrix): Boolean = ???

    def transpose: Matrix = ???

    def determinant: Int = ???

    def isSymmetric: Boolean = transpose == this

    def isSkew: Boolean = !(transpose == this)

    def entryAt(x: Int, y: Int): Int = mt(x)(y)

    def isInvertible: Boolean = determinant != 0

    def inverse: Matrix = if isInvertible then this * createIdentity(mt.length) else this

    def createIdentity(n: Int): Matrix = ???
  }


}
