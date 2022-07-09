object Matrix {

  class Matrix(data: List[List[Int]]) {

    private val mt: List[List[Int]] = data;

    def +(that: Matrix): Matrix = ???

    def *(that: Matrix): Matrix = ???

    def ==(that: Matrix): Boolean = ???

    def transpose: Matrix = ???

    def determinant: Int = ???

    def isSymmetric: Boolean = ???

    def isSkew: Boolean = ???

    def entryAt(x: Int, y: Int): Int = ???

    def isInvertible: Boolean = ???

    def inverse: Matrix = ???

    def createIdentity(n: Int): Matrix = ???
  }


}
