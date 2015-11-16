package at.iem.sysson
package graph

class MatrixTooLargeException(name: String, size: Int, maxSize: Int)
  extends RuntimeException(s"Matrix $name is too large ($size > $maxSize)")