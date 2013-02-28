package at.iem.sysson
package gui
package impl

private[gui] object InterpreterViewImpl {
  def apply(): InterpreterView = new Impl

  private final class Impl extends InterpreterView {
    def component = ???
  }
}