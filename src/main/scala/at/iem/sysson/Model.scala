package at.iem.sysson

object Model {
  type Listener[U] = PartialFunction[U, Unit]
}
trait Model[U] {
  /**
   * Registers a listener for updates from the model.
   * A listener is simply a partial function which receives instances of `U`. Therefore
   * the listener can decide with pattern match cases which updates it wants to handle.
   *
   * Example:
   * {{{
   *   m.addListener {
   *     case NcviewSync.Open(path) => ...
   *   }
   * }}}
   *
   * __Note:__ If the listener should be removed at some point, it is important to store it somewhere:
   *
   * {{{
   *   val l: NcviewSync.Listener = { case NcviewSync.Open(path) => ... }
   *   m.addListener(l)
   *   ...
   *   m.removeListener(l)
   * }}}
   */
  def addListener(pf: Model.Listener[U]): Model.Listener[U]
  /**
   * Unregisters a listener for updates from the model.
   */
  def removeListener(pf: Model.Listener[U]): Unit
}