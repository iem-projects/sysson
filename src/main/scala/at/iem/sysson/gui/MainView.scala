package at.iem.sysson.gui

import swing.Component

import impl.{MainViewImpl => Impl}

object MainView {
  def apply(): MainView = Impl()
}
trait MainView {
  def component: Component
}