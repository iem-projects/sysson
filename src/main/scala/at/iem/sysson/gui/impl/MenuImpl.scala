package at.iem.sysson.gui
package impl

import javax.swing.KeyStroke
import swing.{Frame, Action}

private[gui] object MenuImpl {
  i =>

  private final val ID_KEY = "de.sciss.gui.Menu.id"

  // ---- constructors ----
  import Menu.Item.Attributes

  def itemApply(id: String, action: Action): Menu.Item = new Item(id, action)
  def itemApply(id: String)(attr: Attributes)(action: => Unit): Menu.Item =
    new Item(id, i.action(id, attr.text, attr.keyStroke)(action))

  def itemApply(id: String, attr: Attributes): Menu.Item = {
    val res     = new Item(id, i.noAction(id, attr.text, attr.keyStroke))
    res.enabled = false
    res
  }

  def groupApply(id: String, action: Action): Menu.Group = new Group(id, action)
  def groupApply(id: String)(text: String)(action: => Unit): Menu.Group =
    new Group(id, i.action(id, text, None)(action))
  def groupApply(id: String, text: String): Menu.Group =
    new Group(id, i.noAction(id, text, None))

  def rootApply(): Menu.Root = new Root

  // ---- util ---

  private def action(id: String, text: String, key: Option[KeyStroke])(body: => Unit): Action =
    new ActionImpl(id, text, key, body)

  private def noAction(id: String, text: String, key: Option[KeyStroke]): Action =
    new ActionImpl(id, text, key, ())

  private final class ActionImpl(id: String, text: String, key: Option[KeyStroke], body: => Unit)
    extends Action(text) {

    accelerator = key
    peer.putValue(ID_KEY, id)

    def apply() { body }
  }

  // ---- node ----

//  private final case class Realized[C](window: Frame, component: C)

  private trait Node {
    _: Menu.NodeLike =>

    protected def prefix: String

    override def toString = s"$prefix($id)"
  }

  // ---- realizable tracking ----
  private trait Realizable[C <: swing.Component] extends Node {
    _: Menu.NodeLike =>

    private var mapRealized = Map.empty[Frame, C]

    final protected def getRealized(w: Frame): Option[C] = mapRealized.get(w)
    final protected def realizedIterator: Iterator[(Frame, C)] = mapRealized.iterator

    final protected def addRealized(w: Frame, c: C) {
      mapRealized += w -> c // Realized(w, c)
    }

    final protected def removeRealized(w: Frame) {
      mapRealized -= w
    }
  }

  private trait CanEnable {
    var enabled: Boolean

    final def enable(): this.type = {
      enabled = true
      this
    }
    final def disable(): this.type = {
      enabled = false
      this
    }
  }

  // ---- item ----

  private trait ItemLike /* [C <: swing.MenuItem] extends Node[C] with Menu.ItemLike[C] with */ extends CanEnable {
    _: Menu.ItemLike[_ <: swing.MenuItem] =>

    private var mapWindowActions = Map.empty[Frame, Action] withDefaultValue action
    final def enabled = action.enabled
    final def enabled_=(value: Boolean) { action.enabled = value }

    final protected def actionFor(w: Frame): Action = mapWindowActions(w)

    final def setAction(w: Frame, action: Action) {
      if (mapWindowActions.contains(w)) throw new IllegalStateException("Window specific action already set")
      mapWindowActions += w -> action
    }

    final def setAction(w: Frame)(body: => Unit) {
      setAction(w, i.action(id, action.title, action.accelerator)(body))
    }

    final def clearAction(w: Frame) {
      mapWindowActions -= w
    }
  }

  private final class Item(val id: String, val action: Action) extends ItemLike with Menu.Item {
    protected def prefix = "Menu.Item"

    def create(w: Frame): swing.MenuItem = {
      new swing.MenuItem(actionFor(w))
    }

    def destroy(w: Frame) {
      clearAction(w)
    }
  }

  // ---- group ----

  private final class NodeProxy(val window: Option[Frame]) {
    var seq   = Vector.empty[Menu.Element]
    var map   = Map.empty[String, Menu.NodeLike]

    def create(c: swing.SequentialContainer, w: Frame) {
      if (window.isDefined) require(window.get == w)  // ???

      seq.foreach { n => c.contents += n.create(w)}
    }

    def destroy(w: Frame) {
      if (window.isDefined) require(window.get == w)  // ???

      seq.foreach(_.destroy(w))
    }
  }

  private trait GroupLike[C <: swing.Component with swing.SequentialContainer]
    extends Realizable[C] {
    _: Menu.NodeLike =>

    private var proxies       = Map.empty[Frame, NodeProxy]
    private val defaultProxy  = new NodeProxy(None)

    final protected def added(p: NodeProxy, n: Menu.Element) {
      val isDefault = p.window.isEmpty
      realizedIterator.foreach { case (w, r) =>
        if (isDefault || p.window == Some(w)) r.contents += n.create(w)
      }
    }

    private def getProxy(w: Frame): Option[NodeProxy] = proxies.get(w)

    private def proxy(wo: Option[Frame]): NodeProxy = wo match {
      case Some(w) =>
        proxies.getOrElse(w, {
          val p = new NodeProxy(wo)
          proxies += w -> p
          p
        })
      case None => defaultProxy
    }

    final protected def createProxy(w: Frame, component: C) {
      defaultProxy.create(component, w)
      getProxy(w).foreach(_.create(component, w)) // XXX TODO
    }

    final protected def destroyProxy(w: Frame) {
      defaultProxy.destroy(w)
      getProxy(w).foreach { p =>
        p.destroy(w)
     	  if( p.seq.isEmpty ) proxies -= w
      }
    }

    private def add(p: NodeProxy, elem: Menu.Element) {
      elem match {
        case n: Menu.NodeLike =>
          require(!p.map.contains(n.id), "Element already added")
          p.map += n.id -> n
        case _ =>
      }
      p.seq :+= elem
      added(p, elem)
    }

    // adds window specific action to the tail
    final def add(w: Option[Frame], elem: Menu.Element): this.type = {
      add(proxy(w), elem)
      this
    }

    // adds to the tail
    final def add(n: Menu.Element): this.type = {
      add(defaultProxy, n)
      this
    }

//	// inserts at given index
//	private void add( NodeProxy p, Menu.Node n, int index )
//	{
//		if( p.mapElements.put( n.getID(), n ) != null ) throw new IllegalArgumentException( "Element already added : " + n );
//
//		Realized r;
//		final boolean isDefault = p.w == null;
//
//		p.collElements.add( index, n );
//
//		for( Iterator iter = mapRealized.values().iterator(); iter.hasNext(); ) {
//			r = (Realized) iter.next();
//			if( isDefault || (p.w == r.w) ) {
//				r.c.add( n.create( r.w ), index + (isDefault ? 0 : defaultProxy.size()) );
//			}
//		}
//	}
  }

  private final class Group(val id: String, val action: Action)
    extends GroupLike[swing.Menu] with ItemLike with Menu.Group {

    protected def prefix = "Menu.Group"

    def create(w: Frame): swing.Menu = {
      val c = createComponent(actionFor(w))
      addRealized(w, c)
      createProxy(w, c)
      c
    }

    def destroy(w: Frame) {
      removeRealized(w)
      clearAction(w)
      destroyProxy(w)
    }

    def addLine(): this.type = add(Menu.Line)

    private def createComponent(a: Action): swing.Menu = {
      val res     = new swing.Menu(a.title)
      res.action  = a
      res
    }
  }

  // ---- root ----

  private final class Root extends GroupLike[swing.MenuBar] with Menu.Root with CanEnable {
    private var _enabled = true
    def id = "root"
    protected def prefix = "MenuBar"

    def create(w: Frame): swing.MenuBar = {
      val res = new swing.MenuBar
      addRealized(w, res)
      createProxy(w, res)
      if (!enabled) res.enabled = false
      res
    }

    def destroy(w: Frame) {
      removeRealized(w)
      destroyProxy(w)
    }

    def enabled = _enabled
    def enabled_=(value: Boolean) {
      _enabled = value
      realizedIterator.foreach(_._2.enabled = value)
    }
  }

//  {
//  	private static int uniqueID = 0;
//
//  	public MenuSeparator()
//  	{
//  		super( "_" + String.valueOf( uniqueID++ ), (Action) null );
//  	}
//
//  	public void setEnabled( boolean b ) { /* ignore */ }
//
//  	protected JComponent createComponent( Action a )
//  	{
//  		return new JSeparator();
//  	}
//  }
}