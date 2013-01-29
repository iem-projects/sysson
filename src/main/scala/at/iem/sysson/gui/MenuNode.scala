package at.iem.sysson.gui

import swing.{Action, Frame, Component}
import javax.swing.KeyStroke

import impl.{MenuNodeImpl => Impl}

sealed trait MenuNodeLike {
  def id: String
  var enabled: Boolean
  def create(window: Frame): Component
  def destroy(window: Frame): Unit
}
trait MenuNode[+C <: Component] extends MenuNodeLike {
  def create(window: Frame): C
}

object MenuItem {
  def apply(id: String, action: Action): MenuItem = Impl.itemApply(id, action)
  def apply(id: String)(attr: Attributes)(action: => Unit): MenuItem =
    Impl.itemApply(id)(attr)(action)

  def apply(id: String, attr: Attributes): MenuItem = Impl.itemApply(id, attr)

  object Attributes {
    implicit final class TextOnly(val text: String) extends Attributes {
      def keyStroke   = None
    }
    implicit final class TextAndKeyStroke(tup: (String, KeyStroke)) extends Attributes {
      def text        = tup._1
      def keyStroke   = Some(tup._2)
    }
  }
  sealed trait Attributes {
    def text: String
    def keyStroke: Option[KeyStroke]
  }
}
trait MenuItemLike[+C <: swing.MenuItem] extends MenuNode[C] {
  def action: Action
  def setAction(window: Frame, action: Action): Unit
  def setAction(window: Frame)(body: => Unit): Unit
  def clearAction(window: Frame): Unit
}
trait MenuItem extends MenuItemLike[swing.MenuItem]

object MenuGroup {
  def apply(id: String, action: Action): MenuGroup = Impl.groupApply(id, action)
  def apply(id: String)(text: String)(action: => Unit): MenuGroup = Impl.groupApply(id)(text)(action)
  def apply(id: String, text: String): MenuGroup = Impl.groupApply(id, text)
}
trait MenuGroupLike[+C <: Component with swing.SequentialContainer] extends MenuNode[C]{
  def add(w: Option[Frame], n: MenuNodeLike): this.type
  def add(n: MenuNodeLike): this.type
}
trait MenuGroup extends MenuGroupLike[swing.Menu] with MenuItemLike[swing.Menu]

object MenuRoot {
  def apply(): MenuRoot = Impl.rootApply()
}
trait MenuRoot extends MenuGroupLike[swing.MenuBar]

//public class MenuGroup
//extends MenuItem // implements MenuNode
//{
//
//	public MenuNode get( String id )
//	{
//		return get( defaultProxy, id );
//	}
//
//	public MenuNode get( AbstractWindow w, String id )
//	{
//		return get( getProxy( w, false ), id );
//	}
//
//
//	private MenuNode get( NodeProxy p, String id )
//	{
//		final int i	= id.indexOf( '.' );
//
//		if( i == -1 ) {
//			return (MenuNode) p.mapElements.get( id );
//		} else {
//			final MenuGroup mg = (MenuGroup) p.mapElements.get( id.substring( 0, i ));
//			if( mg == null ) throw new NullPointerException( id );
//			return mg.get( p.w, id.substring( i + 1 ));
//		}
//	}
//
//
//	public int indexOf( String id )
//	{
//		return indexOf( defaultProxy, id );
//	}
//
//	public int indexOf( AbstractWindow w, String id )
//	{
//		return indexOf( getProxy( w, false ), id );
//	}
//
//	private int indexOf( NodeProxy p, String id )
//	{
//		final int i	= id.indexOf( '.' );
//
//		if( i == -1 ) {
//			return p.collElements.indexOf( p.mapElements.get( id ));
//		} else {
//			final MenuGroup mg = (MenuGroup) p.mapElements.get( id.substring( 0, i ));
//			if( mg == null ) throw new NullPointerException( id );
//			return mg.indexOf( id.substring( i + 1 ));
//		}
//	}
//
//	public MenuNode getByAction( Action a )
//	{
//		return getByAction( defaultProxy, a );
//	}
//
//	public MenuNode getByAction( AbstractWindow w, Action a )
//	{
//		return getByAction( getProxy( w, false ), a );
//	}
//
//	private MenuNode getByAction( NodeProxy p, Action a )
//	{
//		MenuNode n;
//
//		for( Iterator iter = p.collElements.iterator(); iter.hasNext(); ) {
//			n = (MenuNode) iter.next();
//			if( n.getAction() == a ) return n;
//		}
//
//		return null;
//	}
//
//	// adds window specific action to the tail
//	public void add( AbstractWindow w, MenuNode n )
//	{
//		add( getProxy( w, true ), n );
//	}
//
//	// adds to the tail
//	public void add( MenuNode n )
//	{
//		add( defaultProxy, n );
//	}
//
//	// inserts at given index
//	public void add( MenuNode n, int index )
//	{
//		add( defaultProxy, n, index );
//	}
//
//	// inserts at given index
//	public void add( AbstractWindow w, MenuNode n, int index )
//	{
//		add( getProxy( w, true ), n, index );
//	}
//
//	// inserts at given index
//	private void add( NodeProxy p, MenuNode n )
//	{
//		add( p, n, p.collElements.size() );
//	}
//
//	public int size()
//	{
//		return defaultProxy.size();
//	}
//
//	public MenuNode get( int idx )
//	{
//		return (MenuNode) defaultProxy.collElements.get( idx );
//	}
//
//	// inserts at given index
//	private void add( NodeProxy p, MenuNode n, int index )
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
//
//	public void addSeparator( AbstractWindow w )
//	{
//		add( w, new MenuSeparator() );
//	}
//
//	public void addSeparator()
//	{
//		addSeparator( null );
//	}
//
//	public void remove( int index )
//	{
//		remove( defaultProxy, index );
//	}
//
//	public void remove( AbstractWindow w, int index )
//	{
//		remove( getProxy( w, false ), index );
//	}
//
//	private void remove( NodeProxy p, int index )
//	{
//		final MenuNode	n = (MenuNode) p.collElements.remove( index );
//		Realized		r;
//
//		p.mapElements.remove( n.getID() );
//		final boolean isDefault = p.w == null;
//
//		for( Iterator iter = mapRealized.values().iterator(); iter.hasNext(); ) {
//			r = (Realized) iter.next();
//			if( isDefault || (p.w == r.w) ) {
//				r.c.remove( index + (isDefault ? 0 : defaultProxy.size()) );
//				n.destroy( r.w );
//			}
//		}
//	}
//
//	public void remove( MenuNode n )
//	{
//		remove( defaultProxy, n );
//	}
//
//	public void remove( AbstractWindow w, MenuNode n )
//	{
//		remove( getProxy( w, false ), n );
//	}
//
//	private void remove( NodeProxy p, MenuNode n )
//	{
//		for( int idx = 0; idx < p.collElements.size(); idx++ ) {
//			if( (MenuNode) p.collElements.get( idx ) == n ) {
//				remove( p, idx );
//				return;
//			}
//		}
//	}
//
//	public JComponent create( AbstractWindow w )
//	{
//		final JComponent c = super.create( w );
//		defaultProxy.create( c, w );
//		final NodeProxy p = getProxy( w, false );
//		if( p != null ) p.create( c, w );
//		return c;
//	}
//
//	public void destroy( AbstractWindow w )
//	{
//		super.destroy( w );
//		defaultProxy.destroy( w );
//		final NodeProxy p = getProxy( w, false );
//		if( p != null ) {
//			p.destroy( w );
//			if( p.isEmpty() ) {
//				proxies.remove( w );
//			}
//		}
//	}
//
//	protected JComponent createComponent( Action a )
//	{
//		return new JMenu( a );
//	}
//
//	public void putMimic( String id, AbstractWindow w, Action a )
//	{
//		if( a == null ) return;
//		final MenuItem mi = (MenuItem) get( id );
//		if( mi == null ) throw new NullPointerException( id );
//
//		final Action src = mi.getAction();
//		a.putValue( Action.NAME, src.getValue( Action.NAME ));
//		a.putValue( Action.SMALL_ICON, src.getValue( Action.SMALL_ICON ));
//		a.putValue( Action.ACCELERATOR_KEY, src.getValue( Action.ACCELERATOR_KEY ));
//		putNoNullNull( src, a, Action.MNEMONIC_KEY );
////		a.putValue( Action.MNEMONIC_KEY, src.getValue( Action.MNEMONIC_KEY ));
//		a.putValue( Action.SHORT_DESCRIPTION, src.getValue( Action.SHORT_DESCRIPTION ));
//		a.putValue( Action.LONG_DESCRIPTION, src.getValue( Action.LONG_DESCRIPTION ));
//
//		mi.put( w, a );
//	}
//
//	// due to bug in java 1.5 JMenuItem
//	private void putNoNullNull( Action src, Action dst, String key )
//	{
//		final Object srcVal = src.getValue( key );
//		final Object dstVal	= dst.getValue( key );
//		if( (srcVal == null) && (dstVal == null) ) return;
//		dst.putValue(  key, srcVal );
//	}
//
////	public void put( String id, AbstractWindow w, Action a )
////	{
////		final MenuItem mi = (MenuItem) get( id );
////		if( mi == null ) throw new NullPointerException( id );
////		mi.put( w, a );
////	}
//

//}
