/*
 * Enhanced Post Processing Tool (EPPT) Copyright (c) 2019.
 *
 * EPPT is copyrighted by the State of California, Department of Water Resources. It is licensed
 * under the GNU General Public License, version 2. This means it can be
 * copied, distributed, and modified freely, but you may not restrict others
 * in their ability to copy, distribute, and modify it. See the license below
 * for more details.
 *
 * GNU General Public License
 */
package vista.graph;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.util.Enumeration;

/**
 *
 */
public class AnimatorCanvas extends GECanvas implements AnimationObserver
{
	/**
	 *
	 */
	private static final boolean DEBUG = false;
	/**
	 *
	 */
	protected Animator _animator;

	// if ( _backgroundImage == null ){
	// Rectangle r = _backgroundElement.getBounds();
	// r.x=0; r.y=0;
	// _backgroundImage = createImage( r.width, r.height );
	// Graphics gi = _backgroundImage.getGraphics();
	// gi.setClip(r);
	// _backgroundElement.draw(gi, r);
	// }
	/**
	 *
	 */
	Image _backgroundImage = null;
	/**
	 *
	 */
	GraphicElement _backgroundElement = null;

	/**
	 *
	 */
	public AnimatorCanvas(GraphicElement ge, Animator animator)
	{
		super(ge);
		_animator = animator;
	}

	/**
	 *
	 */
	public void update(AnimationObservable o, Object arg)
	{
		if(DEBUG)
		{
			System.out.println("Updating graphics");
		}
		update(this.getGraphics());
	}

	/**
	 *
	 */
	public void paint(Graphics g)
	{
		Rectangle r = this.getBounds();
		if((r.width != oldR.width || r.height != oldR.height))
		{
			updateNeeded = true;
		}
		r.x = 0;
		r.y = 0;
		g.setClip(r);
		if(doubleBuffer)
		{
			if(updateNeeded)
			{
				_geImage = createImage(r.width, r.height);
				_backgroundImage = createImage(r.width, r.height);
				Graphics gb = _backgroundImage.getGraphics();
				gb.setClip(r);
				_ge.draw(gb, r);
				updateNeeded = false;
				oldR = r;
			}
			Graphics gi = _geImage.getGraphics();
			gi.setClip(r);
			gi.drawImage(_backgroundImage, 0, 0, this);
			// drawNextFrame(gi);
			_ge.setGraphics(gi);
			_ge.animateNext();
			g.drawImage(_geImage, 0, 0, this);
		}
		else
		{
			if(updateNeeded)
			{
				_ge.draw(g, r);
				_ge.setGraphics(g);
				_ge.animateNext();
				// drawNextFrame(g);
			}
			else
			{
				_ge.draw(g);
				_ge.setGraphics(g);
				_ge.animateNext();
				// drawNextFrame(g);
			}
		}
	}

	/**
	 *
	 */
	protected void drawNextFrame(Graphics gi)
	{

		if(_ge instanceof GEContainer)
		{
			drawNextFrame((GEContainer) _ge, gi);
		}
		else
		{
			_ge.setGraphics(gi);
			((Animate) _ge).animateNext();
		}
	}

	/**
	 *
	 */
	private void drawNextFrame(GEContainer gec, Graphics gi)
	{
		for(Enumeration e = gec.getIterator(); e.hasMoreElements(); )
		{
			GraphicElement ge = (GraphicElement) e.nextElement();
			if(ge instanceof Animate)
			{
				ge.setGraphics(gi);
				((Animate) ge).animateNext();
			}
			else if(ge instanceof GEContainer)
			{
				drawNextFrame((GEContainer) ge, gi);
			}
		}
	}

	/**
	 *
	 */
	public void update(Graphics g)
	{
		paint(g);
	}

	/**
	 *
	 */
	public Animator getAnimator()
	{
		return _animator;
	}
}
