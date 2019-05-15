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


/**
 * The base class for objects that need to be animated. This class has its
 * update method called by the Animator class. This class implements setBounds
 * and draw methods to render itself on the graphics context
 */
public abstract class AnimateElement extends GraphicElement implements
															AnimationObserver, Animate
{
	/**
	 * constructor
	 */
	public AnimateElement(GEAttr attributes)
	{
		super(attributes);
	}

	/**
	 * The update method is called when the animation frame needs to be updated.
	 * Each element is responsible for drawing itself within the bounds given.
	 */
	public abstract void update(AnimationObservable o, Object arg);

	/**
	 * Animates and displays next frame.
	 */
	public abstract void animateNext();
}
