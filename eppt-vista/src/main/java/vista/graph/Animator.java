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
 * This class handles the animation. Animation is controlled in the run method
 * of this class and a thread object is used to run that method. Objects that
 * need to be animated every frame register themselves with the Animator which
 * then uses the observer pattern to notify them at the appropriate time.
 */
public class Animator implements Runnable
{
	/**
	 * debuggin' purposes
	 */
	private static final boolean DEBUG = false;
	/**
	 * The observable for animation
	 */
	protected AnimationObservable obs = null;
	/**
	 * The observable for displays
	 */
	protected AnimationObservable displayNotifier = null;
	/**
	 * length in millisecs to make animator thread sleep to maintain even
	 * animation speed.
	 */
	protected long sleepInterval = 0;
	/**
	 * Interval between two animations in millisecs
	 */
	protected long interval = 10;
	/**
	 * The thread to run this animation
	 */
	protected Thread animatorThread = null;

	/**
	 * Initializes the observer
	 */
	public Animator()
	{
		obs = new AnimationObservable();
		displayNotifier = new AnimationObservable();
	}

	/**
	 * creates a thread and starts animation.
	 */
	public void startAnimation()
	{
		if(animatorThread == null)
		{
			animatorThread = new Thread(this);
		}
		animatorThread.start();
	}

	/**
	 * stops animation by setting thread to null
	 */
	public void stopAnimation()
	{
		animatorThread = null;
	}

	/**
	 * checks the state of animation
	 */
	public boolean isAnimationRunning()
	{
		return (animatorThread != null);
	}

	/**
	 * The main loop of the animation thread. It keeps the animation going at an
	 * even pace.
	 */
	public void run()
	{
		long startTime = System.currentTimeMillis();
		while(Thread.currentThread() == animatorThread)
		{
			animate();
			notifyDisplays();
			try
			{
				startTime += interval;
				Thread.sleep(Math
						.max(0, startTime - System.currentTimeMillis()));
			}
			catch(InterruptedException e)
			{
				break;
			}
		}
	}

	/**
	 * notifies all observers
	 */
	protected void notifyDisplays()
	{
		displayNotifier.notifyAll(this);
	}

	/**
	 * animates registered objects by notifying them.
	 */
	protected void animate()
	{
		if(DEBUG)
		{
			System.out.println("Notifying Observers");
		}
		obs.notifyAll(this);
	}

	/**
	 * Adds the element to the list of observers for animation
	 */
	public void addAnimateElement(AnimationObserver element)
	{
		obs.addObserver(element);
	}

	/**
	 * adds display
	 */
	public void addAnimateDisplay(AnimationObserver display)
	{
		displayNotifier.addObserver(display);
	}

	/**
	 * gets the interval between animation frames
	 */
	public long getInterval()
	{
		return interval;
	}

	/**
	 * sets the interval or interval between animation frames
	 */
	public void setInterval(long d)
	{
		interval = d;
	}
}
