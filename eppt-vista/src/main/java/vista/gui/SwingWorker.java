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
package vista.gui;

import javax.swing.*;

/**
 * An abstract class that you subclass to perform GUI-related work in a
 * dedicated thread. For instructions on using this class, see
 * http://java.sun.com/products/jfc/swingdoc-current/threads2.html
 */
public abstract class SwingWorker
{
	final Runnable doStarted = new Runnable()
	{
		public void run()
		{
			started();
		}
	};
	final Runnable doFinished = new Runnable()
	{
		public void run()
		{
			finished();
		}
	};
	Runnable doConstruct;
	private Object value; // see getValue(), setValue()
	private Thread thread;
	private ThreadVar threadVar;

	/**
	 * Start a thread that will call the <code>construct</code> method and then
	 * exit.
	 */
	public SwingWorker()
	{
		doConstruct = new Runnable()
		{
			public void run()
			{
				SwingUtilities.invokeLater(doStarted);
				try
				{
					setValue(construct());
				}
				finally
				{
					threadVar.clear();
				}
				SwingUtilities.invokeLater(doFinished);
			}
		};
	}

	/**
	 * Get the value produced by the worker thread, or null if it hasn't been
	 * constructed yet.
	 */
	protected synchronized Object getValue()
	{
		return value;
	}

	/**
	 * Set the value produced by worker thread
	 */
	private synchronized void setValue(Object x)
	{
		value = x;
	}

	/**
	 * Compute the value to be returned by the <code>get</code> method.
	 */
	public abstract Object construct();

	/**
	 * Called on the event dispatching thread (not on the worker thread) after
	 * the <code>construct</code> method has returned.
	 */
	public void finished()
	{
	}

	/**
	 * Called on the event dispatching thread (not on the worker thread) before
	 * the <code>construct</code> method has been run.
	 */
	public void started()
	{
	}

	/**
	 * A new method that interrupts the worker thread. Call this method to force
	 * the worker to abort what it's doing.
	 */
	public void interrupt()
	{
		Thread t = threadVar.get();
		if(t != null)
		{
			t.interrupt();
		}
		threadVar.clear();
	}

	/**
	 * Return the value created by the <code>construct</code> method. Returns
	 * null if either the constructing thread or the current thread was
	 * interrupted before a value was produced.
	 *
	 * @return the value created by the <code>construct</code> method
	 */
	public Object get()
	{
		while(true)
		{
			Thread t = threadVar.get();
			if(t == null)
			{
				return getValue();
			}
			try
			{
				t.join();
			}
			catch(InterruptedException e)
			{
				Thread.currentThread().interrupt(); // propagate
				return null;
			}
		}
	}

	/**
	 *
	 */
	public void startWork()
	{
		Thread t = new Thread(doConstruct);
		threadVar = new ThreadVar(t);
		t.start();
	}

	/**
	 * Class to maintain reference to current worker thread under separate
	 * synchronization control.
	 */
	private static class ThreadVar
	{
		private Thread thread;

		ThreadVar(Thread t)
		{
			thread = t;
		}

		synchronized Thread get()
		{
			return thread;
		}

		synchronized void clear()
		{
			thread = null;
		}
	}
}
