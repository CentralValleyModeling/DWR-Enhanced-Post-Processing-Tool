/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * A class that uses SwingWorker to do the work in a thread so as to allow
 * updates to the components
 * 
 * @author Nicky Sandhu
 * @version $Id: WorkerActionListener.java,v 1.2 2000/03/21 18:16:32 nsandhu Exp
 *          $
 */
public abstract class WorkerActionListener implements ActionListener {
	/**
	 * true if you want to use threads and false if you don't. Of course if you
	 * don't some stuff may not work as expected.
	 */
	public static boolean USE_THREADS = true;
	private SwingWorker _worker;

	/**
	 * override this do something before the work
	 */
	public abstract void doPreWork();

	/**
	 * override this method to do the actual work...
	 */
	public abstract void doWork();

	/**
	 * override this to something after the work
	 */
	public abstract void doPostWork();

	/**
	 * an implementation that uses a thread to do pre work task, the main task
	 * and then post work task
	 */
	public final void actionPerformed(ActionEvent evt) {
		if (USE_THREADS) {
			if (_worker == null) {
				_worker = new MySwingWorker(this);
			}
			_worker.startWork();
		} else {
			doPreWork();
			doWork();
			doPostWork();
		}
	}

	/**
	 * An extension of SwingWorker to call pre work, work and post work methods
	 * in that order
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: WorkerActionListener.java,v 1.2 2000/03/21 18:16:32 nsandhu
	 *          Exp $
	 */
	public class MySwingWorker extends SwingWorker {
		private WorkerActionListener _wal;

		/**
      *
      */
		public MySwingWorker(WorkerActionListener wal) {
			super();
			_wal = wal;
		}

		/**
      *
      */
		public void started() {
			_wal.doPreWork();
		}

		/**
      *
      */
		public Object construct() {
			_wal.doWork();
			return "";
		}

		/**
      *
      */
		public void finished() {
			_wal.doPostWork();
		}
	}
}
