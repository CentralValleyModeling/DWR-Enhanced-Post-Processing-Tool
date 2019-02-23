/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.gui;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import javax.swing.*;

/**
 * This class changes the cursor of the source of the fired event to WAIT_CURSOR
 * and then changes it back to the old cursor once the operation finishes. To
 * use this class correctly, make sure you call the super.preWork() and
 * super.postWork() in preWork and postWork of its subclasses.
 * 
 * @author Nicky Sandhu
 * @version $Id: CursorChangeListener.java,v 1.2 2000/03/21 18:16:29 nsandhu Exp
 *          $
 */
public abstract class CursorChangeListener implements ActionListener {
	/**
	 * true if you want to use threads and false if you don't. Of course if you
	 * don't some stuff may not work as expected.
	 */
	public static boolean USE_THREADS = true;
	private SwingWorker _worker;
	private Frame _fr;
	private Component _glass, _comp;
	private Cursor _oldCursor;
	private MouseListener _ml = new MouseAdapter() {
		public void mousePressed(MouseEvent e) {
		}
	};

	/**
	 * override this do something before the work
	 */
	public void doPreWork() {
		setCursor();
	}

	/**
    *
    */
	public abstract void doWork();

	/**
	 * override this to something after the work
	 */
	public void doPostWork() {
		unsetCursor();
	}

	/**
	 * an implementation that uses a thread to do pre work task, the main task
	 * and then post work task
	 */
	public final void actionPerformed(ActionEvent evt) {
		if (evt.getSource() instanceof Component)
			_comp = (Component) evt.getSource();
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
    *
    */
	public Component getComponent() {
		return _comp;
	}

	/**
    *
    */
	public void setCursor() {
		_fr = VistaUtils.getFrameForComponent(getComponent());
		if (_fr instanceof JFrame) {
			JFrame jfr = (JFrame) _fr;
			_glass = jfr.getGlassPane();
			_glass.addMouseListener(_ml);
			_oldCursor = jfr.getCursor();
			_glass.setVisible(true);
			_glass.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			_fr.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		}
	}

	/**
    *
    */
	public void unsetCursor() {
		if (_fr instanceof JFrame) {
			_glass.setCursor(_oldCursor);
			_glass.removeMouseListener(_ml);
			_glass.setVisible(false);
			_fr.setCursor(_oldCursor);
		}
	}

	/**
	 * An extension of SwingWorker to call pre work, work and post work methods
	 * in that order
	 * 
	 * @author Nicky Sandhu
	 * @version $Id: CursorChangeListener.java,v 1.2 2000/03/21 18:16:29 nsandhu
	 *          Exp $
	 */
	public class MySwingWorker extends SwingWorker {
		private CursorChangeListener _wal;

		/**
      *
      */
		public MySwingWorker(CursorChangeListener wal) {
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
			try {
				_wal.doWork();
			} catch (Exception e) {
				VistaUtils.displayException(_comp, e);
			}
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
