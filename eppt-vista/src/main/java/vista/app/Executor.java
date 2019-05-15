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
package vista.app;

import java.awt.Component;
import java.awt.Cursor;
import java.util.Stack;
import javax.swing.*;

import vista.gui.Command;
import vista.gui.VistaException;
import vista.gui.VistaUtils;

/**
 * Executes the command. It also stores the previous commands executed so that
 * they can be unexecuted. No limit is prescribed for now but that may be a user
 * option in the future.
 * 
 * @author Nicky Sandhu
 * @version $Id: Executor.java,v 1.1 2003/10/02 20:48:29 redwood Exp $
 */
public class Executor {
	private static final class BackgroundExecutor extends
			SwingWorker<Command, Object> {
		private Command com;
		private View view;
		private Cursor oldCursor;

		private BackgroundExecutor(Command com, View view, Cursor oldCursor) {
			this.com = com;
			this.view = view;
			this.oldCursor = oldCursor;
		}

		@Override
		protected Command doInBackground() throws Exception {
			try {
				com.execute();
				_history.push(com);
				view.updateView();
				_historyView.push(view);
			} catch (Exception ex) {
				VistaUtils.displayException((Component) view, ex);
			} finally {
				SwingUtilities.getRootPane((JComponent) view).setCursor(
						oldCursor);
			}
			return com;
		}

		protected void done() {
			try {
				SwingUtilities.getRootPane((JComponent) view).setCursor(
						oldCursor);
			} catch (Exception ignore) {
			}
		}
	}

	/**
	 * history of commands
	 */
	private static Stack _history = new Stack();
	/**
	 * history of view on which commands are executed
	 */
	private static Stack _historyView = new Stack();
	private static Cursor waitCursor = Cursor
			.getPredefinedCursor(Cursor.WAIT_CURSOR);
	private static boolean DO_IN_BACKGROUND = false;

	/**
	 * executes the command
	 */
	public static final void execute(final Command com, final View view) {
		Cursor oldCursor = null;
		try {
			if (view instanceof JComponent) {
				JComponent comp = (JComponent) view;
				oldCursor = SwingUtilities.getRootPane(comp).getCursor();
				SwingUtilities.getRootPane(comp).setCursor(
						Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
			}
			BackgroundExecutor worker = new BackgroundExecutor(com, view,
					oldCursor);
			if (DO_IN_BACKGROUND){
				worker.execute();
			} else {
				worker.doInBackground();
			}
		} catch (Exception e) {
			SwingUtilities.getRootPane((JComponent) view).setCursor(
					oldCursor);
			VistaUtils.displayException(null, e);
		}
	}

	/**
	 * unexecutes the command
	 */
	public static final void unexecute() {
		try {
			Command com = (Command) _history.pop();
			com.unexecute();
			View view = (View) _historyView.pop();
			view.updateView();
		} catch (java.util.EmptyStackException ese) {
			VistaUtils.displayException(null, new VistaException(
					"No more to undo"));
		} catch (Exception e) {
			VistaUtils.displayException(null, e);
		}
	}
}
