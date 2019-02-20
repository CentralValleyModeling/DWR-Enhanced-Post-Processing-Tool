/*
    Copyright (C) 1996, 1997, 1998 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0beta
	by Nicky Sandhu
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA 95814
    (916)-653-7552
    nsandhu@water.ca.gov

    Send bug reports to nsandhu@water.ca.gov

    This program is licensed to you under the terms of the GNU General
    Public License, version 2, as published by the Free Software
    Foundation.

    You should have received a copy of the GNU General Public License
    along with this program; if not, contact Dr. Francis Chung, below,
    or the Free Software Foundation, 675 Mass Ave, Cambridge, MA
    02139, USA.

    THIS SOFTWARE AND DOCUMENTATION ARE PROVIDED BY THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES AND CONTRIBUTORS "AS IS" AND ANY
    EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
    PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE CALIFORNIA
    DEPARTMENT OF WATER RESOURCES OR ITS CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
    OR SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA OR PROFITS; OR
    BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
    USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
    DAMAGE.

    For more information about VISTA, contact:

    Dr. Francis Chung
    California Dept. of Water Resources
    Division of Planning, Delta Modeling Section
    1416 Ninth Street
    Sacramento, CA  95814
    916-653-5601
    chung@water.ca.gov

    or see our home page: http://wwwdelmod.water.ca.gov/

    Send bug reports to nsandhu@water.ca.gov or call (916)-653-7552

 */
package vista.app;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Frame;
import java.awt.event.MouseAdapter;
import java.util.Stack;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;

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
