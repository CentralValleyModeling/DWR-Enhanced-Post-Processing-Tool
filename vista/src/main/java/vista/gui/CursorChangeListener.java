/*
    Copyright (C) 1996-2000 State of California, Department of 
    Water Resources.

    VISTA : A VISualization Tool and Analyzer. 
	Version 1.0
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
package vista.gui;

import java.awt.Component;
import java.awt.Cursor;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JFrame;

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
