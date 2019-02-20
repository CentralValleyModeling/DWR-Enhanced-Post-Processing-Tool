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
