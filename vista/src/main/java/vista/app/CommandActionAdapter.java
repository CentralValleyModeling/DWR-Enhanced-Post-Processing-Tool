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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import vista.gui.Command;
import vista.gui.VistaException;

/**
 * A action listener which uses Command interface to execute
 * 
 * @author Nicky Sandhu
 * @version $Id: CommandActionAdapter.java,v 1.1.1.1 1998/09/30 03:57:45 nsandhu
 *          Exp $
 */
public class CommandActionAdapter implements ActionListener, Runnable {
	private Command _command;
	private View _view;
	private boolean _execute;
	private boolean _backgroundExecution;

	/**
	 * Exceutes or unexecutes a command depending upon whether execute is true
	 * or false
	 */
	public CommandActionAdapter(Command com, View view, boolean execute,
			boolean backgroundExecution) {
		_command = com;
		_view = view;
		_execute = execute;
		_backgroundExecution = backgroundExecution;
	}

	/**
	 * Invoked when an action occurs.
	 */
	public void actionPerformed(ActionEvent evt) {
		if (_backgroundExecution)
			new Thread(this).start();
		else
			doAction();
	}

	/**
	 * runs in a thread
	 */
	public void run() {
		doAction();
	}

	/**
	 * does the main action of calling the command interface execute or
	 * unexecute method
	 */
	public void doAction() {
		try {
			if (_execute)
				_command.execute();
			else
				_command.unexecute();
			_view.updateView();
		} catch (Exception e) {
			throw new VistaException(e, getClass().getName()
					+ " execution problem");
		}
	}
}
