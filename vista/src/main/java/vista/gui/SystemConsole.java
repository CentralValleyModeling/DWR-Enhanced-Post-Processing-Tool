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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.FileDialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileWriter;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Writer;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.JViewport;
import javax.swing.WindowConstants;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.JTextComponent;
import javax.swing.text.MutableAttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

/**
 * A display area for console output. This component will display the output of
 * a PrintStream, PrintWriter, or of a running process in a swing text
 * component. The text from the output and error pipes to the child process can
 * be displayed with whatever character attributes desired.
 * 
 * @author Timothy Prinzing
 * @version 1.2 03/04/99
 */
public class SystemConsole extends JFrame {

	/**
	 * Create a console display. By default the text region is set to not be
	 * editable.
	 */
	public SystemConsole() {
		_scrollPane = new JScrollPane();
		outputArea = createOutputArea();
		outputArea.setEditable(false);
		setOutput();
		JViewport vp = _scrollPane.getViewport();
		vp.add(outputArea);
		vp.setBackingStoreEnabled(true);
		//
		JPanel btnPanel = new JPanel();
		JButton clearBtn = new JButton("Clear");
		JButton saveBtn = new JButton("Save");
		JButton quitBtn = new JButton("Quit");
		clearBtn.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				clear();
			}
		});
		saveBtn.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				save();
			}
		});
		quitBtn.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent evt) {
				setVisible(false);
			}
		});
		btnPanel.add(clearBtn);
		btnPanel.add(saveBtn);
		btnPanel.add(quitBtn);
		//
		Container pane = getContentPane();
		pane.setLayout(new BorderLayout());
		pane.add(_scrollPane, BorderLayout.CENTER);
		pane.add(btnPanel, BorderLayout.SOUTH);
		setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
		setSize(400, 600);
	}

	/**
	 * Create the component to be used to display the process output. This is a
	 * hook to allow the component used to be customized.
	 */
	protected JTextComponent createOutputArea() {
		JTextPane pane = new JTextPane();
		return pane;
	}

	/**
	 * Create a PrintStream that will display in the console using the given
	 * attributes.
	 */
	public PrintStream createPrintStream(AttributeSet a) {
		Document doc = outputArea.getDocument();
		OutputStream out = new DocumentOutputStream(doc, a);
		PrintStream pOut = new PrintStream(out);
		return pOut;
	}

	/**
	 * Create a PrintWriter that will display in the console using the given
	 * attributes.
	 */
	public PrintWriter createPrintWriter(AttributeSet a) {
		Document doc = outputArea.getDocument();
		Writer out = new DocumentWriter(doc, a);
		PrintWriter pOut = new PrintWriter(out);
		return pOut;
	}

	/**
	 * Fetch the component used for the output. This allows further parsing of
	 * the output if desired, and allows things like mouse listeners to be
	 * attached. This can be useful for things like compiler output where
	 * clicking on an error warps another view to the location of the error.
	 */
	public JTextComponent getOutputArea() {
		return outputArea;
	}

	/**
	 * Set the attributes to use when displaying the output pipe to the process
	 * being monitored.
	 */
	public void setOutputAttributes(AttributeSet a) {
		outputAttr = a.copyAttributes();
	}

	/**
	 * Get the attributes being used when displaying the output pipe to the
	 * process being monitored.
	 */
	public AttributeSet getOutputAttributes() {
		return outputAttr;
	}

	/**
	 * Set the attributes to use when displaying the error pipe to the process
	 * being monitored.
	 */
	public void setErrorAttributes(AttributeSet a) {
		errorAttr = a.copyAttributes();
	}

	/**
	 * Get the attributes being used when displaying the error pipe to the
	 * process being monitored.
	 */
	public AttributeSet getErrorAttributes() {
		return errorAttr;
	}

	/**
	 * Set output streams System.out and System.err to this console with
	 * different attributes
	 */
	public void setOutput() {
		Document doc = outputArea.getDocument();
		// set the output stream attributes
		setOutputAttributes(new SimpleAttributeSet());
		MutableAttributeSet attr = new SimpleAttributeSet();
		StyleConstants.setForeground(attr, Color.red);
		setErrorAttributes(attr);
		PrintStream pOut = createPrintStream(getOutputAttributes());
		PrintStream pErr = createPrintStream(getErrorAttributes());
		if (orgOut == null) {
			orgOut = System.out;
			orgErr = System.err;
		}
		System.setOut(pOut);
		System.setErr(pErr);
	}

	/**
    *
    */
	public void resetSystemOutput() {
		System.setOut(orgOut);
		System.setErr(orgErr);
	}

	/**
    *
    */
	public void setVisible(boolean b) {
		if (b) {
			orgOut.flush();
			orgErr.flush();
		}
		super.setVisible(b);
	}

	/**
    *
    */
	private void clear() {
		try {
			Document doc = outputArea.getDocument();
			doc.remove(0, doc.getLength());
		} catch (BadLocationException be) {
			be.printStackTrace(System.err);
		}
	}

	/**
    *
    */
	private void save() {
		String saveFile = VistaUtils.getFilenameFromDialog(this,
				FileDialog.SAVE, "txt", "Text File");
		if (saveFile == null)
			return;
		try {
			outputArea.write(new FileWriter(saveFile));
		} catch (Exception e) {
			e.printStackTrace(System.err);
		}
	}

	private JScrollPane _scrollPane;
	private JTextComponent outputArea;
	private AttributeSet outputAttr;
	private AttributeSet errorAttr;
	private PrintStream orgOut, orgErr;
}
