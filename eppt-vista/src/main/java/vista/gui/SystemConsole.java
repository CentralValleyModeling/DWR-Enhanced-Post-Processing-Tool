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
import javax.swing.*;
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
