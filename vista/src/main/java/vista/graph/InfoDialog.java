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
package vista.graph;

import java.awt.Dialog;

/**
 * A dialog for displaying information. This dialog positions itself relative to
 * its parent frame. It also beeps before displaying the dialog.
 * 
 * @author Nicky Sandhu
 * @version $Id: InfoDialog.java,v 1.1 2003/10/02 20:49:02 redwood Exp $
 */
public class InfoDialog extends Dialog {
	/**
	 * Constructor
	 * 
	 * @param parent
	 *            The parent frame to which this dialog belongs
	 * @param title
	 *            The title of this dialog window
	 * @param isModal
	 *            True if this dialog is modal
	 * @param msg
	 *            The message to be displayed in dialog
	 */
	public InfoDialog(java.awt.Frame parent, String title, boolean isModal,
			String msg) {
		super(parent, title, isModal);

		if (msg == null)
			msg = " ";

		java.awt.GridBagConstraints gbc = new java.awt.GridBagConstraints();
		java.awt.GridBagLayout gbl = new java.awt.GridBagLayout();
		setLayout(gbl);

		java.awt.Button OKButton;
		java.awt.Label message;

		message = new java.awt.Label(msg);
		gbc.gridx = 2;
		gbc.gridy = 2;
		gbc.gridwidth = 6;
		gbc.gridheight = 2;
		gbl.setConstraints(message, gbc);
		add(message);

		OKButton = new java.awt.Button(OK_LABEL);
		gbc.gridx = 4;
		gbc.gridy = 6;
		gbc.gridwidth = 2;
		gbc.gridheight = 2;
		gbc.weightx = 1.0;
		gbc.anchor = java.awt.GridBagConstraints.CENTER;
		gbl.setConstraints(OKButton, gbc);
		add(OKButton);

		OKButton.addActionListener(new java.awt.event.ActionListener() {
			public void actionPerformed(java.awt.event.ActionEvent e) {
				((java.awt.Dialog) ((java.awt.Button) e.getSource())
						.getParent()).dispose();
			}
		});

		pack();

		java.awt.Toolkit tk = getToolkit();
		for (int i = 0; i < 3; i++)
			tk.beep();
		java.awt.Rectangle frameBounds = parent.getBounds();
		java.awt.Dimension dialogSize = getSize();
		setLocation(frameBounds.x + frameBounds.width / 2 - dialogSize.width
				/ 2, frameBounds.y + frameBounds.height / 2 - dialogSize.height
				/ 2);
		show();

	}

	/**
	 * Label for OK Button
	 */
	public static final String OK_LABEL = "OK";
}
