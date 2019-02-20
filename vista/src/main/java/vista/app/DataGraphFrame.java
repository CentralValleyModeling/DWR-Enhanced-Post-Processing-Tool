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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FileDialog;
import java.awt.Graphics;
import java.awt.PrintJob;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.util.Properties;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JToolBar;

import vista.graph.AttributeSerializer;
import vista.graph.CoordinateDisplayInteractor;
import vista.graph.Curve;
import vista.graph.CurveDataModel;
import vista.graph.FontResizeInteractor;
import vista.graph.GECanvas;
import vista.graph.GEContainer;
import vista.graph.GETreeDialog;
import vista.graph.Graph;
import vista.graph.GraphFrameInterface;
import vista.graph.GraphProperties;
import vista.graph.GraphUtils;
import vista.graph.GraphicElement;
import vista.graph.ImageSerializer;
import vista.graph.Plot;
import vista.graph.PrintPreviewer;
import vista.graph.ZoomInteractor;
import vista.gui.VistaUtils;
import vista.set.DataReference;
import vista.set.DefaultReference;
import vista.time.TimeWindow;

/**
 * This class constructs a frame and provides the context with which to interact
 * with the Graph object. The Graph object itself is contained with the
 * GraphCanvas object
 * 
 * @see Graph
 * @see HEC.DSS.GraphCanvas
 * @author Nicky Sandhu
 * @version $Id: DataGraph.java,v 1.1 2003/10/02 20:48:25 redwood Exp $
 */
public class DataGraphFrame extends JFrame{
	/**
	 * for debuggin'
	 */
	public static boolean DEBUG = false;
	private DataSetGraph dataGraph;

	/**
	 * shows graph in a frame with frame title and shows it if isVisible is true
	 * 
	 * @wbp.parser.constructor
	 */
	public DataGraphFrame(Graph graph, String frameTitle, boolean isVisible) {
		init(graph, isVisible, frameTitle);
	}

	/**
	 * Constructor
	 */
	public DataGraphFrame(Graph graph, boolean isVisible) {
		init(graph, isVisible, "");
	}

	/**
	 * Constructor
	 */
	public DataGraphFrame(Graph graph, String frameTitle) {
		init(graph, true, frameTitle);
	}

	/**
    *
    */
	public void cleanup() {
		if (DEBUG)
			System.out.println("Disposing of data graph");
		getContentPane().removeAll();
		dataGraph.cleanUp();
		if (DEBUG)
			System.out.println("Removed all components");
		if (DEBUG)
			System.out.println("Set _gC to null");
	}

	/**
   *
   */
	private void init(Graph graph, boolean isVisible, String frameTitle) {
		setTitle(frameTitle);
		setIconImage(Toolkit.getDefaultToolkit().createImage(
				VistaUtils.getImageAsBytes("/vista/planning.gif")));
		dataGraph = new DataSetGraph(graph);
		// add graph canvas to frame and set its listeners
		this.getContentPane().setLayout(new BorderLayout());
		this.getContentPane().add(dataGraph, BorderLayout.CENTER);
		dataGraph.addZoomControlBar(this);
		// set curve filters for the graph
		AppUtils.setCurveFilter(graph, AppUtils.getCurrentCurveFilter());
		// set up menus and their listeners
		JMenuBar mb = new JMenuBar();
		JMenu mainMenu = dataGraph.getMainMenu();
		JMenuItem quitItem = new JMenuItem("Quit");
		quitItem.addActionListener(new QuitListener());
		mainMenu.add(quitItem);
		JMenu displayMenu = dataGraph.getDisplayMenu();
		mb.add(mainMenu);
		mb.add(displayMenu);
		setJMenuBar(mb);
		//
		this.addWindowListener(new QuitListener());
		// position frame and show
		this.pack();
		Toolkit tk = getToolkit();
		Dimension screenSize = tk.getScreenSize();
		Dimension frameSize = getSize();
		this.setLocation(screenSize.width - frameSize.width, screenSize.height
				- frameSize.height);
		this.setVisible(isVisible);
		//this.repaint();
	}


	/**
	 * quits on quit command
	 */
	protected class QuitListener extends WindowAdapter implements
			ActionListener {
		public void actionPerformed(ActionEvent e) {
			if (DEBUG)
				System.out.println("Quit event");
			dispose();
			cleanup();
		}

		public void windowClosed(WindowEvent evt) {
			if (DEBUG)
				System.out.println("Window closed event");
			cleanup();
		}

		public void windowClosing(WindowEvent evt) {
			if (DEBUG)
				System.out.println("Window closing event");
			cleanup();
		}
	}

}
