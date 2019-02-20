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

import java.util.Properties;

import vista.gui.VistaUtils;

/**
   *
   */
public class GraphProperties {
	/**
   *
   */
	public static Properties properties;
	static {
		properties = new Properties();
		// load default properties
		properties.put("GraphInfo.timeUnitString", "TIME");

		properties.put("GraphFrame.LOAD_ATTR", "Load Attributes...");
		properties.put("GraphFrame.SAVE_ATTR", "Save Attributes...");

		properties.put("GraphFrame.gifSelectionMsg", "Select Gif File...");
		properties
				.put("GraphFrame.psSelectionMsg", "Select PostScript File...");
		properties.put("GraphFrame.PPMSelectionMsg", "Select PPM File...");
		properties.put("GraphFrame.jpegSelectionMsg", "Select Jpeg File...");
		properties.put("GraphFrame.GIF_FILE", "graph.gif");
		properties.put("GraphFrame.PS_FILE", "graph.ps");
		properties.put("GraphFrame.PPM_FILE", "graph.ppm");

		properties.put("GraphFrame.MAIN_MENU", "MAIN");
		properties.put("GraphFrame.EDIT_MENU", "Edit");
		properties.put("GraphFrame.EDIT_GRAPH", "Graph...");
		properties.put("GraphFrame.PRINT", "Print");
		properties.put("GraphFrame.QUIT", "Quit");
		properties.put("GraphFrame.SAVE2GIF", "Save2Gif");
		properties.put("GraphFrame.SAVE2PS", "Save2PS");
		properties.put("GraphFrame.SAVE2PPM", "Save2PPM");
		properties.put("GraphFrame.SAVE2JPEG", "Save2Jpeg");
		properties.put("DSSPlotBuilder.useTimeTickGenerator", "true");
		properties.put("GraphFrame.graphPropertiesFile", "demo1.properties");
		//
		properties.put("displayGood", "true");
		properties.put("displayQuestionable", "false");
		properties.put("displayReject", "false");
		properties.put("displayUnscreened", "true");
		// load from file
		java.io.InputStream is = null;
		if (is == null)
			is = VistaUtils.getPropertyFileAsStream("graph.properties");
		if (is == null)
			is = VistaUtils
					.getResourceAsStream("/vista/graph/graph.properties");
		try {
			properties.load(new java.io.BufferedInputStream(is));
		} catch (java.io.IOException ioe) {
			System.out.println("Properties file " + "graph.properties"
					+ " not found");
			System.out.println("using default properties");
		}
	}
}
