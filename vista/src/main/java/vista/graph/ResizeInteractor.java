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

import java.awt.Component;
import java.awt.Rectangle;
import java.awt.event.ComponentEvent;

/**
 * Handles component resize events and calls Graph.setBounds to redo layout of
 * graph
 * 
 * @see Graph
 * @author Nicky Sandhu (DWR).
 * @version $Id: ResizeInteractor.java,v 1.1 2003/10/02 20:49:07 redwood Exp $
 */
public class ResizeInteractor extends ElementInteractor {
	/**
	 * for debuggin' purposes.
	 */
	public static final boolean DEBUG = false;

	/**
	 * Constructor takes the reference to the graph object
	 */
	public ResizeInteractor(ElementContext eC) {
		_ge = eC.getGraphicElement();
	}

	/**
    *
    */
	public void releaseResources() {
		_ge = null;
	}

	/**
	 * Resets bounds for graph whenever the canvas is resized.
	 */
	public void componentResized(ComponentEvent e) {
		Component comp = e.getComponent();
		ElementContext eC = null;
		if (comp instanceof ElementContext) {
			eC = (ElementContext) comp;
			if (eC.getGraphicElement() != _ge)
				return;
		}

		Rectangle r = (eC != null ? eC.getBounds() : null);
		eC.redoNextPaint();
		eC.paint(eC.getGraphics());
	}

	/**
	 * A reference to the graph canvas
	 */
	protected GraphicElement _ge;
}
