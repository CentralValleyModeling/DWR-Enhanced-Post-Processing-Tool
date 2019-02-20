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
package vista.app.schematic;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;

import vista.graph.AnimateElement;
import vista.graph.AnimationObservable;
import vista.graph.GEAttr;
import vista.graph.Scale;

/**
 * The base class for all objects that need to be animated. This class has its
 * update method called by the Animator class. This class implements setBounds
 * and draw methods to render itself on the graphics context
 */
public class ChannelElement extends AnimateElement {
	/**
   * 
   */
	public ChannelElement(GEAttr attr, DSMGridElement grid, ChannelData data) {
		super(attr);
		_grid = grid;
		_data = data;
		System.out.println("_data.getChannelId(): " + _data.getChannelId());
		_channel = (Channel) _grid.getNetwork().getLink(_data.getChannelId());
	}

	/**
    *
    */
	public void Draw() {
	}

	/**
   * 
   */
	public void animateNext() {
		Graphics gc = getGraphics();
		Color previousColor = gc.getColor();
		gc.setColor(getColorForValue(_value));
		System.out.println(_channel.getId() + " " + _value);
		Scale xS = _grid.getGridXScale();
		Scale yS = _grid.getGridYScale();

		Node upnode, downnode;
		upnode = _channel.getNode(Channel.UPNODE_INDEX);
		downnode = _channel.getNode(Channel.DOWNNODE_INDEX);

		_grid.drawThickLine(gc, xS.scaleToUC(upnode.getX()), yS
				.scaleToUC(upnode.getY()), xS.scaleToUC(downnode.getX()), yS
				.scaleToUC(downnode.getY()), channel_width);
		gc.setColor(previousColor);
	}

	/**
	 * The update method is called when the animation frame needs to be updated.
	 * Each element is responsible for drawing itself within the bounds given.
	 */
	public void update(AnimationObservable o, Object arg) {
		_value = _data.getNextValue();
	}

	/**
   *
   */
	public Dimension getPreferredSize() {
		return new Dimension(4, 4);
	}

	/**
   *
   */
	public Dimension getMinimumSize() {
		return getPreferredSize();
	}

	/**
   * 
   */
	protected DSMGridElement _grid;
	/**
   * 
   */
	protected ChannelData _data;
	/**
   * 
   */
	protected Channel _channel;
	/**
   * 
   */
	protected float _value;
	/**
   * 
   */
	private int channel_width = 4;
	/**
   * 
   */
	// Transition Blue -> Green -> Yellow -> Orange -> Red
	private static final float[] ir = { 0, 0, 255, 255, 255 };
	private static final float[] ig = { 0, 255, 255, 165, 0 };
	private static final float[] ib = { 255, 0, 0, 0, 0 };
	private static final float[] fr = { 0, 255, 255, 255, 255 };
	private static final float[] fg = { 255, 255, 165, 0, 0 };
	private static final float[] fb = { 0, 0, 0, 0, 0 };

	/**
   * 
   */
	static Color getColorForValue(float _value) {
		// int ir = 0, ig = 0, ib = 255; // Blue
		// int fr = 255, fg = 255, fb = 0; // yellow
		// int fr = 255, fg = 10, fb = 10; // Dark red
		// int ir = 0, ig = 100, ib = 0; // Dark green

		float[] max = { 100, 500, 2000, 5000, 10000 };
		float[] min = { 50, 100, 500, 2000, 5000 };
		int colorIndex = 0;
		while ((_value > max[colorIndex]) && (colorIndex < max.length - 1)) {
			colorIndex++;
		}
		float range = max[colorIndex] - min[colorIndex];
		if (_value > max[colorIndex])
			_value = max[colorIndex];
		if (_value < min[colorIndex])
			_value = min[colorIndex];
		float normVal = (_value - min[colorIndex]) / range;
		int r = (int) (ir[colorIndex] + normVal
				* (fr[colorIndex] - ir[colorIndex]));
		int g = (int) (ig[colorIndex] + normVal
				* (fg[colorIndex] - ig[colorIndex]));
		int b = (int) (ib[colorIndex] + normVal
				* (fb[colorIndex] - ib[colorIndex]));
		return new Color(r, g, b);
	}
}
