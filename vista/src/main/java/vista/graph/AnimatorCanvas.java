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

import java.awt.Graphics;
import java.awt.Image;
import java.awt.Rectangle;
import java.util.Enumeration;

/**
   *
   */
public class AnimatorCanvas extends GECanvas implements AnimationObserver {
	/**
   *
   */
	public AnimatorCanvas(GraphicElement ge, Animator animator) {
		super(ge);
		_animator = animator;
	}

	/**
   * 
   */
	public void update(AnimationObservable o, Object arg) {
		if (DEBUG)
			System.out.println("Updating graphics");
		update(this.getGraphics());
	}

	// if ( _backgroundImage == null ){
	// Rectangle r = _backgroundElement.getBounds();
	// r.x=0; r.y=0;
	// _backgroundImage = createImage( r.width, r.height );
	// Graphics gi = _backgroundImage.getGraphics();
	// gi.setClip(r);
	// _backgroundElement.draw(gi, r);
	// }

	/**
   * 
   */
	public void paint(Graphics g) {
		Rectangle r = this.getBounds();
		if ((r.width != oldR.width || r.height != oldR.height))
			updateNeeded = true;
		r.x = 0;
		r.y = 0;
		g.setClip(r);
		if (doubleBuffer) {
			if (updateNeeded) {
				_geImage = createImage(r.width, r.height);
				_backgroundImage = createImage(r.width, r.height);
				Graphics gb = _backgroundImage.getGraphics();
				gb.setClip(r);
				_ge.draw(gb, r);
				updateNeeded = false;
				oldR = r;
			}
			Graphics gi = _geImage.getGraphics();
			gi.setClip(r);
			gi.drawImage(_backgroundImage, 0, 0, this);
			// drawNextFrame(gi);
			_ge.setGraphics(gi);
			_ge.animateNext();
			g.drawImage(_geImage, 0, 0, this);
		} else {
			if (updateNeeded) {
				_ge.draw(g, r);
				_ge.setGraphics(g);
				_ge.animateNext();
				// drawNextFrame(g);
			} else {
				_ge.draw(g);
				_ge.setGraphics(g);
				_ge.animateNext();
				// drawNextFrame(g);
			}
		}
	}

	/**
   * 
   */
	protected void drawNextFrame(Graphics gi) {

		if (_ge instanceof GEContainer)
			drawNextFrame((GEContainer) _ge, gi);
		else {
			_ge.setGraphics(gi);
			((Animate) _ge).animateNext();
		}
	}

	/**
   * 
   */
	private void drawNextFrame(GEContainer gec, Graphics gi) {
		for (Enumeration e = gec.getIterator(); e.hasMoreElements();) {
			GraphicElement ge = (GraphicElement) e.nextElement();
			if (ge instanceof Animate) {
				ge.setGraphics(gi);
				((Animate) ge).animateNext();
			} else if (ge instanceof GEContainer) {
				drawNextFrame((GEContainer) ge, gi);
			}
		}
	}

	/**
   *
   */
	public void update(Graphics g) {
		paint(g);
	}

	/**
   * 
   */
	public Animator getAnimator() {
		return _animator;
	}

	/**
   *
   */
	Image _backgroundImage = null;
	/**
   *
   */
	GraphicElement _backgroundElement = null;
	/**
   * 
   */
	private static final boolean DEBUG = false;
	/**
   * 
   */
	protected Animator _animator;
}
