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
package vista.app.schematic;

/**
 * A particle which has a position and id. The position is defined by the id of
 * the waterbody in which it is and its x,y,z position relative to that
 * waterbody.
 */
public class Particle {
	/**
	 * constructor
	 */
	public Particle(int id, int waterbodyId, float x, float y, float z) {
		setData(id, waterbodyId, x, y, z);
	}

	/**
	 * sets the value of data of this particle
	 */
	public void setData(int id, int waterbodyId, float x, float y, float z) {

		_id = id;
		_wbId = waterbodyId;
		_x = x;
		_y = y;
		_z = z;

	}

	/**
   *
   */
	public int getWaterbodyId() {
		return _wbId;
	}

	/**
   *
   */
	public float getDistanceFromUpNode() {
		return _x;
	}
	
	public String toString(){
		return String.format("Particle: %03d in %d @ (%10.2f,%10.2f,%10.2f)",_id,_wbId,_x,_y,_z);
	}

	/**
	 * Id of this particle
	 */
	public int _id;
	/**
	 * Id of waterbody this particle is in.
	 */
	public int _wbId;
	/**
	 * normalized x distance from upnode of channel
	 */
	public float _x;
	/**
	 * normalized y distance from center of channel
	 */
	public float _y;
	/**
	 * normalized z distance from bottom of channel
	 */
	public float _z;
}
