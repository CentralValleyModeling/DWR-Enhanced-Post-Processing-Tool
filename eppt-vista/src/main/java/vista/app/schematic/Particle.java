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
