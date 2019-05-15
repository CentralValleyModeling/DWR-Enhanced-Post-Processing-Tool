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

import vista.app.TimeData;

  /**
   * 
   */
public class PTMTimeData implements TimeData {
  /**
   * 
   */
public PTMTimeData(ParticleData particleData){
  super();
  _particleData = particleData;
}
  /**
   * 
   */
public String getNextValue(){
  return _particleData.getDate();
}
  /**
   * 
   */
private ParticleData _particleData;
}
