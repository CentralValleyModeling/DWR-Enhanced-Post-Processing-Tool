/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
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
