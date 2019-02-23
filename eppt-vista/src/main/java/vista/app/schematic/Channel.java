/*
 * Copyright (c) 2019
 * California Department of Water Resources
 * All Rights Reserved.  DWR PROPRIETARY/CONFIDENTIAL.
 * Source may not be released without written approval from DWR
 */
package vista.app.schematic;

  /**
   *
   */
class Channel extends Link{
  /**
   *
   */
public static final int UPNODE_INDEX = 0;
  /**
   *
   */
public static final int DOWNNODE_INDEX = 1;
  /**
   *
   */
public Channel ( int id, Node [] nodes, float length){
  super(id, nodes);
  _length = length;
}
  /**
   *
   */
public String toString(){
  StringBuffer buf = new StringBuffer("Channel ");
  String eol = System.getProperty("line.separator");
  buf.append("Id: ").append(getId()).append(" Length: ").append(getLength()).append(eol);
  return buf.toString();
}
  /**
   *
   */
public float getLength(){
  return _length;
}
  /**
   *
   */
protected float _length;
}
