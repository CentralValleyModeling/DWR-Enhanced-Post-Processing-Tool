#include <stdio.h>
#include <string.h>
#include "vista_db_dss_DSSDataReader.h"
#include "dssLocal.h"
/*
 * return a character array of LEN1 with blank elements after null 
 * element in string. This makes it compatible with fortran strings
 */
char * cleanCharacterArray(char* array, int length){
  int index = 0;
  while( index < length && array[index] != '\0') index ++;
  index++;
  while( index < length ) {
    array[index] = ' ';
    index ++;
  }
  return array;
}
/*
 * sets the marker at the end of the last non - blank character
 * or at the last element of the array if no last non-blank
 */
void setEndOfStringMarker(char* array){
  int index=0;
  int nonBlank=0;
  int lastNonBlank = -1;
  for(index=0;index<LEN1; index++){
    if ( array[index] != ' ' ) nonBlank = 1;
    if ( array[index] == ' ' && nonBlank == 1 ) {
      lastNonBlank = index;
      nonBlank = 0;
    }
  }
  if ( lastNonBlank == -1 )
    array[LEN1-1] = '\0';
  else
    array[lastNonBlank] = '\0';
}
/*
 * Class:     vista_db_dss_DSSDataReader
 * Method:    generateCatalog
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_vista_db_dss_DSSDataReader_generateCatalog
  (JNIEnv *env, jobject thisObject, jstring dssFile){
  // filename
  char localFilename[LEN2] = "";
  const char* lFilename = env->GetStringUTFChars(dssFile, JNI_FALSE);
  int filenameLength = (int) env->GetStringUTFLength(dssFile);
  if ( filenameLength > LEN2 ) {
    printf("%s\n", "Error: filename length > LEN2");
  }
  strcpy(localFilename, lFilename);
  //  cleanCharacterArray(localFilename, LEN2);
  generateCatalog(localFilename, strlen(localFilename) );
  env->ReleaseStringUTFChars(dssFile, lFilename);
}
/*
 * Class:     vista_db_dss_DSSDataReader
 * Method:    getRecordType
 * Signature: (Ljava/lang/String;Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_vista_db_dss_DSSDataReader_getRecordType
(JNIEnv *env , jobject thisObject, jstring dssFile, jstring pathname){
  // filename
  char localFilename[LEN1] = "";
  const char* lFilename = env->GetStringUTFChars(dssFile, JNI_FALSE);
  int filenameLength = (int) env->GetStringUTFLength(dssFile);
  if ( filenameLength > LEN1 ) {
    printf("%s\n", "Error: filename length > LEN1");
  }
  strcpy(localFilename, lFilename);
  env->ReleaseStringUTFChars(dssFile, lFilename);
  // pathname
  char localPathname[LEN1] = "";
  const char* lPathname = env->GetStringUTFChars(pathname, JNI_FALSE);
  int pathnameLength = (int) env->GetStringUTFLength(pathname);
  if ( pathnameLength > LEN1 ) {
    printf("%s\n", "Error: pathname length > LEN1");
  }
  strcpy(localPathname, lPathname);
  env->ReleaseStringUTFChars(pathname, lPathname);
#ifdef __sun
  return getRecordType(localFilename, localPathname, 
		       strlen(localFilename), strlen(localPathname));
#else
  return getRecordType(localFilename, strlen(localFilename), 
		       localPathname, strlen(localPathname));
#endif
}

/*
 * Class:     vista_db_dss_DSSDataReader
 * Method:    retrieveRegularTimeSeries
 * Signature: (Ljava/lang/String;Ljava/lang/String;IIZLDWR/Data/DSS/DSSDataReader$TimeSeriesData;)I
 */
JNIEXPORT jint JNICALL Java_vista_db_dss_DSSDataReader_retrieveRegularTimeSeries
(JNIEnv *env, jobject thisObject, 
 jstring dssFile, jstring pathname, 
 jlong startJulmin, jlong endJulmin, jboolean retrieveFlags, 
 jobject data){
  // 
  int numberValues;
  double * values;
  float * tmpValues;
  float * flags, *header;
  // filename 
  char localFilename[LEN1] = "";
  const char* lFilename = env->GetStringUTFChars(dssFile, JNI_FALSE);
  int filenameLength  = (int) env->GetStringUTFLength(dssFile);
  strcpy(localFilename, lFilename);
  env->ReleaseStringUTFChars(dssFile, lFilename);
  // pathname string 
  char localPathname[LEN1] = "";
  const char* lPathname = env->GetStringUTFChars(pathname, JNI_FALSE);
  int pathnameLength = (int) env->GetStringUTFLength(pathname);
  strcpy(localPathname, lPathname);
  env->ReleaseStringUTFChars(pathname, lPathname);
  // start and end time
  long int startTime = (long int) startJulmin;
  long int endTime = (long int) endJulmin;
  // flag retrival
  //  printf("Inside retrieve regular time series: java part\n");
  int getFlags = (int) retrieveFlags;
  //
  char units[LEN1], type[LEN1] = "";
  //
  int offset = 0;
  //
  numberValues = getNumberOfValuesInInterval(startTime,endTime,localPathname, strlen(localPathname));
  if ( numberValues <= 0 ) return -1;
  // printf("Number of values: %d", numberValues);
  tmpValues = new float[numberValues];
  if ( getFlags != 0 ) flags = new float[numberValues];
  header = new float[80];
#ifdef __sun
  retrieveRegularTimeSeries(localFilename, 
			    localPathname, 
			    startTime, endTime, getFlags,
			    numberValues, tmpValues, flags, header,
			    units, type, offset,
			    strlen(localFilename), strlen(localPathname), 
			    LEN1, LEN1
			    );
#else
  retrieveRegularTimeSeries(localFilename, strlen(localFilename),
			    localPathname, strlen(localPathname),
			    startTime, endTime, getFlags,
			    numberValues, tmpValues, flags, header,
			    units, LEN1, type, LEN1, offset
			    );
#endif
  if ( numberValues <= 0 ) {
    delete [] tmpValues;
    delete [] flags;
    delete [] header;
    return -1;
  }
  values = new double[numberValues];
  int i=0;
  for(i=0; i < numberValues; i++){
    values[i] = tmpValues[i];
  }
  // fill the dss data object with these values...
  // type signatures [ -> array, C (char), I (int), F(float)
  jclass dataClass = env->GetObjectClass(data);
  jfieldID xValuesId = env->GetFieldID(dataClass, "_xValues","[D");
  jfieldID yValuesId = env->GetFieldID(dataClass, "_yValues","[D");
  jfieldID flagsId = env->GetFieldID(dataClass, "_flags","[I");
  jfieldID numberReadId = env->GetFieldID(dataClass, "_numberRead","I");
  jfieldID xTypeId = env->GetFieldID(dataClass, "_xType","Ljava/lang/String;");
  jfieldID xUnitsId = env->GetFieldID(dataClass, "_xUnits","Ljava/lang/String;");
  jfieldID yTypeId = env->GetFieldID(dataClass, "_yType","Ljava/lang/String;");
  jfieldID yUnitsId = env->GetFieldID(dataClass, "_yUnits","Ljava/lang/String;");
  jfieldID offsetId = env->GetFieldID(dataClass, "_offset","I");
  // set everything...
  jsize alength = (jsize) numberValues;
  // set number read 
  env->SetIntField(data,numberReadId, alength);
  // set offset value
  env->SetIntField(data, offsetId, offset);
  // set y values array to values
  jdoubleArray valueArray = env->NewDoubleArray( alength );
  env->SetDoubleArrayRegion(valueArray, 0, alength, (jdouble*) values);
  env->SetObjectField(data, yValuesId, valueArray);
  // set flag array to values
  if ( getFlags != 0 ){
    jintArray flagsArray = env->NewIntArray( alength );
    env->SetIntArrayRegion(flagsArray, 0, alength, (jint*) flags);
    env->SetObjectField(data, flagsId, flagsArray);
  }
  // set units and type
  setEndOfStringMarker(units);
  setEndOfStringMarker(type);
  jstring unitString = env->NewStringUTF(units);
  env->SetObjectField(data, yUnitsId, unitString);
  jstring typeString = env->NewStringUTF(type);
  env->SetObjectField(data, yTypeId, typeString);
  // clean up
  delete [] values;
  delete [] tmpValues;
  if ( getFlags != 0 ) delete [] flags;
  delete [] header;
  return 0;
}

/*
 * Class:     vista_db_dss_DSSDataReader
 * Method:    retrieveIrregularTimeSeries
 * Signature: (Ljava/lang/String;Ljava/lang/String;IIZLDWR/Data/DSS/DSSDataReader$IrregularTimeSeriesData;)I
 */
JNIEXPORT jint JNICALL Java_vista_db_dss_DSSDataReader_retrieveIrregularTimeSeries
(JNIEnv *env, jobject thisObject, 
 jstring dssFile, jstring pathname, 
 jlong startJulmin, jlong endJulmin, jboolean retrieveFlags, 
 jobject data){
  // 
  int numberValues;
  double *xValues, *yValues;
  int * timeValues;
  float * values;
  float *flags, *header;
  // filename 
  char localFilename[LEN1] = "";
  const char* lFilename = env->GetStringUTFChars(dssFile, JNI_FALSE);
  int filenameLength  = (int) env->GetStringUTFLength(dssFile);
  strcpy(localFilename, lFilename);
  env->ReleaseStringUTFChars(dssFile, lFilename);
  // pathname string 
  char localPathname[LEN1] = "";
  const char* lPathname = env->GetStringUTFChars(pathname, JNI_FALSE);
  int pathnameLength = (int) env->GetStringUTFLength(pathname);
  strcpy(localPathname, lPathname);
  env->ReleaseStringUTFChars(pathname, lPathname);
  // start and end time
  long int startTime = (long int) startJulmin;
  long int endTime = (long int) endJulmin;
  // flag retrival
  int getFlags = (int) retrieveFlags;
  //
  char units[LEN1], type[LEN1] = "";
  // guess at max values. resize arrays appropriately later...
  int maxvals = 500000; // half a million for now....?? bad design... need more info from hec
  timeValues = new int[maxvals];
  values = new float[maxvals];
  header = new float[80];
  if ( getFlags != 0 )
    flags = new float[maxvals];
  // retrieve irregular time series data
#ifdef __sun
  retrieveIrregularTimeSeries(localFilename, 
			      localPathname, 
			      startTime, endTime, getFlags,
			      numberValues, 
			      timeValues, values, flags, header,
			      units, type, 
			      strlen(localFilename), strlen(localPathname), LEN1, LEN1
			      );
#else
  retrieveIrregularTimeSeries(localFilename, strlen(localFilename),
			      localPathname, strlen(localPathname),
			      startTime, endTime, getFlags,
			      numberValues, 
			      timeValues, values, flags, header,
			      units, LEN1, type, LEN1
			      );
#endif
  if ( numberValues <= 0 ) {
    delete [] timeValues;
    delete [] yValues;
    if ( getFlags != 0 ) delete [] flags;
    return -1;
  }
  // fill the dss data object with these values...
  xValues = new double[numberValues];
  yValues = new double[numberValues];
  for(int i=0; i < numberValues; i++){
    xValues[i] = timeValues[i];
    yValues[i] = values[i];
  }
  // type signatures [ -> array, C (char), I (int), F(float)
  jclass dataClass = env->GetObjectClass(data);
  jfieldID xValuesId = env->GetFieldID(dataClass, "_xValues","[D");
  jfieldID yValuesId = env->GetFieldID(dataClass, "_yValues","[D");
  jfieldID flagsId = env->GetFieldID(dataClass, "_flags","[I");
  jfieldID numberReadId = env->GetFieldID(dataClass, "_numberRead","I");
  jfieldID xTypeId = env->GetFieldID(dataClass, "_xType","Ljava/lang/String;");
  jfieldID xUnitsId = env->GetFieldID(dataClass, "_xUnits","Ljava/lang/String;");
  jfieldID yTypeId = env->GetFieldID(dataClass, "_yType","Ljava/lang/String;");
  jfieldID yUnitsId = env->GetFieldID(dataClass, "_yUnits","Ljava/lang/String;");
  // set everything...
  jsize alength = (jsize) numberValues;
  // set number read 
  env->SetIntField(data,numberReadId, alength);
  // set x values array to time values
  jdoubleArray xValueArray = env->NewDoubleArray( alength );
  env->SetDoubleArrayRegion(xValueArray, 0, alength, (jdouble*) xValues);
  env->SetObjectField(data, xValuesId, xValueArray);
  // set y values array to values
  jdoubleArray valueArray = env->NewDoubleArray( alength );
  env->SetDoubleArrayRegion(valueArray, 0, alength, (jdouble*) yValues);
  env->SetObjectField(data, yValuesId, valueArray);
  // set flag array to values
  if ( getFlags != 0 ){
    jintArray flagsArray = env->NewIntArray( alength );
    env->SetIntArrayRegion(flagsArray, 0, alength, (jint*) flags);
    env->SetObjectField(data, flagsId, flagsArray);
  }
  // set units and type
  setEndOfStringMarker(units);
  setEndOfStringMarker(type);
  jstring unitString = env->NewStringUTF(units);
  env->SetObjectField(data, yUnitsId, unitString);
  jstring typeString = env->NewStringUTF(type);
  env->SetObjectField(data, yTypeId, typeString);
  // clean up
  delete [] timeValues;
  delete [] values;
  delete [] xValues;
  delete [] yValues;
  if ( getFlags != 0 ) delete [] flags;
  return 0;
}

/*
 * Class:     vista_db_dss_DSSDataReader
 * Method:    retrievePairedData
 * Signature: (Ljava/lang/String;Ljava/lang/String;LDWR/Data/DSS/DSSDataReader$PairedData;)I
 */
JNIEXPORT jint JNICALL Java_vista_db_dss_DSSDataReader_retrievePairedData
(JNIEnv *env, jobject thisObject, jstring dssFile, jstring pathname, jobject data){
  int numberValues;
  double *xValues, *yValues;
  float * values;
  float * header;
  // filename 
  char localFilename[LEN1] = "";
  const char* lFilename = env->GetStringUTFChars(dssFile, JNI_FALSE);
  int filenameLength  = (int) env->GetStringUTFLength(dssFile);
  strcpy(localFilename, lFilename);
  env->ReleaseStringUTFChars(dssFile, lFilename);
  // pathname string 
  char localPathname[LEN1] = "";
  const char* lPathname = env->GetStringUTFChars(pathname, JNI_FALSE);
  int pathnameLength = (int) env->GetStringUTFLength(pathname);
  strcpy(localPathname, lPathname);
  env->ReleaseStringUTFChars(pathname, lPathname);
  //
  char xUnits[LEN1] = "", xType[LEN1] = "", yUnits[LEN1] = "", yType[LEN1] = "";
  // a wild guess at max number of points
  int maxvals = 500000;
  values = new float[maxvals];
  header = new float[80];
  // retrieve paired data 
#ifdef __sun
  retrievePairedData(localFilename, 
		     localPathname, 
		     numberValues, 
		     values, header,
		     xUnits, xType, yUnits, yType, 
		     strlen(localFilename), strlen(localPathname), 
		     LEN1, LEN1, LEN1, LEN1 
		     );
#else
  retrievePairedData(localFilename, strlen(localFilename),
		     localPathname, strlen(localPathname), 
		     numberValues, 
		     values, header,
		     xUnits, LEN1, xType, LEN1, yUnits, LEN1, yType, LEN1
		     );
#endif
  if ( numberValues <= 0 ) {
    delete [] values;
    delete [] header;
    return -1;
  }
  //
  xValues = new double[numberValues];
  yValues = new double[numberValues];
  for(int i=0; i < numberValues; i++){
    xValues[i] = values[i];
    yValues[i] = values[i+numberValues];
  }
  // fill the dss data object with these values...
  // type signatures [ -> array, C (char), I (int), F(float)
  jclass dataClass = env->GetObjectClass(data);
  jfieldID xValuesId = env->GetFieldID(dataClass, "_xValues","[D");
  jfieldID yValuesId = env->GetFieldID(dataClass, "_yValues","[D");
  jfieldID flagsId = env->GetFieldID(dataClass, "_flags","[I");
  jfieldID numberReadId = env->GetFieldID(dataClass, "_numberRead","I");
  jfieldID xTypeId = env->GetFieldID(dataClass, "_xType","Ljava/lang/String;");
  jfieldID xUnitsId = env->GetFieldID(dataClass, "_xUnits","Ljava/lang/String;");
  jfieldID yTypeId = env->GetFieldID(dataClass, "_yType","Ljava/lang/String;");
  jfieldID yUnitsId = env->GetFieldID(dataClass, "_yUnits","Ljava/lang/String;");
  // set everything...
  jsize alength = (jsize) numberValues;
  // set number read 
  env->SetIntField(data,numberReadId, alength);
  // set x values array to xValues
  jdoubleArray xValueArray = env->NewDoubleArray( alength );
  env->SetDoubleArrayRegion(xValueArray, 0, alength, (jdouble*) xValues);
  env->SetObjectField(data, xValuesId, xValueArray);
  // set y values array to yValues
  jdoubleArray yValueArray = env->NewDoubleArray( alength );
  env->SetDoubleArrayRegion(yValueArray, 0, alength, (jdouble*) yValues);
  env->SetObjectField(data, yValuesId, yValueArray);
  // set units and type
  setEndOfStringMarker(xUnits);
  setEndOfStringMarker(yUnits);
  setEndOfStringMarker(xType);
  setEndOfStringMarker(yType);
  //
  jstring xUnitString = env->NewStringUTF(xUnits);
  env->SetObjectField(data, xUnitsId, xUnitString);
  jstring xTypeString = env->NewStringUTF(xType);
  env->SetObjectField(data, xTypeId, xTypeString);
  jstring yUnitString = env->NewStringUTF(yUnits);
  env->SetObjectField(data, yUnitsId, yUnitString);
  jstring yTypeString = env->NewStringUTF(yType);
  env->SetObjectField(data, yTypeId, yTypeString);
  // clean up
  delete [] values;
  delete [] xValues;
  delete [] yValues;
  delete [] header;
  //
  return 0;
}

