#include <stdio.h>
#include <string.h>
#include "vista_db_dss_DSSDataWriter.h"
#include "dssLocal.h"
#include <math.h>
/*
 * Class:     vista_db_dss_DSSDataWriter
 * Method:    storeTimeSeriesData
 * Signature: (Ljava/lang/String;Ljava/lang/String;JJLDWR/Data/DSS/DSSData;Z)V
 */
JNIEXPORT void JNICALL Java_vista_db_dss_DSSDataWriter_storeTimeSeriesData
(JNIEnv *env, jobject thisObject, 
 jstring dssFile, jstring pathname, 
 jlong startJulmin, jlong endJulmin, 
 jobject data, jboolean storeFlags){
  // 
  int numberValues;
  double * values;
  float * tmpValues;
  float *flags;
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
  int setFlags = (int) storeFlags;
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
  // get y values array to values
  jdoubleArray valueArray = 
    (jdoubleArray) env->GetObjectField(data, yValuesId);
  jsize alength = env->GetArrayLength(valueArray);
  if ( alength <= 0 ) return;
  values = new double[alength];
  env->GetDoubleArrayRegion(valueArray, 0, alength, (jdouble*) values);
  tmpValues = new float[alength];
  for(int i=0; i < alength; i++) tmpValues[i] = values[i];
  delete [] values;
  // get flag array to values
  //  printf("Inside retrieve regular time series: java part\n");
  if ( setFlags != 0 ) {
    jintArray flagsArray = (jintArray) env->GetObjectField(data, flagsId);
    alength = env->GetArrayLength(flagsArray);
    flags = new float[alength];
    env->GetIntArrayRegion(flagsArray, 0, alength, (jint*) flags);
  }
  // get units
  jstring unitString = (jstring) env->GetObjectField(data, yUnitsId);
  char yUnit[LEN1] = "";
  const char* lUnit = env->GetStringUTFChars(unitString, JNI_FALSE);
  int lUnitLen  = (int) env->GetStringUTFLength(unitString);
  strcpy(yUnit, lUnit);
  env->ReleaseStringUTFChars(unitString, lUnit);
  // get type
  jstring typeString = (jstring) env->GetObjectField(data, yTypeId);
  char yType[LEN1] = "";
  const char* lType = env->GetStringUTFChars(typeString, JNI_FALSE);
  int lTypeLen  = (int) env->GetStringUTFLength(typeString);
  strcpy(yType, lType);
  env->ReleaseStringUTFChars(typeString, lType);
  // clean all character arrays
  cleanCharacterArray(localFilename, LEN1);
  cleanCharacterArray(localPathname, LEN1);
  cleanCharacterArray(yUnit, LEN1);
  cleanCharacterArray(yType, LEN1);
  //
  // store data in dss
#ifdef __sun
  storeRegularTimeSeries( localFilename, 
			  localPathname, 
			  startTime, endTime, setFlags,
			  tmpValues, flags, 
			  yUnit, yType, 
			  strlen(localFilename),strlen(localPathname), 
			  strlen(yUnit), strlen(yType) );
#else
  storeRegularTimeSeries( localFilename, strlen(localFilename),
			  localPathname, strlen(localPathname),
			  startTime, endTime, setFlags,
			  tmpValues, flags, 
			  yUnit, strlen(yUnit), yType, strlen(yType)
			  );
#endif
  if ( setFlags != 0 ) delete [] flags;
  delete [] tmpValues;
}

/*
 * Class:     vista_db_dss_DSSDataWriter
 * Method:    storeIrregularTimeSeriesData
 * Signature: (Ljava/lang/String;Ljava/lang/String;JJLDWR/Data/DSS/DSSData;Z)V
 */
JNIEXPORT void JNICALL Java_vista_db_dss_DSSDataWriter_storeIrregularTimeSeriesData
(JNIEnv *env, jobject thisObject, 
 jstring dssFile, jstring pathname, 
 jlong startJulmin, jlong endJulmin, 
 jobject data, jboolean storeFlags){
  // 
  int numberValues;
  double * xValues, *yValues;
  int * timeValues;
  float * values;
  float *flags;
  int i;
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
  int setFlags = (int) storeFlags;
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
  // get number of values
  numberValues = env->GetIntField(data, numberReadId);
  if ( numberValues <= 0 ) return;
  // get x values array to values
  jdoubleArray xValueArray = 
    (jdoubleArray) env->GetObjectField(data, xValuesId);
  jsize alength = env->GetArrayLength(xValueArray);
  xValues = new double[alength];
  env->GetDoubleArrayRegion(xValueArray, 0, alength, (jdouble*) xValues);
  timeValues = new int[alength];
  for(i=0; i < alength; i++) timeValues[i] = (int) xValues[i];
  delete [] xValues;
  // get y values array to values
  jdoubleArray yValueArray = 
    (jdoubleArray) env->GetObjectField(data, yValuesId);
  alength = env->GetArrayLength(yValueArray);
  yValues = new double[alength];
  env->GetDoubleArrayRegion(yValueArray, 0, alength, (jdouble*) yValues);
  values = new float[alength];
  for(i=0; i < alength; i++) values[i] = yValues[i];
  delete [] yValues;
  // get flag array to values
  //  printf("Inside retrieve regular time series: java part\n");
  if ( setFlags != 0 ) {
    jintArray flagsArray = (jintArray) env->GetObjectField(data, flagsId);
    alength = env->GetArrayLength(flagsArray);
    flags = new float[alength];
    env->GetIntArrayRegion(flagsArray, 0, alength, (jint*) flags);
  }
  // get y units
  jstring yUnitString = (jstring) env->GetObjectField(data, yUnitsId);
  char yUnit[LEN1] = "";
  const char* lyUnit = env->GetStringUTFChars(yUnitString, JNI_FALSE);
  int lyUnitLen  = (int) env->GetStringUTFLength(yUnitString);
  strcpy(yUnit, lyUnit);
  env->ReleaseStringUTFChars(yUnitString, lyUnit);
  // get y type
  jstring typeString = (jstring) env->GetObjectField(data, yTypeId);
  char yType[LEN1] = "";
  const char* lType = env->GetStringUTFChars(typeString, JNI_FALSE);
  int lTypeLen  = (int) env->GetStringUTFLength(typeString);
  strcpy(yType, lType);
  env->ReleaseStringUTFChars(typeString, lType);
  // clean all character arrays
  cleanCharacterArray(localFilename, LEN1);
  cleanCharacterArray(localPathname, LEN1);
  cleanCharacterArray(yUnit, LEN1);
  cleanCharacterArray(yType, LEN1);
  //
  // store data in dss
#ifdef __sun
  storeIrregularTimeSeries( localFilename, 
			    localPathname, 
			    startTime, endTime,
			    setFlags, numberValues,
			    timeValues, values, flags, 
			    yUnit, yType, 
			    strlen(localFilename),strlen(localPathname), 
			    strlen(yUnit), strlen(yType)
			    );
#else
  storeIrregularTimeSeries( localFilename, strlen(localFilename),
			    localPathname, strlen(localPathname),
			    startTime, endTime,
			    setFlags, numberValues,
			    timeValues, values, flags, 
			    yUnit, strlen(yUnit), yType, strlen(yType)
			    );
#endif
  delete [] timeValues;
  delete [] values;
  if ( setFlags != 0 ) delete [] flags;
}

/*
 * Class:     vista_db_dss_DSSDataWriter
 * Method:    storePairedData
 * Signature: (Ljava/lang/String;Ljava/lang/String;LDWR/Data/DSS/DSSData;Z)V
 */
JNIEXPORT void JNICALL Java_vista_db_dss_DSSDataWriter_storePairedData
(JNIEnv *env, jobject thisObject,
 jstring dssFile, jstring pathname, 
 jobject data, jboolean storeFlags){
  // 
  int numberValues;
  double *xValues, *yValues;
  float * values;
  int i;
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
  
  // get number of values
  numberValues = env->GetIntField(data, numberReadId);
  if ( numberValues <= 0 ) return ;
  //
  values = new float[numberValues*2];
  // get x values array to values
  jdoubleArray xValueArray = 
    (jdoubleArray) env->GetObjectField(data, xValuesId);
  jsize alength = env->GetArrayLength(xValueArray);
  xValues = new double[alength];
  env->GetDoubleArrayRegion(xValueArray, 0, alength, (jdouble*) xValues);
  for(i=0; i < alength; i++) values[i] = xValues[i];
  delete [] xValues;
  // get y values array to values
  jdoubleArray yValueArray = 
    (jdoubleArray) env->GetObjectField(data, yValuesId);
  alength = env->GetArrayLength(yValueArray);
  yValues = new double[alength];
  env->GetDoubleArrayRegion(yValueArray, 0, alength, (jdouble*) yValues);
  for(i=0; i < alength; i++) values[i+alength] = yValues[i];
  delete [] yValues;
  // get x units
  jstring xUnitString = (jstring) env->GetObjectField(data, xUnitsId);
  char xUnit[LEN1] = "";
  const char* lxUnit = env->GetStringUTFChars(xUnitString, JNI_FALSE);
  int lxUnitLen  = (int) env->GetStringUTFLength(xUnitString);
  strcpy(xUnit, lxUnit);
  env->ReleaseStringUTFChars(xUnitString, lxUnit);
  // get y units
  jstring yUnitString = (jstring) env->GetObjectField(data, yUnitsId);
  char yUnit[LEN1] = "";
  const char* lyUnit = env->GetStringUTFChars(yUnitString, JNI_FALSE);
  int lyUnitLen  = (int) env->GetStringUTFLength(yUnitString);
  strcpy(yUnit, lyUnit);
  env->ReleaseStringUTFChars(yUnitString, lyUnit);
  // get x type
  jstring xTypeString = (jstring) env->GetObjectField(data, xTypeId);
  char xType[LEN1] = "";
  const char* lxType = env->GetStringUTFChars(xTypeString, JNI_FALSE);
  int lxTypeLen  = (int) env->GetStringUTFLength(xTypeString);
  strcpy(xType, lxType);
  env->ReleaseStringUTFChars(xTypeString, lxType);
  // get y type
  jstring yTypeString = (jstring) env->GetObjectField(data, yTypeId);
  char yType[LEN1] = "";
  const char* lyType = env->GetStringUTFChars(yTypeString, JNI_FALSE);
  int lyTypeLen  = (int) env->GetStringUTFLength(yTypeString);
  strcpy(yType, lyType);
  env->ReleaseStringUTFChars(yTypeString, lyType);
  // clean all character arrays
  cleanCharacterArray(localFilename, LEN1);
  cleanCharacterArray(localPathname, LEN1);
  cleanCharacterArray(xUnit, LEN1);
  cleanCharacterArray(xType, LEN1);
  cleanCharacterArray(yUnit, LEN1);
  cleanCharacterArray(yType, LEN1);
  //
  // store data in dss
#ifdef __sun
  storePairedData( localFilename, 
		   localPathname, 
		   numberValues, 
		   values,
		   xUnit, xType, 
		   yUnit, yType, 
		   strlen(localFilename),strlen(localPathname), 
		   strlen(xUnit), strlen(xType), 
		   strlen(yUnit), strlen(yType)
		   );
#else
  storePairedData( localFilename, strlen(localFilename),
		   localPathname, strlen(localPathname),
		   numberValues, 
		   values,
		   xUnit, strlen(xUnit), xType, strlen(xType),
		   yUnit, strlen(yUnit), yType, strlen(yType)
		   );
#endif
  delete [] values;
}
