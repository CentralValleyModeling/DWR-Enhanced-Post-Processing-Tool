#define LEN1 1800
#define LEN2 5000
/* native threads in Win NT/95 demand excessive memory -- change this to result
 in smaller stack space requirements */
#define MAXHEADER 90
#ifdef __sun 
#define STDCALL
#define STRLEN_TYPE long int
#define generateCatalog generate_catalog_
#define getRecordType get_record_type_
#define retrieveRegularTimeSeries retrieveregulartimeseries_
#define retrieveIrregularTimeSeries retrieveirregulartimeseries_
#define retrievePairedData retrievepaired_
#define storeRegularTimeSeries storeregulartimeseries_
#define storeIrregularTimeSeries storeirregulartimeseries_
#define storePairedData storepaired_
#define getNumberOfValuesInInterval getnumberofvaluesininterval_
#else
#define STDCALL _cdecl
#define STRLEN_TYPE int
#define generateCatalog GENERATE_CATALOG
#define retrieveRegularTimeSeries RETRIEVEREGULARTIMESERIES
#define retrieveIrregularTimeSeries RETRIEVEIRREGULARTIMESERIES
#define retrievePairedData RETRIEVEPAIRED
#define storeRegularTimeSeries STOREREGULARTIMESERIES
#define storeIrregularTimeSeries STOREIRREGULARTIMESERIES
#define storePairedData STOREPAIRED
#define getRecordType GET_RECORD_TYPE
#define getNumberOfValuesInInterval GETNUMBEROFVALUESININTERVAL
#endif
#ifdef __cplusplus
extern "C" {
#endif
#ifdef __sun /* sun specific calls */
  /*
   * return a character array of LEN1 with blank elements after null 
   * element in string. This makes it compatible with fortran strings
   */
  char * cleanCharacterArray(char* array, int length);
  /*
   * sets the marker at the end of the last non - blank character
   * or at the last element of the array if no last non-blank
   */
  void setEndOfStringMarker(char* array);
  /* generate catalog */
  void STDCALL generateCatalog(char * filename, STRLEN_TYPE lenf);
  /* get type of record */
  int STDCALL getRecordType(char * filename, char * pathname,
			    STRLEN_TYPE lenf, STRLEN_TYPE lenp);
  /* get number of values in a given interval of a path*/
  int STDCALL getNumberOfValuesInInterval(long int& startTime, long int& endTime, char * cpath, 
					  STRLEN_TYPE lencpath);
  /* Retrieve time series data */
  void STDCALL retrieveRegularTimeSeries(char * filename, 
					 char * pathname, 
					 long int& startTime, 
					 long int& endTime, 
					 int& retrieveFlags,
					 int& nvals, 
					 float * values, 
					 float * flags, 
					 float * headu,
					 char * units, 
					 char * type, 
					 int& offset,
					 STRLEN_TYPE lenf, STRLEN_TYPE lenp, 
					 STRLEN_TYPE lenu, STRLEN_TYPE lent
					 );
  /* Retrieve regular time series data */
  void STDCALL retrieveIrregularTimeSeries(char * filename, 
					   char * pathname, 
					   long int& startTime, 
					   long int& endTime, 
					   int& retrieveFlags,
					   int& nvals, 
					   int * timeValues,
					   float * yValues,
					   float * flags, 
					   float * headu,
					   char * units, 
					   char * type, 
					   STRLEN_TYPE lenf, STRLEN_TYPE lenp, 
					   STRLEN_TYPE lenu, STRLEN_TYPE lent
					   );
  /* Retrieve paired data */
  void STDCALL retrievePairedData(char * filename, 
				  char * pathname, 
				  int& nvals, 
				  float * values,
				  float * headu,
				  char * xUnits,
				  char * xType,
				  char * yUnits,
				  char * yType,
				  STRLEN_TYPE lenf, STRLEN_TYPE lenp, 
				  STRLEN_TYPE lenxu, STRLEN_TYPE lenxt, 
				  STRLEN_TYPE lenyu, STRLEN_TYPE lenyt
				  );
  /* Store time series data */
  void STDCALL storeRegularTimeSeries( char * filename, 
				       char * pathname, 
				       long int& startTime, 
				       long int& endTime, 
				       int& storeFlags,
				       float * values, 
				       float * flags, 
				       char * units, 
				       char * type,
				       STRLEN_TYPE lenf, STRLEN_TYPE lenp, 
				       STRLEN_TYPE lenu, STRLEN_TYPE lent);
  /* Store regular time series data */
  void STDCALL storeIrregularTimeSeries(char * filename,
					char * pathname,
					long int& startTime, 
					long int& endTime,  
					int& storeFlags,
					int& nvals, 
					int * xValues, 
					float * values, 
					float * flags, 
					char * units, 
					char * type, 
					STRLEN_TYPE lenf, STRLEN_TYPE lenp, 
					STRLEN_TYPE lenu, STRLEN_TYPE lent);
  /* Store paired data */
  void STDCALL storePairedData(char * filename, 
			       char * pathname, 
			       int& nvals, 
			       float * values, 
			       char * xUnits, 
			       char * xType, 
			       char * yUnits,
			       char * yType, 
			       STRLEN_TYPE lenf, STRLEN_TYPE lenp, 
			       STRLEN_TYPE lenxu, STRLEN_TYPE lenxt, 
			       STRLEN_TYPE lenyu, STRLEN_TYPE lenyt);

#else /* Windows specific code */
  /*
   * return a character array of LEN1 with blank elements after null 
   * element in string. This makes it compatible with fortran strings
   */
  char * cleanCharacterArray(char* array, int length);
  /*
   * sets the marker at the end of the last non - blank character
   * or at the last element of the array if no last non-blank
   */
  void setEndOfStringMarker(char* array);
  /* generate catalog */
  void STDCALL generateCatalog(char * filename, STRLEN_TYPE lenf);
  /* get type of record */
  int STDCALL getRecordType(char * filename, STRLEN_TYPE lenf, 
			    char * pathname, STRLEN_TYPE lenp);
  /* get number of values in a given interval of a path*/
  int STDCALL getNumberOfValuesInInterval(long int& startTime, long int& endTime, char * cpath, 
					  STRLEN_TYPE lencpath);
  /* Retrieve time series data */
  void STDCALL retrieveRegularTimeSeries(char * filename, STRLEN_TYPE lenf, 
					 char * pathname, STRLEN_TYPE lenp,
					 long int& startTime, 
					 long int& endTime, 
					 int& retrieveFlags,
					 int& nvals, 
					 float * values, 
					 float * flags, 
					 float * headu,
					 char * units, STRLEN_TYPE lenu, 
					 char * type, STRLEN_TYPE lent,
					 int& offset
					 );
  /* Retrieve regular time series data */
  void STDCALL retrieveIrregularTimeSeries(char * filename, STRLEN_TYPE lenf,
					   char * pathname, STRLEN_TYPE lenp,
					   long int& startTime, 
					   long int& endTime, 
					   int& retrieveFlags,
					   int& nvals, 
					   int * timeValues,
					   float * yValues,
					   float * flags, 
					   float * headu,
					   char * units, STRLEN_TYPE lenu,
					   char * type, STRLEN_TYPE lent
					   );
  /* Retrieve paired data */
  void STDCALL retrievePairedData(char * filename, STRLEN_TYPE lenf,
				  char * pathname,  STRLEN_TYPE lenp, 
				  int& nvals, 
				  float * values,
				  float * headu,
				  char * xUnits, STRLEN_TYPE lenxu,
				  char * xType, STRLEN_TYPE lenxt,
				  char * yUnits, STRLEN_TYPE lenyu,
				  char * yType, STRLEN_TYPE lenyt
				  );
  /* Store time series data */
  void STDCALL storeRegularTimeSeries( char * filename, STRLEN_TYPE lenf,
				       char * pathname, STRLEN_TYPE lenp,
				       long int& startTime, 
				       long int& endTime, 
				       int& storeFlags,
				       float * values, 
				       float * flags, 
				       char * units, STRLEN_TYPE lenu, 
				       char * type, STRLEN_TYPE lent
				       );
  /* Store regular time series data */
  void STDCALL storeIrregularTimeSeries(char * filename, STRLEN_TYPE lenf,
					char * pathname, STRLEN_TYPE lenp,
					long int& startTime, 
					long int& endTime,  
					int& storeFlags,
					int& nvals, 
					int * xValues, 
					float * values, 
					float * flags, 
					char * units, STRLEN_TYPE lenu,
					char * type, STRLEN_TYPE lent
					);
  /* Store paired data */
  void STDCALL storePairedData(char * filename, STRLEN_TYPE lenf,
			       char * pathname, STRLEN_TYPE lenp, 
			       int& nvals, 
			       float * values, 
			       char * xUnits, STRLEN_TYPE lenxu,
			       char * xType, STRLEN_TYPE lenxt,
			       char * yUnits, STRLEN_TYPE lenyu,
			       char * yType, STRLEN_TYPE lenyt
			       );

#endif
#ifdef __cplusplus
	   }
#endif
