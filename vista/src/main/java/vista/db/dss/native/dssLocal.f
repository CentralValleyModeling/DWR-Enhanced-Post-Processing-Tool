c----- subroutine to generate a catalog file ( condensed &
c----- standard )
      subroutine generate_catalog (filename)
      implicit none
      include 'dssLocal.inc'
c----- arg list
      character*(*) filename
      integer ifltab(600), iostat, idtype
     &     ,icunit              ! catalog file unit number
     &     ,icdunit             ! condensed unit number
     &     ,inunit              ! input unit number
     &     ,nrecs               ! number of records in the catalog file
c-----character*(*) cname
      character cinstr*256,cname*256
      integer nname, ncrecs
      logical
     &     labrev               ! true for abbreviated cat file
     &     ,lsort               ! true to sort cat file
     &     ,lcdcat              !
     &     ,lcatlg
     &     ,lcatd
     &     ,lcreat,lcreatd
     &     ,lopen,lopend
      logical lfileExist
c----- check if dss file exists. If not return with -1 ( error code for 
c----- non - existent file )
      call zset('PROGRAM', 'VISTA', 0)
      call zset('MLEVEL', '', 0)
      call zset('CCDATE','ON',0)
      call zfname( filename, cname, nname, lfileExist )
      if (.not. lfileExist) then
         write(*,*) '** The DSS File does not exist: ' , filename
         idtype = -1
         goto 999
      endif
c-----open dss file
      call zopen( ifltab, filename, iostat )
      if ( iostat .ne. 0 ) then
         write(*,*) ' *** Error in opening DSS File, status: ', iostat, 
     &        ', Name: ', filename
         idtype = -1
         goto 999
      endif
c-----generate catalog 
      icunit=12
      icdunit=13
      lcreat=.true.
      lopen=.true.
      lcreatd=.true.
      lopend=.true.
      call zopnca (filename, icunit, lcreat, lopen, lcatlg,
     &     icdunit, lcreatd, lopend, lcatd, nrecs)
      
      inunit = 0
      cinstr='CA.NC'
      labrev=.true.
      lsort=.true.
      lcdcat=.true.
      call zcat(ifltab, icunit, icdunit, inunit, cinstr, 
     &     labrev, lsort, lcdcat, ncrecs)
c-----close dss file if opened...
 990  call zclose(ifltab)
      close(icunit)
      close(icdunit)
 999  end
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c----- A function returning an integer specifying the type of 
c----- record referenced by the pathname in given filename
c----- returns
c----- -1 if error occurs ( non-existent file, opening file or 
c----- non-existent pathname )
c----- 0 (undefined)
c----- 100 ( regular-interval time series)
c----- 110 ( irregular-interval time series)
c----- 200 ( paired data )
c----- 300 ( text data )
      integer function get_record_type(filename, pathname)
      implicit none
      include 'dssLocal.inc'
c----- arg list
      character*(*) filename, pathname
c-----locals for open
      integer lnblnk
      integer ifltab(600), iostat, idtype
      character cname*256, cdtype*3
      integer nname
      logical lfileExist, lexist
c----- check if dss file exists. If not return with -1 ( error code for 
c----- non - existent file )
      call zset('PROGRAM', 'VISTA', 0)
      call zset('MLEVEL', '', mlevel)
      call zfname( filename, cname, nname, lfileExist )
      if (.not. lfileExist) then
         write(*,*) '** The DSS File does not exist: ' , filename
         idtype = -1
         goto 999
      endif
c----- open dss file
      call zopen( ifltab, cname, iostat )
      if ( iostat .ne. 0 ) then
         write(*,*) ' *** Error in opening DSS File, status: ', iostat, 
     &        ', Name: ', cname
         idtype = -1
         goto 999
      endif
c----- get type of pathname in file
      call zdtype( ifltab, pathname, lnblnk(pathname), 
     &     lexist, cdtype, idtype)

      if (.not. lexist) then
         write(*,*) '** The pathname: ' , pathname, ' does not exist'
         idtype = -1
         goto 990
      endif
c-----close dss file if opened...
 990  call zclose(ifltab)
c----- set value of record type and return
c-----write(*,*) "Type of record is: " , idtype
 999  get_record_type = idtype
      return
      end
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine retrieveRegularTimeSeries(filename, pathname,
     &     startTime, endTime, retrieveFlags, nvals,
     &     values, flags, headu,
     &     cunits, ctype, iofset)
      implicit none
      include 'dssLocal.inc'
c----- arg list
c-----input list
      character*(*) filename, pathname, cunits, ctype
      integer*4 startTime, endTime
      logical retrieveFlags
      integer nvals, lnblnk
c-----output list
      real values(*)
c-----real   tmpValues(*)
      real flags(*), headu(*)
      integer iofset
c-----locals
      integer m2ihm
      logical lFileExist, lfread
      character cname*256, cdate*20,ctime*4
      integer ifltab(600), nname, 
     &     getNumberOfValuesInInterval, 
     &     ndate, itime, nheadu
      integer iostat, istat, icomp
      integer idate
c----- get number of values for given start time and end time
c----- in minutes since Dec 31, 1899 2400
      call zset('PROGRAM', 'VISTA', 0)
      call zset('MLEVEL', '', mlevel)
      nvals = getNumberOfValuesInInterval(startTime, endTime, pathname)
      if ( nvals .eq. -1 ) then
         write(*,*) 'Could not calculate interval for ' , pathname
         goto 999
      endif
c      if ( nvals .ge. maxvals) then
c         write(*,*) 'Number of values more than maximum: ', 
c     &        maxvals, ' requested'
c         write(*,*) 'Only retrieving ',maxvals
c         nvals = maxvals
c      endif
c----- check if dss file exists. If not return with -1 ( error code for 
c----- non - existent file )
      call zfname( filename, cname, nname, lfileExist )
      if (.not. lfileExist) then
         write(*,*) '** The DSS File does not exist: ' , filename
         goto 999
      endif
c----- open dss file
      call zopen( ifltab, cname, iostat )
      if ( iostat .ne. 0 ) then
         write(*,*) ' *** Error in opening DSS File, status: ', iostat, 
     &        ', Name: ', cname
         goto 999
      endif
c----- 
      idate = startTime/1440
      itime = mod(startTime,1440)
      call juldat(idate, 104, cdate, ndate)
      itime = m2ihm(itime, ctime)
c----- retrieve the data
      call zrrtsx(ifltab, pathname, cdate, ctime, 
     &     nvals, values, flags, retrieveFlags, lfread,
     &     cunits, ctype, headu, kheadu, nheadu,
     &     iofset, icomp, istat)
c----- check on data retrieved
      if ( istat .eq. 5 ) then
c--------write(*,*) 'No records found in file ', filename,
c--------&        ' for pathname', pathname
      elseif ( istat .gt. 10 ) then
         write(*,*) 'A fatal error occured in file ', filename,
     &        ' for pathname', pathname
         nvals = -1
      endif
c----- close file
      call zclose(ifltab)
c----- copy values into array
c-----do i=1, nvals
c         values(i) = tmpValues(i)
c      enddo
c----- append null character
      cunits = cunits(1:lnblnk(cunits)) !// char(0)
      ctype = ctype(1:lnblnk(ctype)) !// char(0)
      return
 999  nvals = -1
      return
      end
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine storeRegularTimeSeries(
     &     filename, pathname,
     &     startTime, endTime, storeFlags, 
     &     values, flags,
     &     cunits, ctype)
      implicit none
      include 'dssLocal.inc'
c----- arg list
c-----input list
      character*(*) filename, pathname, cunits, ctype
      integer*4 startTime, endTime
      logical storeFlags
      integer nvals
c-----output list
      real values(*)
      real flags(*), headu(80)
c-----locals
      integer m2ihm
      logical lbasev, lhigh, iprec
      real basev
      character cname*256, cdate*20,ctime*4
      integer ifltab(600), 
     &     getNumberOfValuesInInterval, 
     &     ndate, itime, nheadu, iplan, icomp
      integer iostat, istat
      integer isdate, istime
c----- get number of values for given start time and end time
c----- in minutes since Dec 31, 1899 2400
      call zset('PROGRAM', 'VISTA', 0)
      call zset('MLEVEL', '', mlevel)
      nvals = getNumberOfValuesInInterval(startTime, endTime, pathname)
      if ( nvals .eq. -1 ) then
         write(*,*) 'Could not calculate interval for ' , pathname
         goto 999
      endif
c      if ( nvals .ge. maxvals) then
c         write(*,*) 'Number of values more than maximum: ', 
c     &        maxvals, ' requested'
c         write(*,*) 'Only retrieving ',maxvals
c         nvals = maxvals
c      endif
c----- open dss file
      call zopen( ifltab, filename, iostat )
      if ( iostat .ne. 0 ) then
         write(*,*) ' *** Error in opening DSS File, status: ', iostat, 
     &        ', Name: ', cname
         goto 999
      endif
c----- 
      isdate = startTime/1440
      istime = mod(startTime,1440)
      call juldat(isdate, 104, cdate, ndate)
      itime = m2ihm(istime, ctime)
      nheadu=0                  !no header info at this time
      iplan=0                   !always overwrite
      icomp=-1                  !no compression
      basev=0
      lbasev=.false.
      lhigh=.false.
      iprec=.false.
c----- retrieve the data
      call zsrtsx(ifltab, pathname, cdate, ctime, 
     &     nvals, values, flags, storeFlags, 
     &     cunits, ctype, headu, nheadu,
     &     iplan, icomp, basev, lbasev, lhigh, iprec, istat)
c----- check on data retrieved
      if ( istat .eq. 4 ) then
         write(*,*) 'No records found in file ', filename,
     &        ' for pathname', pathname
         nvals = -1
      elseif ( istat .gt. 10 ) then
         write(*,*) 'A fatal error occured in file ', filename,
     &        ' for pathname', pathname
         nvals = -1
      endif
c----- close file
      call zclose(ifltab)
      return
 999  nvals = -1
      return
      end
c----- +++++++++++++++++++++++++++++++++++++++++++++++++++
      function getNumberOfValuesInInterval(startTime, endTime, cpath)
      implicit none
c----- returns the number of values in given interval
c----- or -1 if an error occured in calculating the number of values
c----- the interval is specified by starting and ending times
c----- in julian minutes since Dec 31, 1899 2400 and the pathname
c-----arg list
      include 'dssLocal.inc'
      integer getNumberOfValuesInInterval
      integer*4 startTime, endTime
      character*(*) cpath
c-----locals
      character ca*64, cb*64, cc*64, cd*64, ce*64, cf*64
      integer na, nb, nc, nd, ne, nf
      integer npath, istat, interval, lnblnk, nvals, nopers
      integer isdate, istime, iedate, ietime
      call zset('MLEVEL', '', mlevel)
c----- get e part in pathname
      npath = lnblnk(cpath)
      call zufpn(ca,na,cb,nb,cc,nc,cd,nd,ce,ne,cf,nf,
     &     cpath,npath,istat)
      if ( istat .ne. 0 ) then
         write(*,*) 'Malformed pathname: ' , cpath
         goto 999
      endif
c----- get interval from e part
      istat = 1
      call zgintl(interval, ce, nvals, istat)
      if ( istat .ne. 0 ) then
         if ( istat .eq. 1 ) then
            write(*,*) 'Irregular time E part: ' , ce
            goto 999
         else 
            write(*,*) 'Non-time series E part: ' , ce
            goto 999
         endif
      endif
c----- get number of intervals in interval
      isdate = startTime/1440
      istime = mod(startTime,1440)
      iedate = endTime/1440
      ietime = mod(endTime,1440)
      nvals = nopers(interval, 0, isdate, istime, iedate, ietime)
      getNumberOfValuesInInterval = nvals+1 ! by 1 to include end of interval
      return
c----- error occured
 999   getNumberOfValuesInInterval = -1
       return
      end
c----- +++++++++++++++++++++++++++++++++++++++++++++++++++
      function getOffset(cpath)
c----- returns the offset for a given pathname if any
c----- if pathname has no offset it returns 0
      implicit none
      include 'dssLocal.inc'
      integer getOffset
c-----locals
      character cpath*1800
      character ca*64, cb*64, cc*64, cd*64, ce*64, cf*64
      integer na, nb, nc, nd, ne, nf, npath
      integer iofset, jul, itime, intl, istat, nvals
c-----
      call zset('MLEVEL', '', mlevel)
      call zufpn(ca,na,cb,nb,cc,nc,cd,nd,ce,ne,cf,nf,
     &     cpath, npath,istat)
      if ( istat .ne. 0 ) then
         write(*,*) 'Malformed pathname: ', cpath
         goto 999
      endif
      call zgintl(intl, ce, nvals, istat )
      iofset = 0
      if ( istat .eq. 0 ) then
         call zofset(jul,itime,intl,0,iofset)
      endif
 999  getOffset = iofset
      end
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine retrieveIrregularTimeSeries(filename, pathname,
     &     startTime, endTime, retrieveFlags, nvals,
     &     timeValues, yValues, flags, headu,
     &     cunits, ctype)
      implicit none
      include 'dssLocal.inc'
c----- arg list
c-----input list
      character*(*) filename, pathname, cunits, ctype
      integer*4 startTime, endTime
      logical retrieveFlags
      integer nvals
c-----output list
      integer timeValues(*)
      real yValues(*)
      real flags(*), headu(*)
c-----locals
      logical lFileExist, lfread
      character cname*360
      integer ifltab(600), nname, 
     &     nheadu, jbdate
      integer iostat, istat, index
      integer isdate, istime, iedate, ietime
c----- get number of values for given start time and end time
c----- in minutes since Dec 31, 1899 2400
      call zset('PROGRAM', 'VISTA', 0)
      call zset('MLEVEL', '', mlevel)
      if ( startTime .gt. endTime ) then
         write(*,*) 'Start time greater than end time '
         goto 999
      endif
c----- check if dss file exists. If not return with -1 ( error code for 
c----- non - existent file )
      call zfname( filename, cname, nname, lfileExist )
      if (.not. lfileExist) then
         write(*,*) '** The DSS File does not exist: ' , filename
         goto 999
      endif
c----- open dss file
      call zopen( ifltab, cname, iostat )
      if ( iostat .ne. 0 ) then
         write(*,*) ' *** Error in opening DSS File, status: ', iostat, 
     &        ', Name: ', cname
         goto 999
      endif
c----- retrieve the data
      isdate = startTime/1440
      istime = mod(startTime, 1440)
      iedate = endTime/1440
      ietime = mod(endTime, 1440)
      call zritsx(ifltab, pathname, 
     &     isdate, istime,
     &     iedate, ietime,
     &     timeValues, yValues, maxvals, nvals, jbdate, 
     &     flags, retrieveFlags, lfread,
     &     cunits, ctype, headu, kheadu, nheadu,
     &     3, istat)
c----- check on data retrieved
      if ( istat .eq. 4 ) then
         write(*,*) 'No data found in file ', filename,
     &        ' for pathname', pathname
         nvals = -1
      elseif ( istat .gt. 10 ) then
         write(*,*) 'A fatal error occured in file ', filename,
     &        ' for pathname', pathname
         nvals = -1
      endif
c----- close file
      call zclose(ifltab)
c----- update time array with julian minutes since base date
      jbdate = jbdate*1440
      do index = 1, nvals
         timeValues(index) = timeValues(index)+jbdate
      enddo
c-----append null character
      cunits = cunits! // char(0)
      ctype = ctype! // char(0)
      return
 999  nvals = -1
      return
      end
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine storeIrregularTimeSeries(filename, pathname,
     &     startTime, endTime, storeFlags, nvals,
     &     timeValues, yValues, flags, 
     &     cunits, ctype)
      implicit none
      include 'dssLocal.inc'
c----- arg list
c-----input list
      character*(*) filename, pathname, cunits, ctype
      integer*4 startTime, endTime
      logical storeFlags
      integer nvals
c-----output list
      integer timeValues(*)
      real yValues(*), headu(80)
      real flags(*)
c-----locals
      integer ifltab(600), 
     &     nheadu, jbdate
      integer iostat, istat, i
c----- get number of values for given start time and end time
c----- in minutes since Dec 31, 1899 2400
      call zset('PROGRAM', 'VISTA', 0)
      call zset('MLEVEL', '', mlevel)
      if ( startTime .gt. endTime ) then
         write(*,*) 'Start time greater than end time '
         goto 999
      endif
c----- check for name and make it a valid one
c-----call zfname( filename, cname, nname, lfileExist )
c----- open dss file
      call zopen( ifltab, filename, iostat )
      if ( iostat .ne. 0 ) then
         write(*,*) ' *** Error in opening DSS File, status: ', iostat, 
     &        ', Name: ', filename
         goto 999
      endif
c----- adjust the data to a starttime and offsets from there on...
      jbdate = timeValues(1)/1440
      do i=1,nvals
         timeValues(i) = timeValues(i)-jbdate*1440
      enddo
      nheadu = 0
      call zsitsx(ifltab, pathname, 
     &     timeValues, yValues, nvals, jbdate, 
     &     flags, storeFlags, 
     &     cunits, ctype, headu, nheadu,
     &     1, istat)
c----- check on data stored
      if ( istat .ne. 0 ) then
         write(*,*) 'Error occured storing ', pathname, ' in ', filename
      endif
c----- close file
 999  call zclose(ifltab)
      return
      end
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine retrievePaired(filename, pathname,
     &     nvals,
     &     values, headu,
     &     c1unit, c1type, c2unit, c2type)
      implicit none
      include 'dssLocal.inc'
c----- arg list
c-----input list
      character*(*) filename, pathname, c1unit, c1type, c2unit, c2type
      integer nvals
c-----output list
      real values(*)
      real headu(*)
c-----locals
      logical lFileExist, label
      character cname*256
      character*80 clabel(kheadu)
      integer ifltab(600), nname, nord, ncurve, ihoriz,
     &     nheadu
      integer iostat, istat
      call zset('PROGRAM', 'VISTA', 0)
      call zset('MLEVEL', '', mlevel)
c-----write(*,*) "retrieveing paired"
c-----write(*,*) "filename: " , filename
c----- check if dss file exists. If not return with -1 ( error code for 
c----- non - existent file )
      call zfname( filename, cname, nname, lfileExist )
      if (.not. lfileExist) then
         write(*,*) '** The DSS File does not exist: ' , filename
         goto 999
      endif
c----- open dss file
      call zopen( ifltab, cname, iostat )
      if ( iostat .ne. 0 ) then
         write(*,*) ' *** Error in opening DSS File, status: ', iostat, 
     &        ', Name: ', cname
         goto 999
      endif
c----- retrieve the data
      call zrpd(ifltab, pathname, nord, ncurve, ihoriz,
     &     c1unit, c1type, c2unit, c2type, 
     &     values, maxvals, nvals, 
     &     clabel, kheadu, label,
     &     headu, kheadu, nheadu,
     &     istat)
c----- check on data retrieved
      if ( istat .eq. -1 ) then
         write(*,*) 'No data found in file ', filename,
     &        ' for pathname', pathname
         nvals = -1
      elseif ( istat .gt. 0 ) then
         write(*,*) 'Record may not be paired or may be too big in '
     &        , filename,
     &        ' for pathname', pathname
         nvals = -1
      endif
c----- close file
      call zclose(ifltab)
c----- check for # of curves in pathname.
      if ( ncurve .gt. 1 ) then
         write(*,*) 'Too many curves in pathname ', pathname
         goto 999
      else if (ncurve .lt. 1) then
         write(*,*) 'Too few curves in pathname ', pathname
         goto 999
      endif
c----- fill up x and y value arrays
      nvals = nord
c-----append null character
      c1unit = c1unit! // char(0)
      c1type = c1type! // char(0)
      c2unit = c2unit! // char(0)
      c2type = c2type! // char(0)
      return
 999  nvals = -1
      return
      end
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine storePaired(filename, pathname,
     &     nvals,
     &     values,
     &     c1unit, c1type, c2unit, c2type)
      implicit none
      include 'dssLocal.inc'
c----- arg list
c-----input list
      character*(*) filename, pathname, c1unit, c1type, c2unit, c2type
      integer nvals
c-----output list
      real values(*)
      real headu(80)
c-----locals
      logical lFileExist, label
      character cname*256
      character*80 clabel(kheadu)
      integer ifltab(600), nname, nord, ncurve, ihoriz,
     &     nheadu
      integer iostat, istat, iplan
c----- check if dss file exists. If not return with -1 ( error code for 
c----- non - existent file )
      call zset('PROGRAM', 'VISTA', 0)
      call zfname( filename, cname, nname, lfileExist )
      if (.not. lfileExist) then
         write(*,*) '** The DSS File does not exist: ' , filename
         goto 999
      endif
c----- open dss file
      call zopen( ifltab, cname, iostat )
      if ( iostat .ne. 0 ) then
         write(*,*) ' *** Error in opening DSS File, status: ', iostat, 
     &        ', Name: ', cname
         goto 999
      endif
      ncurve = 1
      nheadu = 0
      iplan = 0
c----- store the data
      call zspd(ifltab, pathname, nord, ncurve, ihoriz,
     &     c1unit, c1type, c2unit, c2type, 
     &     values,  
     &     clabel, label,
     &     headu, nheadu,
     &     iplan, istat)
c----- check on data stored
      if ( istat .ne. 0 ) then
         write(*,*) 'Error storing ', pathname, ' in ', filename
      endif
c----- close file
 999  call zclose(ifltab)
      return
      end
c++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      subroutine removePath(filename,pathname,startTime,endTime)
      implicit none
      include 'dssLocal.inc'
      character*(*) filename, pathname
      integer*4 startTime, endTime
      end
