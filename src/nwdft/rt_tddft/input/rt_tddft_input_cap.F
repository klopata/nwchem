C
C     rt_tddft_input_cap.F
C     
C     Parses input deck for rt-tddft spatial complex absorbing potential (CAP).
C     
C
      subroutine rt_tddft_input_cap (rtdb)
      implicit none

#include "rt_tddft.fh"
#include "errquit.fh"
#include "inp.fh"
#include "rtdb.fh"
#include "mafdecls.fh"
#include "stdio.fh"

      
C     == Inputs ==
      integer, intent(in)      :: rtdb

C     == Parameters ==
      character(*), parameter :: pname = "rt_tddft_input_cap: "

C     == Variables ==
      logical done
      character*255 test
      character*32 entry_name

      double precision :: qmin(3), qmax(3)
      double precision :: maxval
      integer :: nq(3)
      logical :: lhave_maxval
      logical :: lhave_qmin, lhave_qmax, lhave_nq
      integer :: nfound
      logical lactive

      lhave_maxval = .false.
      lhave_qmin = .false.
      lhave_qmax = .false.
      lhave_nq = .false.
      lactive = .false.  ! CAP defaults to turned off

      done = .false.
      do while (.not. done)

         if (.not. inp_read()) 
     $        call errquit(pname//'Read failed input',0, INPUT_ERR)
         if (.not. inp_a(test)) 
     $        call errquit(pname//'Read failed keyword',0, INPUT_ERR)

         if (inp_compare(.false.,test,'maxval')) then
            if (.not. inp_f (maxval)) call errquit (pname//
     $           "maxval takes a float",0,0)
            lhave_maxval = .true.

         elseif (inp_compare(.false.,test,'qmin')) then
            if (.not. inp_f (qmin(1))) call errquit (pname//
     $           "qmin takes three floats",0,0)
            if (.not. inp_f (qmin(2))) call errquit (pname//
     $           "qmin takes three floats",0,0)
            if (.not. inp_f (qmin(3))) call errquit (pname//
     $           "qmin takes three floats",0,0)
            lhave_qmin = .true.

         elseif (inp_compare(.false.,test,'qmax')) then
            if (.not. inp_f (qmax(1))) call errquit (pname//
     $           "qmax takes three floats",0,0)
            if (.not. inp_f (qmax(2))) call errquit (pname//
     $           "qmax takes three floats",0,0)
            if (.not. inp_f (qmax(3))) call errquit (pname//
     $           "qmax takes three floats",0,0)
            lhave_qmax = .true.

         elseif (inp_compare(.false.,test,'nq')) then
            if (.not. inp_ilist (3, nq, nfound))
     $           call errquit (pname//"nq takes three ints",0,0)
            lhave_nq = .true.

         elseif (inp_compare(.false.,test,'on')) then
            lactive = .true.

         elseif (inp_compare(.false.,test,'off')) then
            lactive = .false.


C     
C     end of parse
C     
         else if (inp_compare(.false.,test,'end')) then
            done = .true.
         else
            call errquit(pname//'Unknown directive: '//trim(test),
     $           0, INPUT_ERR)
         endif
      enddo


C
C     check params
C
      if (.not. lhave_maxval)
     $     call errquit (pname//"Must supply maxval for CAP", 0, 0)

      if (.not. lhave_qmin)
     $     call errquit (pname//"Must supply qmin for CAP", 0, 0)
         
      if (.not. lhave_qmax)
     $     call errquit (pname//"Must supply qmax for CAP", 0, 0)

      if (.not. lhave_nq)
     $     call errquit (pname//"Must supply nq for CAP", 0, 0)


C
C     Load into rtdb
C
      entry_name = "rt_tddft:cap_maxval"
      if (.not.rtdb_put(rtdb,entry_name,mt_dbl,1,maxval))
     $     call errquit(pname//'Write failed to rtdb',0,RTDB_ERR)

      entry_name = "rt_tddft:cap_qmin"
      if (.not.rtdb_put(rtdb,entry_name,mt_dbl,3,qmin))
     $     call errquit(pname//'Write failed to rtdb',0,RTDB_ERR)

      entry_name = "rt_tddft:cap_qmax"
      if (.not.rtdb_put(rtdb,entry_name,mt_dbl,3,qmax))
     $     call errquit(pname//'Write failed to rtdb',0,RTDB_ERR)

      entry_name = "rt_tddft:cap_nq"
      if (.not.rtdb_put(rtdb,entry_name,mt_int,3,nq))
     $     call errquit(pname//'Write failed to rtdb',0,RTDB_ERR)

      entry_name = "rt_tddft:cap_active"
      if (.not.rtdb_put(rtdb,entry_name, mt_log,1, lactive))
     $     call errquit(pname//'Write failed to rtdb',0,RTDB_ERR)

      end subroutine
