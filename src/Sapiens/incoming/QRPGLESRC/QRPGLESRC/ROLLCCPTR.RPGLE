     H**
     H** DO NOT INSTALL/RUN AT CLIENT - RATHER RUN IN PDSDEV AND COPY FILE
     H** NEW RECORDS TO CLIENT - THIS IS TO ENSURE THAT PGM CVTTXDENT
     H** CAN WRITE ALL INCOMING RECORDS
     H**
     H**
     H* (C) Copyright Progressive Data Solutions, 2002-2008
     H/TITLE ROLLUCCPT                                                                      *
     H**************************************************************************************
     H*  Description:  Rollover CPT Codes to new effective date                            *
     H*                                                                                    *
     H*  Created: 02/25/02   Programmer C:79    Project:PAMNT                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*  06/12/08   C:180    PROJECT TX042
     H*                      - Copied source from MCST.UTIL to ALDON.
     H*                      - Change effective date to 03/01/08
     H*
     H*                                                                                    *
     H**************************************************************************************
     FFMPRPUC   IF A E             DISK
     FFMPRLUC   IF   E           K DISK    Rename(ppr01:ppr02)

     D*
     D sys_date        s               D   datfmt(*USA)
     D sys_time        s               T
     D sys_time_n      s              6  0
     ***** Begin change ******  06/12/08 **********************************
     D**** kkefdt          s              7  0 inz(1080101)
     D kkefdt          s              7  0 inz(1080301)
     ***** End change   ******  06/12/08 **********************************

     DPSDS           ESDS                  Extname(SZQ1P)

     C     *like         define    prco#         kkco#
     C     *like         define    prefdt        kkefdt
     C     *like         define    prcpt4        kkcpt4

     C     prky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkcpt4
     C                   kfld                    kkefdt

     C*----------------------------------------------------------------------
     C*  Read through CPT file
     C*----------------------------------------------------------------------
     C     *hival        setgt     ppr01
     C                   readp     ppr01                                  99
     C                   dow       (*in99 = *off)
     C*
     C                   eval      kkco# =prco#
     C                   eval      kkcpt4=prcpt4
     C*
     C                   if        prefdt = 1080101
     C*
     C     prky          chain     ppr02                              98
     C                   if        *in98=*on and prstat = 'A'

     C                   exsr      initz
     C*
     C                   exsr      writ
     C*
     C                   endif
     C*
     C                   endif
     C*
     C                   readp     ppr01                                  99
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   Return
     C*
     C     initz         begsr
     C*
     C                   move      *date         sys_date
     C                   time                    sys_time_n
     C     *hms          move      sys_time_n    sys_time
     C*
     C                   endsr
     C*
     C     writ          begsr

     C                   z-add     kkefdt        prefdt

     C     *cymd         move      sys_date      prdt
     C                   eval      pridt=prdt
     C                   eval      prpgm=q1pgm
     C                   eval      pruser=q1user
     C                   eval      priusr=q1user
     C                   move      sys_time      prtime
     C*
     C                   write     ppr01
     C*
     C     writend       endsr
     C*
