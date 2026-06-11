     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE W8311R - DCI Data Export Initial Selection               *
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION: DCI Data Export Initial Selection        *
     H*                                                               *
     H* CALLED BY: W8311C                                             *
     H*                                                               *
     H* CALLS TO: NONE                                                *
     H*                                                               *
     H* CREATED: 05/21/09  C:191 PROJECT WC1792                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/08/11 C:191  PROJECT: WC1987                               *
     H*                 Modify process to log claims with date        *
     H*                 reported less than the period end date.       *
     H*                                                               *
     H*****************************************************************
    F*  Primary logical file definition
     Fwxf1l     if   e           k disk
     Fwmcml16   if   e           k disk
     Fwddil1    uf a e           k disk
    D*---------------------------------------------------------------------
    D* Stand Alone Fields
    D*---------------------------------------------------------------------
      *
     D @@co#           S              3
     D @@prd           S              4
     D @@trcd          S              1
     D*
     D xxco#           S              3  0
     D xxprd           S              4  0
     D*** BEGIN ADD ****** 03/08/11 ************************************
     D xxprd7          S              7  0
     D date1           s               d   datfmt(*iso)
     D date2           s               d   datfmt(*iso)
     D*** END   ADD ****** 03/08/11 ************************************
     D*
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D*
      *
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
     D psds          esds                  extname(szq1p)
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D*** BEGIN DEL ****** 03/08/11 ************************************
     D************     ds
     D*xxdt7******                    7  0
     D**xxyy******                    2  0 overlay(xxdt7:2)
     D**xxmm******                    2  0 overlay(xxdt7:4)
     D*** END   DEL ****** 03/08/11 ************************************
     D                 ds
     D xxmy                           4  0
     D  xxmm1                         2  0 overlay(xxmy)
     D  xxyy1                         2  0 overlay(xxmy:3)
     D*** BEGIN ADD ****** 03/08/11 ************************************
     D                 ds
     D xxmdy                          6  0
     D  xxmm                          2  0 overlay(xxmdy)
     D  xxdd                          2  0 overlay(xxmdy:3)
     D  xxyy                          2  0 overlay(xxmdy:5)
     D*** END   ADD ****** 03/08/11 ************************************
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@co#             3
     C                   parm                    @@prd             4
     C                   parm                    @@trcd            1
     C*
     C     dtcmdp        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *blanks       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM      *BLANK        SRERR
     C*
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C* Key list for Claim File
     C     cmky          klist
     C                   kfld                    xxco#
     C                   kfld                    f1st
     C                   kfld                    f1fnd
     C*
     C* Key list for DCI file
     C     diky          klist
     C                   kfld                    xxco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C*
    C*================================================================
    C*  MAIN PROGRAM
    C*================================================================
     C                   move      @@co#         xxco#             3 0
     C                   move      @@prd         xxprd             4 0
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C* Get last day of period
     C                   eval      xxmy = xxprd
     C                   eval      xxmm = xxmm1
     C                   eval      xxdd = 01
     C                   eval      xxyy = xxyy1
     C*
     C     *MDY          move      xxmdy         date1
     C                   eval      date2 = (date1 + %months(1)) - %days(1)
     C     *cymd         move      date2         xxprd7
     C*** END   ADD ****** 03/08/11 ************************************
     C*
     C* Read records from DCI extract file
     C     xxco#         setll     pf101
     C                   dou       %eof(wxf1l)
     C     xxco#         reade     pf101
     C                   if        not %eof(wxf1l) and f1flag = 'I'
     C*
     C* if DCI extract record has flag = include, then get record from claims file
     C     cmky          setll     pcm01
     C                   dou       %eof(wmcml16)
     C     cmky          reade     pcm01
     C                   if        %eof(wmcml16)
     C                   leave
     C                   endif                                                   %eof(wmcml16)
     C*
     C* check claims record for validity
     C                   exsr      srvalid
     C* Valid claim record found, process
     C                   if        not *in90
     C* Write record to DCI Claim history file
     C                   exsr      srWrtDCI
     C                   endif                                                  not *in90
     C                   enddo                                                  %eof(wmcml16)
     C                   endif                                                  not %eof & f1flag
     C                   enddo
     C* done process return
     C                   seton                                        lr
    C*================================================================
    C*  srvalid - Validate claim record
    C*================================================================
     C     srvalid       begsr
     C* Assume error...
     C                   eval      *in90 = *on
     C* Check if claim date reported in is run period
     C*** BEGIN CHG ****** 03/08/11 ************************************
     C************       eval      xxdt7 = cmdtrp
     C************       eval      xxmm1 = xxmm
     C************       eval      xxdd1 = xxdd
     C************       eval      xxyy1 = xxyy
     C************       if        xxmy <> xxprd
     C* If date claim reported is greater than period enddate then skip
     C                   if        cmdtrp > xxprd7
     C*** END   CHG ****** 03/08/11 ************************************
     C                   leavesr
     C                   endif
     C* Lost time flag 'I' or 'M'
     C                   if        cmltfl <> 'I' and cmltfl <> 'M'
     C                   leavesr
     C                   endif
     C* Reached here so no error found
     C                   eval      *in90 = *off
     C                   endsr
    C*================================================================
    C*  srWrtDCI - Write/Update record to DCI history file
    C*================================================================
     C     srWrtDCI      begsr
     C*
     C* Check existing record
     C     diky          chain     pdi01                              60
     C*** BEGIN ADD ****** 03/08/11 ************************************
     C                   if        not %found
     C*** END   ADD ****** 03/08/11 ************************************
     C* Set Claim fields
     C                   eval      dico#  = xxco#
     C                   eval      difnd  = cmfnd
     C                   eval      difyr  = cmfyr
     C                   eval      dicase = cmcase
     C                   eval      didtrp = cmdtrp
     C                   eval      dist   = cmjrst
     C*
     C* Set DCI flags to blanks - these will be set while sampling
     C                   eval      dirnbr = 0
     C                   eval      dirplv = 0
     C                   eval      distat = ' '
     C                   eval      diindc = 'Y'
     C                   eval      dipdci = ' '
     C                   eval      diopni = ' '
     C                   eval      dicpnt = ' '
     C                   eval      dicnpt = ' '
     C                   eval      disamp = ' '
     C* Set maintenance fields
     C                   eval      diuser = q1user
     C                   eval      dipgm  = q1pgm
     C*
     C                   time                    dsdttm
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcmdp
     C                   movel(p)  srdto         didt
     C                   eval      ditime = dstime
     C*
     C*** BEGIN DEL ****** 03/08/11 ************************************
     C***************    if        not %found
     C*** END   DEL ****** 03/08/11 ************************************
     C* Set initial fields
     C                   eval      diiusr = q1user
     C                   eval      diipgm = q1pgm
     C                   movel(p)  srdto         diidt
     C                   eval      diitm  = dstime
     C* Write record
     C                   write     pdi01
     C*** BEGIN DEL ****** 03/08/11 ************************************
     C***************    else                                                   not %found
     C* Update record
     C***************    update    pdi01
     C*** END   DEL ****** 03/08/11 ************************************
     C                   endif                                                  not %found
     C*
     C                   endsr
