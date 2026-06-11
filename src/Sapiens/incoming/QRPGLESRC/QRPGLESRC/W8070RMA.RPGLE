     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H bnddir('WTBNDDIR')
     H/title W8070RMA - Massachusetts NOI print program
     H*****************************************************************
     H* Program Description:  This program prints Massachusetts NOI   *
     H*                       forms.                                  *
     H*                                                               *
     H* Called from:  W8070CMA                                        *
     H* Calls to:  None                                               *
     H*                                                               *
     H*****************************************************************
     H* Created 05/18/2005 by c:132    Project 129WIZ                 *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 06/09/20 C:33  Project: SY420
     H*                Mask SSN
     H*
     H*   10/17/11 C:279 Project: SY195                               *
     H*                  - Modify program to pass/return UNC path     *
     H*                    field and IFS flag to called/calling       *
     H*                    programs.                                  *
     H*                                                               *
     H* 03/24/10 C:232  Project: 139WNN                               *
     H*                 Update programs to use the new EDI Control API*
     H*                 WA076R instead of using WMIXP.                *
     H*                                                               *
     H*****************************************************************
     fwmlgl1    if   e           k disk
     fwmlgl     uf   e           k disk
     f                                     rename(plg01:plg02)
     fwxnil     if   e           k disk
     fwxnil1    if   e           k disk
     f                                     rename(pni01:pni02)
     fsmorl     if   e           k disk
     fwmcml1    if   e           k disk
     fwmfnl     if   e           k disk
     fwdf2l     if   e           k disk
     fwmcdl     if   e           k disk
     fwmeml     if   e           k disk
     fwmcxl     if   e           k disk
     fwmnil     if   e           k disk
     f                                     rename(pni01:pni04)
     fwmc1l     if   e           k disk
     F*** BEGIN DLT *** 03/24/10 **************************************
     f***wmixl     if   e           k disk
     F*** END DLT ***** 03/24/10 **************************************
     fsdcmld    o    e           k disk
     f                                     rename(pcm01:pcm02)
     fsdcmla    if   e           k disk
     f                                     rename(pcm01:pcm03) prefix(c3:2)
     fstctl1    if   e           k disk
     fwtabl     if   e           k disk
     f                                     ignore(pat01)
     F                                     IGNORE(pb201)
     f                                     ignore(pb701)
     f                                     ignore(pba01)
     f                                     ignore(pbc01)
     f                                     ignore(pbs01)
     f*************                        ignore(pca01)
     f                                     ignore(pcc01)
     f                                     ignore(pcs01)
     f                                     ignore(pcn01)
     f                                     ignore(phy01)
     f                                     ignore(pid01)
     f                                     ignore(pis01)
     f                                     ignore(plr01)
     f*************                        ignore(pna01)
     f                                     ignore(pnc01)
     f*************                        ignore(ppa01)
     f                                     ignore(ppc01)
     f                                     ignore(prc01)
     f                                     ignore(prs01)
     f                                     ignore(prt01)
     f                                     ignore(psp01)
     f                                     ignore(psy01)
     f                                     ignore(ptr01)
     f                                     ignore(ptt01)
     f                                     ignore(pvs01)
      *
     fqsysprt   o    f  127        printer oflind(*inoa)
     f                                     usropn
     fqsysprt1  o    f   85        printer oflind(*inof)
     f                                     usropn
     fqsysprt2  o    f  127        printer oflind(*inog)
     f                                     usropn
     D*================================================================
     D* Arrays and Tables
     D*================================================================
     d cnb             s              1    dim(40) ascend                       COMPANY NAME W/ BLAN
     d cn              s              1    dim(40)                              COMPANY NAME
     d sz              s              1    dim(13)                              STATE & ZIP
     d st              s              2    dim(1)                               STATE
     d cpy             s             21    dim(3) ctdata perrcd(1)              COPY MSG
     d ovr             s              1    dim(33)
     D*================================================================
     D* Constants
     D*================================================================
     D*
     d xxovr           c                   const('OVRPRTF QSYSPRT  OUT-
     d                                     Q(          )')
     d xxovr1          c                   const('OVRPRTF QSYSPRT1 OUT-
     d                                     Q(          )')
     d xxovr2          c                   const('OVRPRTF QSYSPRT2 OUT-
     d                                     Q(          )')
     D*================================================================
     D* Data Structures
     D*================================================================
     d psds          esds                  extname(szq1p)
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     D*================================================================
     D* User Define Data Structures
     D*================================================================
     d                 ds
     d  dsdttm                 1     14  0
     d  dstime                 1      6  0
     d  dsdate                 7     14  0
     d                 ds
     d  qckey                  1     40
     d  qcco#                  1      3  0
     d  qcfnd                  4      6  0
     d  qcfyr                  7      9  0
     d  qccase                10     16  0
     d                 ds
     d  xxzip                  1      9  0
     d  xxzpm1                 1      5  0
     d  xxzpm2                 6      9  0
     d                 ds
     d  XXXPHN                 1     10  0
     d  xxxara                 1      3  0
     d  xxXPH#                 4     10  0
     D*
     d                 ds
     d  ni10a1                 1     10
     d  wd                     1      7
     d                                     dim(7)                               DAY OF WK WORKED
      *
     d ##doc           s             12
     d ##fnd           s              8
     d ##fold          s              8
     d ##fpth          s             40
     d ##rt            s              2
     d ##ser           s              8
     d ##sub           s              8
     d ##term          s              1
     d @@case          s              7
     d @@co#           s              3
     d @@date          s              8
     d @@fnd           s              3
     d @@froi          s              2
     d @@fyr           s              3
     d @@jrst          s              2
     d kkco#           s                   like(nico#)
     d kkfnd           s                   like(nifnd)
     d kkfyr           s                   like(cmfyr)
     d kkcase          s                   like(cmcase)
     d kkdate          s                   like(nidt)
     d kkhcpy          s                   like(lghcpy)
     d*** BEG DEL ****** 03/24/10 *************************************
     d*** kkjrst          s                   like(ixjrst)
     d*** BEG DEL ****** 03/24/10 *************************************
     d kkkey           s                   like(pakey)
     d*** BEG DEL ****** 03/24/10 *************************************
     d*** kktset          s                   like(ixtrid)
     d*** BEG DEL ****** 03/24/10 *************************************
     d svfnd           s                   like(nifnd)
     d svtset          s                   like(lgtset)
     d ##dcid          s                   like(cmdcid)
     d ##fldr          s                   like(cmfold)
     d xxrmcd          s                   like(cmrmcd)
     d xxsctc          s                   like(f2sctc)
      *
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d test1           s              2  0
     d tm6             s              6  0
     d Length          s              2  0
     d pos1            s              2  0
     d pos2            s              2  0
     d x               s              5  0
     d xxaadr          s             50
     d xxacdt          s              8  0
     d xxactm          s              4  0
     d xxaww           s              7  2
     d xxbtdt          s              8  0
     d xxbtyp          s              1
     d xxcadr          s             60
     d xxccsz          s             40
     d xxclnm          s             46
     d xxcnam          s             50
     d xxcopy          s              1  0
     d xxcpy           s             21
     d xxcsz           s             40
     d xxcty           s             25
     d xxdphn          s             10  0
     d xxdod           s              8  0
     d xxdtlw          s              8  0
     d xxdtob          s              8  0
     d xxdtrp          s              8  0
     d xxdt1           s              8  0
     d xxdt2           s              8  0
     d xxemp#          s              9
     d xxephn          s             10  0
     d xxfadr          s             55
     d xxfmzp          s              9
     d xxfnd           s              3
     d xxfnme          s             40
     d xxfphn          s             10  0
     d xxfrst          s             15
     d xxfyr           s              3
     d xxhidt          s              8  0
     d xxlast          s             56
     d xxmad1          s             60
     d xxmad2          s             60
     d xxmc            s              2
     d xxopen          s              1
     d xxopn1          s              1
     d xxopn2          s              1
     d xxoutq          s             10
     d xxprep          s              8  0
     d xxprnm          s             20
     d xxprti          s             19
     d xxprt           s              1
     d xxrtw           s              8  0
     C***  Begin Chg  ***  06/09/20  *******************************
     d*xxss#           s              9  0
     d xxss#           s             11
     C***  End Chg  ***  06/09/20  *******************************
     d xxst            s             13
     d xxstat          s              2
     d xxsz            s             13
     d xxterm          s              1
     d xxwit1          s             40
     d xxwit2          s             40
     d xxzipa          s              5
     d xxzipb          s              4
     d xxzp1           s              5
     d xxzp2           s              4
     d XX10A6          s             10
     d XX30A4          s             30
     d XX30A5          s             30
     d XX30A6          s             30
     d x1outq          s             10
     d x2outq          s             10
      ***** BEG ADD ***** 10/17/11 ***********************************
     d root            s             10
     d env             s              1
      ***** END ADD ***** 10/17/11 ***********************************
     C***  Begin Add  ***  06/09/20  *******************************
     D/copy sPrSecure
     C***  End Add  ***  06/09/20  *******************************
     d*** End   Add *** 12/09/15 *********************************************
      *
     C*================================================================
     C* Parameter lists
     C*================================================================
     c     *entry        plist
     c                   parm                    @@co#
     c                   parm                    @@froi
     c                   parm                    @@jrst
     c                   parm                    @@date
     c                   parm                    @@fnd
     c                   parm                    @@fyr
     c                   parm                    @@case
     C*
     c     datchk        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     C*
     c     dtcymd        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      '*CYMD'       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     C*
     c     dtmdyy        plist
     c                   parm                    srdti
     c                   parm      '*CYMD'       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
     C*
     C*  TO SIX DIGIT YEAR
     c     dtmdy         plist
     c                   parm                    srdti
     c                   parm      '*CYMD'       srffmt
     c                   parm      '*MDY'        srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
      *
     C*** BEG ADD ****** 03/24/10 *************************************
     C     IXAPI         PLIST
     C                   PARM                    kkco#
     C                   PARM                    kkjrst            2
     C                   PARM                    kktset            3
     C                   PARM                    CMFND
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
     C*** END ADD ****** 03/24/10 *************************************
      *
     C*================================================================
     C* Key lists
     C*================================================================
     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     c     coky          klist
     c                   kfld                    kkco#
     C*
     C*  KEY LIST FOR THE NOI WORKFILE
     C*
     c     niky          klist
     c                   kfld                    kkco#
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     c     fnky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     C*
     C*  KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     c     f2ky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     c     cmky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase
     C*
     C*  KEY LIST FOR THE LOG HISTORY FILE
     C*
     c     lgky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkhcpy
     c                   kfld                    kkjrst
     c                   kfld                    kkdate
     C*
     c     lgky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase
     c                   kfld                    lgidt
     c                   kfld                    lgitm
     C*
     C*  KEY LIST FOR THE DARTS REMARK CODE FILE
     C*
     c     ctky          klist
     c                   kfld                    ixhcpr
     C*
     C*  KEY LIST FOR THE EDI CLAIM CONTROL FILE
     C*
     c     ixky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkjrst
     c                   kfld                    kktset
     C*
     C*  KEY LIST FOR THE CLAIMANT DEMOGRAPHICS FILE
     C*
     c     cdky          klist
     c                   kfld                    cmss#
     c*
     c*  KEY LIST FOR THE EMPLOYER MASTER FILE
     c*
     c     emky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    cmemp#
     c                   kfld                    cmdiv
     c*
     c*  KEY LIST FOR THE DARTS FILE
     c*
     c     c3ky          klist
     c                   kfld                    qckey
     c*
     c*   KEY LIST FOR THE OVERRIDE FILE
     c*
     c     orky          klist
     c                   kfld                    q1pgm
     c                   kfld                    xxprt
      *
      *  TABLE FILE
      *
     c     tbky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkkey
      *
      *  Key for User Profile file
      *
     c     upky          klist
     c                   kfld                    cmuser
     c*
     c*================================================================
     c* Main Processing
     c*================================================================
      ***** BEG ADD ***** 10/17/11 ***********************************
      * Get Root
     c                   call      'RTVDOCROOT'
     c                   parm                    root
      * Find IFS
     c                   call      'RTVDOCENV'
     c                   parm                    env
      ***** END ADD ***** 10/17/11 ***********************************
     c                   move      *blanks       xxcpy
     c                   eval      x = 0
     c                   time                    dsdttm
     c*
     c                   move      @@co#         kkco#
     c                   move      @@date        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         kkdate
     c                   movel     @@froi        kkhcpy
     c                   move      @@jrst        kkjrst
     c*
     c                   if        (q1prms > 4)
     c                   move      @@fnd         kkfnd
     c                   move      @@fyr         kkfyr
     c                   move      @@case        kkcase
     c                   else
     c                   clear                   kkfnd
     c                   clear                   kkfyr
     c                   clear                   kkcase
     c                   endif
     c*
     c*   GET THE DISTRIBUTION INFORMATION
     c*
     c                   move      *blanks       xxoutq
     c                   move      '0'           xxprt
     c     orky          chain     por01                              99
     c                   if        (oroutq = *blank)
     c                   movel     'QPRINT'      xxoutq
     c                   endif
     c*
     c                   move      *blanks       oroutq
     c                   move      '1'           xxprt
     c     orky          chain     por01                              99
     c                   movel     oroutq        x1outq
     c                   if        (x1outq = *blank)
     c                   movel     'QPRINT'      x1outq
     c                   endif
     C*
     c                   move      *blanks       oroutq
     c                   move      '2'           xxprt
     c     orky          chain     por01                              99
     c                   movel     oroutq        x2outq
     c                   if        (x2outq = *blank)
     c                   movel     'QPRINT'      x2outq
     c                   endif
     C*
     C*  IF THIS PGM WAS CALLED FROM THE DAILY BATCH, WXNIP WILL BE READ
     C*  3 TIMES TO PRODUCE 3 COPIES.  IF CALLED FROM THE EXTRACT
     C*  OPTION, 1 READ WILL OCCUR.
     C*
     c                   if        (kkcase = *zeros)
     c                   if        (@@froi <> '  ')
     c                   eval      xxcopy = 3
     c                   else
     c                   eval      xxcopy = 1
     c                   endif
     c                   endif
     C*
     c     1             do        2
     C*
     c                   if        (kkcase = *zeros)
     c                   eval      svfnd = 0
     c                   eval      x = xxcopy
     c                   move      cpy(x)        xxcpy
     c     x             comp      1                                      71
     c     x             comp      2                                      72
     c     x             comp      3                                      73
     c                   endif
     C*
     c                   setoff                                       75
     c                   if        (kkcase <> *zeros)
     c                   seton                                        75
     c                   endif
     C*
     C*   OPEN THE PRINT FILE
     C*
     c                   if        (not *in73)
     c                   if        (xxcopy = 1)
     c                   open      qsysprt
     c                   move      *on           xxopen
     c                   endif
     c                   if        (xxcopy = 2)
     c                   open      qsysprt1
     c                   move      *on           xxopn1
     c                   open      qsysprt2
     c                   move      *on           xxopn2
     c                   endif
     c                   endif
     C*
     c                   if        (*in75)
     c                   open      qsysprt2
     c                   move      *on           xxopn2
     c                   eval      xxcopy = 0
     c                   move      *blanks       xxcpy
     c                   endif
     C*
     c                   select
     c                   when      (*in71)
     c     niky          chain     pni01                              99
     c                   when      (*in72)
     c     niky          chain     pni02                              99
     c                   when      (*in73)
     c     lgky          chain     plg01                              99
     c                   when      (*in75)
     c     cmky          chain     pcm01                              99
     c                   endsl
     C*
     c                   dow       (not *in99)
     C*
     c                   do
     C*
     c                   if        (*in71) and
     c                             (niidt <> kkdate) or
     c                             (*in72) and
     c                             (niidt <> kkdate)
     c                   iter
     c                   endif
     c                   if        (*in73)
     c                   if        (lgprsq <> 0) or
     c                             (lgschp <> lgexdt)
     c                   iter
     c                   endif
     c                   endif
     C*
     C*   OPEN THE PRINT FILE
     C*
     c                   if        (*in73)
     c                   open      qsysprt2
     c                   move      *on           xxopn2
     c                   endif
     C*
     c                   exsr      srinit
     C*
     C*  IF THE FUND CHANGED, GET THE NEW FUND INFORMATION.
     C*
     c                   if        (kkfnd <> svfnd)
     c                   exsr      fndbrk
     c                   eval      svfnd = kkfnd
     c                   endif
     C*
     C*  LOAD CLAIMANT INFORMATION
     C*
     c     cmky          chain     pcm01                              99
     c                   if        (*in99)
     c                   leave
     c                   endif
     C*
     c     cdky          chain     pcd01                              99
     c                   if        (*in99)
     c                   leave
     c                   endif
     C*
     c     cmky          chain     pni04                              99
     c                   if        (*in99)
     c                   clear                   pni04
     c                   endif
     C*
     c     cmky          chain     pc101                              99
     c                   if        (*in99)
     c                   clear                   pc101
     c                   endif
     C*
     C* GET EMPLOYER INFO
     C*
     c     emky          chain     pem01                              99
     c                   if        (*in99)
     c                   leave
     c                   endif
     C*
     c                   movea     '00000'       *in(21)
     c                   movea     '00000'       *in(26)
     c                   movea     '00000'       *in(31)
     c                   movea     '000000'      *in(36)
     C*
     C*================================================================
     C* Employee Info
     C*================================================================
     C*
     C*  Box 1 - Name
     C*
     c                   movel(p)  cmlsn         xxclnm
     c                   cat       ',':0         xxclnm
     c                   cat       cmfsn:1       xxclnm
     c                   if        (cmmid <> *blanks)
     c                   cat       cmmid:1       xxclnm
     c                   cat       '.':0         xxclnm
     c                   endif
     c*
     c*  Box 2  - Home phone Number
     c*
     c                   if        cdphn# <> *zeros
     c                   z-add     cdarea        xxxara
     c                   z-add     cdphn#        xxxph#
     c                   eval      xxdphn = xxxphn
     c                   else
     c                   eval      xxdphn = *zeros
     c                   endif
     c*
     c*  Box 3 - Soc Sec #
     C***  Begin Chg  ***  06/09/20  *******************************
     c*                  move      *zeros        xxss#
     c                   move      *blanks       xxss#
     C***  End   Chg  ***  06/09/20  *******************************
     c                   testn                   cmss#                98
     c                   if        (*in98)
     c                   move      cmss#         xxss#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'xxss#'       qqname           25
     C     xxss#         parm      xxss#         qqval           100
     C                   eval      xxss#=displayss#(xxss#)
     C***  End Add  ***  06/09/20  *******************************
     c                   endif
     C*
     C*  Box 5 - Address
     C*
     c                   movel     cdzip         xxzipa
     c                   move      cdzip         xxzipb
     c                   clear                   xxcadr
     C*
     c                   movel(p)  cdadr1        xxcadr
     c                   if        (cdadr2 <> *blanks)
     c     xxcadr        cat       cdadr2:2      xxcadr
     c                   endif
     c                   cat       cdcty:2       xxcadr
     c                   cat       ',':0         xxcadr
     c                   cat       cdst:1        xxcadr
     c                   cat       xxzipa:2      xxcadr
     c                   if        (xxzipb <> '0000')
     c                   cat       '-':0         xxcadr
     c                   cat       xxzipb:0      xxcadr
     c                   endif
     c*
     c*  Box 6 - Sex
     c*
     c                   eval      *in21 = *OFF
     c                   eval      *in22 = *OFF
     c                   select
     c                   when      (cdsex = 'M')
     c                   eval      *in21 = *ON
     c                   when      (cdsex = 'F')
     c                   eval      *in22 = *ON
     c                   endsl
     c*
     c*  Box 6 - Marital Status
     c*  CMMSTS
     c                   eval      *in23 = *OFF
     c                   eval      *in24 = *OFF
     c                   select
     c                   when      (cdmsts = 'M')
     c                   eval      *IN23 = *ON
     c                   when      (cdmsts = 'S')
     c                   eval      *IN24 = *ON
     c                   endsl
     c*
     c*  Box 7 - Number of Dependents
     c*
     c*  CM#DEP
     c*
     c*  Box 8 - Date of Hire
     c*
     c                   if        (cmhidt <> *zeros)
     c                   eval      srdti = cmhidt
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxhidt
     c                   else
     c                   clear                   xxhidt
     c                   endif
     c*
     c*
     c*  Box 9 - Date of Birth
     c*
     c                   if        (cddtob <> *zeros)
     c                   eval      srdti = cddtob
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxdtob
     c                   else
     c                   clear                   xxdtob
     c                   endif
     c*
     c*  Box 10 - Average Earnings per Week
     c*
     c                   eval      *in25 = *OFF
     c                   eval      *in26 = *OFF
     c                   eval(h)   xxaww = (cmaww * 1)
     c                   select
     c                   when      (ni1a2  = 'E')
     c                   eval      *IN25 = *ON
     c                   when      (ni1a2  = 'A')
     c                   eval      *IN26 = *ON
     c                   endsl
     c*
     c*================================================================
     c* Employer Info
     c*================================================================
     c*
     c*  Box 11 - Firm name
     c*
     c* EMFDNM
     c*
     c*  Box 13 - Mailing Address
     c*
     c                   movel     emmzip        xxzipa
     c                   move      emmzip        xxzipb
     c*
     c                   clear                   xxmad1
     c                   clear                   xxmad2
     c                   if        (emmad1 <> *blanks)
     c     emmad1        cat(p)    emmad2:1      xxmad1
     c                   movel(P)  emmcty        xxmad2
     c                   cat       ',':0         xxmad2
     c                   cat       emmst:1       xxmad2
     c                   cat       xxzipa:2      xxmad2
     c                   if        (xxzipb <> '0000')
     c                   cat       '-':0         xxmad2
     c                   cat       xxzipb:0      xxmad2
     c                   endif
     c                   endif
     c*
     c*  Box 14 - Employer phone number
     c                   if        emphn# <> *zeros
     c                   z-add     emarea        xxxara
     c                   z-add     emphn#        xxxph#
     c                   eval      xxephn = xxxphn
     c                   else
     c                   eval      xxephn = *zeros
     c                   endif
     c*
     c*  Box 16 - Carrier Name & Phone Number
     c*  FNName
     c                   if        fnph1  <> *zeros
     c                   z-add     fnph1a        xxxara
     c                   z-add     fnph1         xxxph#
     c                   eval      xxfphn = xxxphn
     c                   else
     c                   eval      xxfphn = *zeros
     c                   endif
     c*
     c*
     c*  Box 18 - Self-Insured? Business Type
     c*
     c                   eval      *in40 = *OFF
     c                   eval      *in39 = *OFF
     c                   IF        (ni1a1  = 'Y')
     c                   eval      *IN40 = *ON
     c                   EndIf
     c                   IF        (ni1a1  = 'N')
     c                   eval      *IN39 = *ON
     c                   EndIf
     c*
     c                   eval      *in28 = *OFF
     c                   eval      *in29 = *OFF
     c                   eval      *in30 = *OFF
     c                   eval      *in31 = *OFF
     c                   eval      *in32 = *OFF
     c                   movel(p)  ni20A7        xxBtyp
     c                   select
     c                   when      (xxbtyp = 'S')
     c                   eval      *IN28 = *ON
     c                   when      (xxbtyp = 'W')
     c                   eval      *IN29 = *ON
     c                   when      (xxbtyp = 'M')
     c                   eval      *IN30 = *ON
     c                   when      (xxbtyp = 'R')
     c                   eval      *IN31 = *ON
     c                   when      (xxbtyp = 'O')
     c                   eval      *IN32 = *ON
     c                   other
     c                   eval      *IN32 = *ON
     c                   endsl
     c*
     c*
     c* NI20A2
     c*
     c*================================================================
     c* Injury or Illness Info
     c*================================================================
     c*
     c*  Box 20 - Date of Injury
     c*
     c                   eval      srdti = cmacdt
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxacdt
     C*
     C*  Box 21 - On employer's premises?
     C*
     c                   eval      *IN33 = *Off
     c                   eval      *IN34 = *Off
     c                   setoff                                       33
     c                   if        (cmeadr = 'Y')
     c                   eval      *IN33 = *ON
     c                   else
     c                   eval      *IN34 = *ON
     c                   endif
     c*
     c*  Box 22 - Location where event or exposure
     c*
     c*
     c                   MOVE      *BLANKS       xxaadr
     c                   if        (ni40a1 <> *blanks)
     c                   movel(P)  ni40a1        xxaadr
     c     xxaadr        cat       ni25a1:2      xxaadr
     c                   cat       ',':0         xxaadr
     c                   cat       NI2a1         xxaadr
     c                   Z-ADD     ni9n2         xxzip
     c                   move      *blanks       xxzp1
     c                   move      *blanks       xxzp2
     c                   move      xxzpm1        xxzp1
     c                   move      xxzpm2        xxzp2
     c                   cat       xxzp1:2       xxaadr
     c                   If        xxzp2 <> *zeros
     c                   cat       '-':0         xxaadr
     c                   cat       xxzp2:0       xxaadr
     c                   endif
      *
     c                   else
      *
     c                   if        (cmacdr <> *blanks)
     c                   move      cmacz1        xxzipa
     c                   move      cmacz2        xxzipb
     c     cmacdr        cat       cmacty:2      xxaadr
     c                   cat       ',':0         xxaadr
     c                   cat       cmacst:1      xxaadr
     c                   cat       xxzipa:2      xxaadr
     c                   if        (xxzipb <> '0000')
     c                   cat       '-':0         xxaadr
     c                   cat       xxzipb:0      xxaadr
     c                   endif
     c                   endif
      *
     c                   endif
     C*
     C*  Box 23 - Fisrt day of total/partial Incapacity.
     C*
     c                   eval      srdti = cmdbdt
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxbtdt
     C*
     C*  Box 24 - Fifth day of total/partial Incapacity.
     C*
     C*
     c                   eval      srdti = nidt1
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxdt1
     c*
     c                   setoff                                       27
     c                   movel     cmactm        xxactm
     c                   movel     xxactm        test1
     c                   if        (test1 >= 12)
     c                   seton                                        27
     c                   if        (test1 > 12)
     c                   eval      xxactm = (xxactm - 1200)
     c                   endif
     c                   else
     c                   if        (test1 = 0)
     c                   eval      xxactm = (xxactm + 1200)
     c                   endif
     c                   endif
     C*
     C*  Box 25 - Date of Death
     C*
     c                   move      *zeros        xxdod
     c                   if        (cmdod <> *zeros)
     c                   eval      srdti = cmdod
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxdod
     c                   endif
     c*
     c*  Box 29 - Date reported
     c*
     c                   eval      srdti = cmdtrp
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxdtrp
     c*
     c*  Box 30 - Date reported as work related.
     c*
     c                   eval      srdti = nidt2
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxdt2
     c*
     c                   if        (cmdtlw <> *zeros)
     c                   eval      srdti = cmdtlw
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxdtlw
     c                   else
     c                   clear                   xxdtlw
     c                   endif
     c*
     c*  Box 31, 32 - Body Part Codes, Witnesses
     c*
     c                   movel(P)  ni20a6        xx10a6
     c*
     c                   movel(p)  ni80a3        xxwit1
     c                   move(p)   ni80a3        xxwit2
     c*
     c                   clear                   xx30a4
     c                   movel(p)  cmnatr        kkkey
     c     tbky          chain     pna01                              99
     c                   if        (not *in99)
     c                   movel(p)  nadesc        xx30a4
     c                   endif
     C*
     c                   clear                   xx30a5
     c                   movel(p)  cmpart        kkkey
     c     tbky          chain     ppa01                              99
     c                   if        (not *in99)
     c                   movel(p)  padesc        xx30a5
     c                   endif
     C*
     c                   clear                   xx30a6
     c                   movel(p)  cmcaus        kkkey
     c     tbky          chain     pca01                              99
     c                   if        (not *in99)
     c                   movel(p)  cadesc        xx30a6
     c                   endif
     c*
     c*  Box 34 - Date returned to work.
     c*
     c                   eval      *IN35 = *Off
     c                   eval      *IN36 = *Off
     c                   setoff                                       33
     c                   if        (ni1a3  = 'Y')
     c                   eval      *IN35 = *ON
     c                   else
     c                   eval      *IN36 = *ON
     c                   endif
     c*
     c                   eval      srdti = cmrtw
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxrtw
     c*
     c*  Box 36 - Has employe returned to regular occupation?
     c*
     c                   eval      *IN37 = *Off
     c                   eval      *IN38 = *Off
     c                   setoff                                       33
     c                   if        (ni1a4  = 'Y')
     c                   eval      *IN37 = *ON
     c                   else
     c                   eval      *IN38 = *ON
     c                   endif
     c*
     c*  Box 37 & 38- Preparer Name & title
     c*
     C                   if        niprnm <> *blanks
     C*
     C                   eval      niprnm = %trim(niprnm)
     c                   eval      pos1 = 1
     c                   eval      pos2 = %scan(',':niprnm:pos1)
     C                   eval      length = pos2 - pos1
     C                   if        length > 0
     c                   eval      xxprnm = %subst(niprnm:pos1:length)
     c                   endif
     C*
     c                   eval      pos1 = pos2 + 1
     c                   eval      xxprti = %subst(niprnm:pos1:19)
     C*
     C                   else
     c                   clear                   xxprnm
     c                   clear                   xxprti
     c                   endif
     c*  Box 40 - Prep date of of This Report
     c*
     c*   DSDATE
     c*
     c                   if        niprep <> *zeros
     c                   z-add     dsdate        xxprep
     c                   z-add     niprep        srdti
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         xxprep
     c                   else
     c                   z-add     *zeros        xxprep
     c                   endif
     c*
     c*================================================================
     c                   if        (*in71)
     c                   except    noi
     c                   else
     c                   except    noi2
     c                   endif
     C*
     C*  PRINT A LABEL FOR THE EMPLOYEE COPY ONLY.
     C*
     c                   if        (*in72)
     C*
     c                   movel(p)  cdlsn         xxcnam
     c     xxcnam        cat       ',':0         xxcnam
     c     xxcnam        cat       cdfsn:1       xxcnam
     c     xxcnam        cat(p)    cdmid:1       xxcnam
     C*
     c                   move      cdcty         xxcty
     c                   move      cdst          xxstat
     c                   eval      xxzip = cdzip
     c                   exsr      sraddr
     c                   move      *blanks       xxccsz
     c                   movel     xxcsz         xxccsz
     c     cdadr2        comp      *blanks                                42
     C*
     c                   move      *blanks       xxmc
     c                   if        (cmmco = 'Y')
     c                   movel     'MC'          xxmc
     c                   endif
     C*
     c                   except    label
     c                   endif
     C*
     C*  CLOSE THE LABEL SPOOL FILE IF THE EMPLOYEE COPY WAS PROCESSED.
     C*
     c                   if        (*in73) and
     c                             (xxopn2 = *on)
     c                   close     qsysprt2
     c                   move      *off          xxopn2
     c                   endif
     C*
     C*  WRITE THE DARTS REMARK, THEN PRINT THE NOI FORM.
     C*
     c                   if        (not *in75)
     c                   if        (not *in73)
     c                   exsr      darts
     c                   else
     c                   exsr      srdart
     c                   endif
     c                   endif
     C*
     c                   enddo                                                  LEAVE
     C*
     C*  GET THE NEXT RECORD
     C*
     c                   select
     c                   when      (*in71)
     c     niky          reade     pni01                                  99
     c                   when      (*in72)
     c     niky          reade     pni02                                  99
     c                   when      (*in73)
     c     lgky          reade     plg01                                  99
     c                   when      (*in75)
     c                   seton                                        99
     c                   endsl
     C*
     c                   enddo
     C*
     C*  CLOSE THE LABEL SPOOL FILE IF THE EMPLOYEE COPY WAS PROCESSED.
     C*
     c   71              if        (xxopen = *on)
     c                   close     qsysprt
     c                   move      *off          xxopen
     c                   endif
     c   72              if        (xxopn1 = *on)
     c                   close     qsysprt1
     c                   move      *off          xxopn1
     c                   endif
     c   72              if        (xxopn2 = *on)
     c                   close     qsysprt2
     c                   move      *off          xxopn2
     c                   endif
     c   75              if        (xxopn2 = *on)
     c                   close     qsysprt2
     c                   move      *off          xxopn2
     c                   endif
     C*
     C*  PROCESS THE NEXT COPY
     C*
     c                   if        (xxcopy = 1)
     c                   eval      xxcopy = (xxcopy + 1)
     c                   else
     c                   leave
     c                   endif
     C*
     c                   enddo
     C*
     C*  END OF PROGRAM
     C*
     c                   seton                                        lr
     C*****************************************************************
     C* SRINIT SUBROUTINE              ** Subroutine **   S R I N I T *
     C* THIS SUBROUTINE INITIALIZES ALL WORK FIELDS                   *
     C*****************************************************************
     c     srinit        begsr
     C*
     c                   if        (not *in75)
     c                   if        (not *in73)
     c                   eval      kkfnd = nifnd
     c                   eval      kkfyr = nifyr
     c                   eval      kkcase = nicase
     c                   else
     c                   eval      kkfnd = lgfnd
     c                   eval      kkfyr = lgfyr
     c                   eval      kkcase = lgcase
     c                   endif
     c                   endif
     C*
     c                   endsr
     C*****************************************************************
     C* SRADDR SUBROUTINE              ** Subroutine **   S R A D D R *
     C* THIS SUBROUTINE PUTS TOGETHER AN ADDRESS INTO ONE STRING      *
     C* CONTAINING THE CITY, STATE AND ZIP                            *
     C*****************************************************************
     c     sraddr        begsr
     C*
     c                   move      *blanks       sz
     c                   move      *blanks       st
     c                   move      *blanks       xxzp1
     c                   move      *blanks       xxzp2
     c                   movel     xxstat        xxst
     c                   movea     xxst          sz
     c                   move      xxzpm1        xxzp1
     c                   movea     xxzp1         sz(4)
     c                   if        (xxzpm2 <> 0)
     c                   move      '-'           sz(9)
     c                   move      xxzpm2        xxzp2
     c                   movea     xxzp2         sz(10)
     c                   endif
     C*
     c                   move      *blanks       xxsz
     c                   movea     sz            xxsz
     c                   move      *blanks       xxlast
     c                   move      *blanks       xxfrst
     c                   movel     xxcty         xxlast
     c                   movel     xxsz          xxfrst
     c                   move      '1'           xxterm
     c                   call      'SRNAME'
     c                   parm                    xxlast
     c                   parm                    xxfrst
     c                   parm                    xxterm
     c                   movel     xxlast        xxcsz
     C*
     c                   endsr
     C*****************************************************************
     C* FNDBRK SUBROUTINE              ** Subroutine **   F N D B R K *
     C* THIS SUBROUTINE GETS THE FUND INFORMATION.                    *
     C*****************************************************************
     c     fndbrk        begsr
     C*
     C*   GET REMARK CODE FOR DIVISION DIARY REMARKS.
     C*
     c     fnky          chain     pcx01                              99
     c                   move      *blanks       xxrmcd
     c                   if        (not *in99)
     c                   move      cxrsrd        xxrmcd
     c                   endif
     C*
     C*  GET THE FUND NAME AND ADDRESS
     C*
     c     fnky          chain     pfn01                              99
     C*
     c                   movel(p)  fnname        xxfnme
     C*
     c                   clear                   xxfadr
     c                   movel(p)  fnmad1        xxfadr
     c                   move      fnmzip        xxfmzp
     c                   if        (fnmad2 <> *blanks)
     c     xxfadr        cat       fnmad2:2      xxfadr
     c                   endif
     c                   cat       fnmcty:2      xxfadr
     c                   cat       ',':0         xxfadr
     c                   cat       fnmst:1       xxfadr
     c                   cat       xxfmzp:2      xxfadr
     C*
     C*  GET THE CARRIER CODE NUMBER FROM FUND AUXILIARY FILE
     C*
     c     fnky          chain     pf201                              99
     C*
     c                   if        (not *in99)
     c                   movel(p)  f2sctc        xxsctc
     c                   else
     c                   move      *zeros        xxsctc
     c                   endif
     C*
     c                   endsr
     C*****************************************************************
     C* DARTS SUBROUTINE               ** Subroutine **   D A R T S   *
     C* THIS SUBROUTINE WRITES A DARTS REMARK FOR EACH NOI FORM       *
     C* PRINTED.                                                      *
     C*****************************************************************
     c     darts         begsr
     C*
     C*  ESTABLISH KEY
     C*
     c                   move      *blanks       cmkey
     c                   move      *blanks       qckey
     c                   move      kkco#         qcco#
     c                   move      kkfnd         qcfnd
     c                   move      kkfyr         qcfyr
     c                   move      kkcase        qccase
     C*
     C*  DO NOT WRITE A DARTS RECORD IF THE REMARK CODE WAS NOT SETUP.
     C*
     c                   if        (xxrmcd <> *blanks)
     C*
     c                   clear                   pcm02
     c                   movel     qckey         cmkey
     c                   move      *blanks       cmcomm
     c   71              movel     cpy(1)        cmcomm
     c   72              movel     cpy(2)        cmcomm
     c                   move      *blanks       cmcsid
      *
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         cmcdat
     c                   move      *blanks       cmcode
     c                   move      *blanks       cmdcid
     c                   movel     'W0070R'      cmpnam
      *
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         cmtrdt
      *
     c                   eval      cmtrtm = dstime
     c                   move      *zeros        cmsecl
     c                   move      'N'           cmsbyl
     c                   move      *blanks       cmstat
     c                   movel     q1user        cmuser
     c                   movel     'W8070RFL'    cmpgm
     c                   eval      cmtime = dstime
      *
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         cmdt
      *
     c                   movel     q1user        cmiusr
     c                   move      *blanks       cmfold
     c                   move      *zeros        cmdcdt
     c                   move      *blanks       cmdifl
     c                   move      'WC'          cmapid
     c                   move      *blanks       cmidoc
     c                   move      *blanks       cmique
     c                   move      *blanks       cmimgf
     c                   move      *blanks       cmipth
     c                   move      *zeros        cmpidx
     c                   move      *blanks       cmqpty
     c                   move      *blanks       cmdctp
     c                   movel     'W8070RFL'    cmipgm
     c                   move      *blanks       cmsmcd
     c                   move      *blanks       cmfl01
     c                   move      *blanks       cmfl02
     c                   move      *blanks       cmfl03
     c                   move      *blanks       cmfl04
     c                   move      *blanks       cmfl05
     c                   move      xxrmcd        cmrmcd
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         cmidt
      ***** BEG ADD ***** 10/17/11 ***********************************
     c                   eval      cmuncn = *blanks
     c                   eval      cmifsp = *blanks
      ***** END ADD ***** 10/17/11 ***********************************
     C*
     C*  GET LAST SEQ# USED
     C*
     c     c3ky          setgt     pcm03
     c     c3ky          readpe    pcm03                                  99
     c                   if        (not *in99)
     c                   eval      cmseq# = (c3seq# + 1)
     c                   else
     c                   eval      cmseq# = 1
     c                   endif
     C*
     c                   write     pcm02
     c                   feod      sdcmld
     c                   endif
     C*
     c                   endsr
     C*****************************************************************
     C* SRDART SUBROUTINE              ** Subroutine **   S R D A R T *
     C* THIS SUBROUTINE WRITES A DARTS REMARK FOR EACH REPORT WHICH   *
     C* IS PROCESSED.                                                 *
     C*****************************************************************
     c     srdart        begsr
     C*
     C*  DON'T CREATE THE DARTS REMARK IF THE HARDCOPY DARTS CODE IS
     C*  BLANK OR INVALID.
     C*
     c                   move      '148'         kktset
     C*** BEG CHG ****** 03/24/10 *************************************
     c***  ixky          chain     pix01                              99
     c                   call      'WA076R'      ixapi                  60
     c                   if        @rtncd = 'Y'
     c                   move      '0'           *in99
     c                   else
     c                   move      '1'           *in99
     c                   endif
     c*** END CHG ****** 03/24/10 *************************************
     c                   if        (not *in99)
     C*
     C*  ESTABLISH KEY
     C*
     c                   move      *blanks       qckey
     c                   move      kkco#         qcco#
     c                   move      kkfnd         qcfnd
     c                   move      kkfyr         qcfyr
     c                   move      kkcase        qccase
     C*
     C*  ESTABLISH RECORD
     C*
     c                   clear                   pcm02
     c                   movel     qckey         cmkey
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         cmcdat
     c                   movel     ixhcpr        cmrmcd
     C*
     c     cmrmcd        chain     pct01                              99
     c                   if        (not *in99)
     C*
     c                   move      ctsmcd        cmsmcd
     c                   movel     cpy(3)        cmcomm
     C*
     C*  GET FOLDER NAME
     C*
     C*
     c                   movel     kkfnd         ##fnd                          for Fund
     c                   movel(p)  'C'           ##fold                         means "Member"
     c                   move      cmcase        ##fold
     C*
     c                   call      'SRFBLD'
     c                   parm                    ##fnd
     c                   parm      'CS'          ##ser
     c                   parm      cttype        ##sub
     c                   parm                    ##fold
     c                   parm      *blanks       ##fpth                         Built, returned
     c                   parm      *blanks       ##term
     C*
     c                   movel(p)  ##fpth        cmfold
     C*
     C*  GET DOCUMENT ID
     C*
     c                   call      'SRDBLD'
     c                   parm                    ##fpth                         From SRFBLD routine
     c                   parm      cttype        ##rt                           Rmrk Type for Doc ID
     c                   parm      *blanks       ##doc                          Built, returned
     c                   parm      *blanks       ##term
     C*
     c                   movel(p)  ##doc         cmdcid
      ***** BEG ADD ***** 10/17/11 ***********************************
     c                   if        cmfold <> ' ' or cmdcid <> ' '
     c                   eval      cmuncn = '/' + %trim(root) + '/' +
     c                                      %trim(cmfold) + '/' +
     c                                      %trim(cmdcid)
     c                   eval      cmifsp = env
     c                   else
     c                   eval      cmuncn = ' '
     c                   eval      cmifsp = ' '
     c                   endif
      ***** END ADD ***** 10/17/11 ***********************************
     c                   movel(p)  'W0070R'      cmpnam
     C*
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         cmtrdt
     C*
     c                   eval      cmtrtm = dstime
     c                   move      'N'           cmsbyl
     c                   movel     lgiusr        cmuser
     c                   movel     q1pgm         cmpgm
     c                   eval      cmtime = dstime
     C*
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         cmdt
     C*
     c                   movel     lgiusr        cmiusr
     c                   move      'WC'          cmapid
     c                   movel     q1pgm         cmipgm
     C*
     c                   eval      srdti = dsdate
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         cmidt
     C*
     C*  GET LAST SEQ# USED
     C*
     c     c3ky          setgt     pcm03
     c     c3ky          readpe    pcm03                                  99
     c                   if        (not *in99)
     c                   eval      cmseq# = (c3seq# + 1)
     c                   else
     c                   eval      cmseq# = 1
     c                   endif
     C*
     c                   write     pcm02
     c                   feod      sdcmld
     C*
     C*  LOAD THE DARTS SEQ# INTO THE EDI LOG FILE
     C*
     c     lgky1         chain     plg02                              99
     c                   if        (not *in99)
     c                   eval      lgprsq = cmseq#
     c                   update    plg02
     c                   endif
     C*
     C*  CALL W8070C1FL TO RESOLVE THE OUTPUT INTO AN OFFICE DOCUMENT
     C*
     c                   call      'W8070C1F'
     c                   parm                    q1job
     c                   parm                    q1user
     c                   parm                    q1jobn
     c                   parm      cmdcid        ##dcid
     c                   parm      cmfold        ##fldr
     C*
     c                   endif
     c                   endif
     C*
     c                   endsr
     oqsysprt   e            noi              01
      *
      *  Employee Name, phone, SS# and gender
     o          e            noi              22
     o          e            noi         1
     o                       CMOCDS              55
      *  37 - Employee Preparer Name and Title.
     o          e            noi              72
     o                       xxprnm              34
     o                       xxprti              84
      *  40 - Date prepared
     o          e            noi              75
     o                       xxprep              95 '  /  /    '
     o*
     o* ================================================================= *
     o* Now print back page of form, has no fields
     o* ================================================================= *
     o          e            noi               6
     o                                            5 '  '
     o*
     o*
     oqsysprt1  e            LABEL          1 11
     o                       xxcnam              50
     o          e            LABEL       1
     o                       CDADR1              45
     o          e      N42   LABEL       1
     o                       CDADR2              45
     o          e            LABEL       1
     o                       XXCCSZ              45
     o                       XXMC                 2
      *
     oqsysprt2  e            noi2              1
      *
      *  Box 1,2,3,4 - Employee Name, phone, SS# and gender
     o          e            noi2             22
     o                       xxclnm              60
     o                                           62 '('
     o                       xxdphn              74 '   )   -    '
     C***  Begin Chg  ***  06/09/20  *******************************
     o*                      xxss#               90 '   -  -    '
     o                       xxss#               90
     C***  End Chg  ***  06/09/20  *******************************
     o                  21                       97 'X'                         Male
     o                  22                      105 'X'                         Female
      *
      *  Box 5,6,7 - Employee address martial Status, Nbr of dependents
     o          e            noi2             25
     o                       xxcadr              74
     o                  23                       80 'X'                         Male
     o                  24                       88 'X'                         Female
     o                       CM#DEP        z    110
      *  Box 8,9,10 - Hire date, Date of birth, Average weekly wage
     o          e            noi2             28
     o                       xxhidt              30 '  /  /    '
     o                       xxdtob              60 '  /  /    '
     o                       xxaww         1     90
     o                  25                       95 'X'
     o                  26                      106 'X'
      *  Box 11, 12 - Employer Name and FEIN
     o          e            noi2             31
     o                       EMFDNM              53
     o                       EMFEIN             100 '  -       '
      *  Box 13,14,15 - Employer address, phone #, Industry Code
     o          e            noi2             33
     o                       xxmad1              74
     o                                           88 '('
     o                       xxephn             100 '   )   -    '
     o          e            noi2        1
     o                       xxmad2              74
     o          e            noi2        1
     o                       ni2a1              105
      *  Box 16, 17 - Carrier Info, WC Policy Number
     o          e            noi2             37
     o                       fnname              74
     o                                           63 ' ('
     o                       xxfphn              75 '   )   -    '
     o                       ni20a1             100
      *  Box 18, 19 - Self-Insured, Business Type
     o          e            noi2             39
     o                  40                       26 'X'
     o                  39                       34 'X'
     o                  28                       90 'X'
     o                  29                       98 'X'
     o                  30                      107 'X'
     o          e            noi2        2
     o                       ni40A3              72
     o                  31                       80 'X'
     o                  32                       87 'X'
     o                  32   ni20a7             113
     o*
      * Box 20 - Injury Info - accident Date
     o          e            noi2             43
     o                       xxacdt              56 '  /  /    '
     o*
      * Box 21, 22 - Injury Info - On employer Premises? Location
     o          e            noi2             44
     o               33                          48 'X'
     o               34                          56 'X'
     o          e            noi2        1
     o                       xxaadr             112
      * Box 22, 24 - First day of total/partial incapacity - Fifth day?
     o          e            noi2             48
     o                       xxbtdt              35 '  /  /    '
     o                       xxdt1               85 '  /  /    '
      * Box 25, 26 - Injury Info - Employee death Date?  Source of Injury
     o          e            noi2             51
     o                       xxdod               40 '  /  /    '
     o                       ni60a1             127
     o          e            noi2        1
     o                       ni60a2             127
      *
      *  Box 27 - How Injury Occurred
     o          e            noi2             54
     o                       ni80a1              94
     o          e            noi2        1
     o                       xx30a4              44
     o                       xx30a5              77
     o                       xx30a6             110
      *  Box 28 - Person to whom injury was reported? Dates
     o          e            noi2             59
     o                       NICONT              53
     o                       xxdtrp              75 '  /  /    '
     o                       xxdt2              110 '  /  /    '
      *  Box 31, 32 - Injury codes, Body parts, Witnesses to injury
     o          e            noi2             61
     o                       NI10A1              25
     o                       ni10a4              49
     o                       xxwit1             105
     o          e            noi2        2
     o                       NI10A2              25
     o                       ni10a5              49
     o                       xxwit2             105
     o          e            noi2        2
     o                       NI10A3              25
     o                       xx10a6              49
      *  Box 33, 34 - Returned to work, Date returned
     o          e            noi2             66
     o               35                          39 'X'
     o               36                          46 'X'
     o          e            noi2        1
     o                       xxrtw              110 '  /  /    '
      *  Box 35,36 - Employee regular Occupation.
     o          e            noi2             69
     o               37                          97 'X'
     o               38                         106 'X'
     o          e            noi2        1
     o                       CMOCDS              54
      *  Box 37, 38 - Employee Preparer Name and Title.
     o          e            noi2             72
     o                       xxprnm              34
     o                       xxprti              84
      *  Box 40 - Date prepared
     o          e            noi2             75
     o                       xxprep              95 '  /  /    '
     o*
     o* ================================================================= *
     o* Now print back page of form, has no fields
     o* ================================================================= *
     o          e            noi2              6
     o                                            5 '  '
     o*
     o*
**  COPY MSG
EMPLOYER COPY
EMPLOYEE COPY
DIVISION COPY
