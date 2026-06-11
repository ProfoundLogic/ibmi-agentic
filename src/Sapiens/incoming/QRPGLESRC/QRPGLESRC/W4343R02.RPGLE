     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H**************************************************************************************
     H/TITLE W4343R02 - Work with attorney orders maintenance                              *
     H Bnddir('WTBNDDIR')
     H**************************************************************************************
     H**************************************************************************************
     H*  Description: This program maintenance the attorney orders    *
     H*               (Add, Change, Display on a claim                *
     H**************************************************************************************
     H*  Created: 02/2010    Programmer C:247   Project: 25WPT                             *
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H* 08/25/10 C:183 Project: 25WPT(Priss#36)                                            *
     H*                 Added edit for duplicate Seq# for a CO#/Fund/Year/Case             *
     H*                                                                                    *
     H* 06/23/10 C:190 Project: 25WPT(Priss#33, 34, 35)                                    *
     H*                 Corrected position cursor for Attorney FEIN.                       *
     H*                 Made Start and End date as required field.                         *
     H*                 Edit for Order date - It cannot be less than Start/End date.       *
     H*                 Restrict duplicate records.                                        *
     H**************************************************************************************
    H*
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     Fw4343f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwmaop     uf a e             disk
     F                                     rename(pao01:dbrcdf)
     Fwmpdl     if   e           k disk
     Fwmfnl     if   e           k disk
     Fwmcdl     if   e           k disk
     Fwmcml1    if   e           k disk
     Fwmeml     if   e           k disk
     F*** beg   add *** 06/23/10 **************************************
     Fwmaol4    if   e           k disk
     F*** end   add *** 06/23/10 **************************************
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   'Work with attorney orders'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
      *

      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************

     D                 DS
     D  CMSS#                  1      9
     D  SS1                    1      3
     D  SS2                    4      5
     D  SS3                    6      9
     D*
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    qqco#             3
     C                   parm                    qqfnd             3
     C                   parm                    qqfyr             3
     C                   parm                    qqcase            7
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
      *
     C     fnky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
      *
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
      *
     C     emky          klist
     C                   kfld                    kkco#
     C                   kfld                    cmfnd
     C                   kfld                    cmemp#
     C                   kfld                    cmdiv
     C*** beg   add *** 06/23/10 **************************************
     C     aoky          klist
     C                   kfld                    kkco#
     C                   kfld                    KKfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C                   kfld                    kkordt
     C                   kfld                    kkfein
     C                   kfld                    kkmod
     C                   kfld                    kkstdt
     C                   kfld                    kkendt
     C                   kfld                    kkfamt
     C                   kfld                    kkptop
     C*** end   add *** 06/23/10 **************************************
     C*** Beg   add *** 08/25/10 **************************************
     C     aoky1         klist
     C                   kfld                    kkco#
     C                   kfld                    KKfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C*** End   add *** 08/25/10 **************************************
    C*================================================================
    C*================================================================
    C*  *LIKE DEFN statements
     C     *LIKE         DEFINE    AOFEIN        XXFEIN
     C     *LIKE         DEFINE    AOMOD         XXMOD
     C     *LIKE         DEFINE    AODT          XXFROM
     C     *LIKE         DEFINE    AODT          XXTO
     C     *LIKE         DEFINE    AOCO#         KKCO#
     C     *LIKE         DEFINE    AOFND         KKFND
     C     *LIKE         DEFINE    AOFYR         KKFYR
     C     *LIKE         DEFINE    AOCASE        KKCASE
     C     *LIKE         DEFINE    CDSS#         XXSS#
     C*** beg   add *** 06/23/10 **************************************
     C     *like         define    aoseq#        kkseq#                         Sequence Number
     C     *like         define    aodt          kkordt                         Order Date
     C     *like         define    aofein        kkfein                         FEIN
     C     *like         define    aomod         kkmod                          Modifier
     C     *like         define    aodt          kkstdt                         Start Date
     C     *like         define    aodt          kkendt                         End Date
     C     *like         define    aofamt        kkfamt                         % to pay
     C     *like         define    aoptop        kkptop                         Fee Amount
     C     *like         define    aodt          xxfrom1
     C     *like         define    aodt          xxto1
     C     *like         define    aoseq#        @tseq#                         Sequence Number
     C     *like         define    aodt          @tordt                         Order Date
     C     *like         define    aofein        @tfein                         FEIN
     C     *like         define    aomod         @tmod                          Modifier
     C     *like         define    aodt          @tstdt                         Start Date
     C     *like         define    aodt          @tendt                         End Date
     C     *like         define    aofamt        @tfamt                         % to pay
     C     *like         define    aoptop        @tptop                         Fee Amount
     C*** end   add *** 06/23/10 **************************************
    C*================================================================
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C                   setoff                                       90
     C* ATTORNEY FEIN / MOD ARE REQUIRED
     C*
     C     SCATTF        IFEQ      *ZEROS
     C                   seton                                        3190
     C                   endif
     C*  IF THE ATTORNEY FEIN ENTERED, VALIDATE
     C*
     C     SCATTF        IFNE      *ZERO
     C                   MOVE      SCATTF        XXFEIN
     C                   MOVE      SCATTM        XXMOD
     C     PDKEY         CHAIN     PPD01                              22
     C  N22PDSTAT        COMP      'I'                                    22
     C                   If        *in22 = *on
     C                   seton                                        90
     c                   endif
     c                   endif
     C*  ORDER DATE IS REQUIRED
     C*
     C     SCORDT        IFEQ      *ZERO
     C                   seton                                        2790
     C                   endif
     C*  ORDER DATE MUST BE A VALID DATE
     C*
     C     SCORDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCORDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCORDT
     C*
     C     SRERR         IFEQ      *ON
     C                   seton                                        2890
     C                   endif
     C                   endif
     C*** beg   add *** 06/23/10 **************************************
     C*  START DATE IS REQUIRED
     C     scstdt        ifeq      *zero
     C                   seton                                        5290
     C                   endif
     C*** end   add *** 06/23/10 **************************************
     C*  START DATE MUST BE A VALID DATE
     C*
     C     SCSTDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCSTDT
     C*
     C     SRERR         IFEQ      *ON
     C                   seton                                        2690
     c                   endif
     c                   endif
     C*** beg   add *** 06/23/10 **************************************
     C*  TO date is required
     C     scendt        ifeq      *zero
     C                   seton                                        5390
     C                   endif
     C*** end   add *** 06/23/10 **************************************
     C*  TO DATE MUST BE A VALID DATE
     C*
     C     SCENDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCENDT
     C*
     C     SRERR         IFEQ      *ON
     C                   seton                                        2990
     C                   endif
     C                   endif
     C*  START DATE MAY NOT BE GREATER THAN END DATE
     C*
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFROM
     C*
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTO
     C*
     C     SCENDT        IFGT      *ZERO
     C     XXFROM        ANDGT     XXTO
     C                   seton                                        3090
     C                   endif
     C*** beg   add *** 06/23/10 **************************************
     C*  Order date must be greater than to date
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFROM1
     C*
     C                   Z-ADD     SCORDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTO1
     C*
     C     xxfrom1       ifge      xxto1
     C                   seton                                        5490
     c                   endif

     c*  Record must not already exist
     c                   eval      kkseq# = scseq#                              Sequence Number
     C*** Beg   add *** 08/25/10 **************************************
     c*  Check the Sequence# for duplicate. If a record already exists
     c*  for CO#/FND/FYR/CASE/SEQ# then flag it as duplicate.
     c*
     c     aoky1         chain     pao01                              60
     c  n90              if        *in60 = *off and @@mode = 'A'
     c                   seton                                        87  90
     c                   endif

     c*  If in Change mode compare Seq# changed on the screen.
     c  n90              if        @@mode = 'C'

     c                   eval      @@chg = *blanks
     c                   if        @tseq# <> scseq#
     c                   move      'Y'           @@chg             1
     c                   endif                                                  @tseq# <> scseq# or
     c*
     c* If changed record already exist, do allow to save it.
     c  n90              if        *in60 = *off and @@chg = 'Y'
     c                   seton                                        87  90
     c                   endif                                                  *in60 = *off and

     c                   endif                                                  @@mode = 'C'
     C*** End   add *** 08/25/10 **************************************
     c                   eval      kkordt = xxto1                               Order Date
     c                   eval      kkfein = xxfein                              FEIN
     c                   eval      kkmod  = xxmod                               Modifier
     c                   eval      kkstdt = xxfrom                              Start Date
     c                   eval      kkendt = xxto                                End Date
     c                   eval      kkfamt = scprpy                              % to pay
     c                   eval      kkptop = scfamt                              Fee Amount
     c     aoky          chain     pao01                              60
     c  n90              if        *in60 = *off and @@mode = 'A'
     c                   seton                                        86  90
     c                   endif

     c*  If in Change mode compare anything changed on the screen.
     c  n90              if        @@mode = 'C'

     C                   z-add     @tordt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         @sordt            8 0

     C                   z-add     @tstdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         @sstdt            8 0

     C                   z-add     @tendt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         @sendt            8 0

     c                   eval      @@chg = *blanks
     c                   if        @tseq# <> scseq# or
     c                             @sordt <> scordt or
     c                             @tfein <> scattf or
     c                             @tmod  <> scattm or
     c                             @sstdt <> scstdt or
     c                             @sendt <> scendt or
     c                             @tfamt <> scprpy or
     c                             @tptop <> scfamt
     c                   move      'Y'           @@chg             1
     c                   endif                                                  @tseq# <> scseq# or
     c*
     c* If changed record already exist, do allow to save it.
     c  n90              if        *in60 = *off and @@chg = 'Y'
     c                   seton                                        86  90
     c                   endif                                                  *in60 = *off and

     c                   endif                                                  @@mode = 'C'
     c*** end   add *** 06/23/10 **************************************
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   Z-ADD     AOFEIN        SCATTF
     C                   Z-ADD     AOMOD         SCATTM
     C                   movel     AOSEQ#        SCSEQ#
     C                   Z-ADD     AOORDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCORDT
     C                   Z-ADD     AOSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSTDT
     C                   Z-ADD     AOENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCENDT
     C                   Z-ADD     AOPTOP        SCFAMT
     C                   Z-ADD     AOFAMT        SCPRPY
     c*** beg   add *** 06/23/10 **************************************
     c** Save Database value if in change mode
     c                   if        @@mode = 'C'
     c                   eval      @tseq# = aoseq#
     c                   eval      @tordt = aoordt
     c                   eval      @tfein = aofein
     c                   eval      @tmod  = aomod
     c                   eval      @tstdt = aostdt
     c                   eval      @tendt = aoendt
     c                   eval      @tfamt = aofamt
     c                   eval      @tptop = aoptop
     c                   endif

     C                   move      aofein        xxfein
     C                   move      aomod         xxmod
     C     pdkey         chain     ppd01                              60
     c                   if        *in60 = *off
     C                   exsr      getnm
     C                   movel     xxatnm        scatnm
     c                   endif
     C*** end   add *** 06/23/10 **************************************
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
      *
     C                   Z-ADD     SCATTF        AOFEIN
     C                   Z-ADD     SCATTM        AOMOD
     C*
     C                   Z-ADD     SCORDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AOORDT
     C*
     C                   Z-ADD     SCSTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AOSTDT
     C*
     C                   Z-ADD     SCENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AOENDT
     C*
     C                   MOVE      Q1USER        AOUSR
     C                   MOVE      Q1PGM         AOPGM
     C                   TIME                    AOTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AODT
     C*
     C                   Z-ADD     SCFAMT        AOPTOP
     C                   Z-ADD     SCPRPY        AOFAMT
     C                   MOVE      QQCO#         AOCO#
     C                   MOVE      SCFND         AOFND
     C                   MOVE      SCCASE        AOCASE
     C                   MOVE      SCFYR         AOFYR
     C                   MOVEl     SCSEQ#        AOSEQ#
     C                   MOVE      Q1USER        AOIUSR
     C                   MOVE      Q1PGM         AOIPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AOIDT
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C     *inkj         caseq     *on           kjdo                     90
     c                   endcs
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    c*================================================================
     c     pgmstr        begsr
     c*** beg   add *** 06/23/10 **************************************
     c                   eval      *in38 = *zeros
     c                   eval      scattf = *zeros
     c                   eval      scattm = *zeros
     c                   eval      scordt = *zeros
     c                   eval      scstdt = *zeros
     c                   eval      scendt = *zeros
     c                   eval      scfamt = *zeros
     c                   eval      scprpy = *zeros
     c                   eval      scatnm = *blanks
     c****               eval      xxatnm = *blanks
     C*** end   add *** 06/23/10 **************************************
     C                   move      qqfnd         scfnd
     C                   move      qqfyr         scfyr             3 0
     C                   move      qqcase        sccase            7 0
     C                   move      qqco#         kkco#
     C                   move      qqfnd         kkfnd
     C                   move      qqfyr         kkfyr
     C                   move      qqcase        kkcase
      *
     c     cmky          chain     pcm01                              60
     c                   move      cmss#         xxss#
     c                   z-add     cmacdt        srdti
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         scacdt
      *
     c     cdky          chain     pcd01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       QQFMN            15
     C                   MOVE      *BLANKS       QQFSN            15
     C                   MOVE      *BLANKS       QQMID            56
     C                   MOVE      '1'           QQTERM            1
     C                   MOVEL     CDFSN         QQFSN
     C                   MOVEL     CDMID         QQMID
     C                   CALL      'SRFRLS'
     C                   PARM                    QQMID
     C                   PARM                    QQFSN
     C                   PARM                    QQTERM
     C                   MOVEL     QQMID         QQFMN
     C*
     C                   MOVE      '1'           QQTERM
     C                   MOVE      *BLANKS       QQLSN            56
     C                   MOVEL     CDLSN         QQLSN
     C                   CALL      'SRNAME'
     C                   PARM                    QQLSN
     C                   PARM                    QQFMN
     C                   PARM                    QQTERM
     C                   MOVEL     QQLSN         SCCLNM
      *****  Begin Chg  ***  06/09/20  ***********************
     C*****              MOVE      CDSS#         SCSS#
      /Free
       SCSS# = DisplaySS#(MaskData('SSN':'CDSS#':CDSS#));
      /End-Free
      *****  End Chg  ***  06/09/20  ***********************
     C                   ELSE
     C                   MOVE      *BLANKS       SCCLNM
     C                   MOVE      *ZEROS        SCSS#
     C                   END
     C     FNKY          CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
      *
      *  CLAIMANT SS#
     C     CMSS#         IFNE      *BLANK
      *****  Begin Chg  ***  06/09/20  ***********************
     C*****SS1           CAT(P)    '-':0         SCSS#
     C*****SCSS#         CAT       SS2:0         SCSS#
     C*****SCSS#         CAT       '-':0         SCSS#
     C*****SCSS#         CAT       SS3:0         SCSS#
      /Free
       SCSS# = DisplaySS#(MaskData('SSN':'CMSS#':CMSS#));
      /End-Free
      *****  End Chg  ***  06/09/20  ***********************
     C                   END
      *
      *  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
      *
      *  AGENGY CLAIM# (OR, SATE CLAIM#)
      *
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL     CMACL#        SCSCL#
      *
     C     EMKY          CHAIN     PEM01                              60
      *
      * CONCATENATE EMP #, DIV, & NAME
      *
     C                   CALL      'WA005R'
     C                   PARM                    CMEMP#
     C                   PARM                    CMDIV
     C                   PARM                    EMNAME
     C     SCEDN         PARM                    XXEDN            80
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   update    dbrcdf
E01  C                   endif
     C                   MOVE      *ON           OUTFLG
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   write     dbrcdf
     C                   feod      wmaop
     C                   MOVE      *ON           OUTFLG
     C                   endsr
     C************************************************************
     c     kjdo          begsr
     C* ******************************
     C*   F10 = SEARCH
     C* ******************************
     C*
     C*
     C*  ATTORNEY FEIN/MOD SEARCH
     C*
     C     HFLD          IFEQ      'SCATTF'
     C     HFLD          OREQ      'SCATTM'
      *
     C*** BEG   ADD *** 06/23/10 **************************************
     C                   eval      *IN34 = *ZEROS
     C                   eval      *IN38 = *ZEROS
     c                   clear                   QQRTCD
     C*** END   ADD *** 06/23/10 **************************************
     C                   CALL      'W0030R'
     C                   PARM      '2'           QQCALL            1
     C                   PARM                    QAFEIN            9
     C                   PARM                    QAMOD             3
     C                   PARM                    QQRTCD            1
     C*** BEG   ADD *** 06/23/10 **************************************
     C     QQRTCD        IFEQ      'A'
     C                   MOVE      *ON           *INKC
     C                   ENDIF
     C     QQRTCD        IFEQ      'B'
     C                   MOVE      *ON           *IN34                          POS CUSR
     C                   END
     C*** END   ADD *** 06/23/10 **************************************
     C*
     C     QQRTCD        IFEQ      '*'
     C                   MOVE      QAFEIN        SCATTF
     C                   MOVE      QAMOD         SCATTM
     C                   MOVE      SCATTF        XXFEIN
     C                   MOVE      SCATTM        XXMOD
     C                   MOVEL     *BLANKS       XXATNM           30
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   EXSR      GETNM
     C                   MOVEL     XXATNM        SCATNM
     C*** BEG   ADD *** 06/23/10 **************************************
     C                   MOVE      *ON           *IN38
     C*** END   ADD *** 06/23/10 **************************************
     C                   END
     C                   END                                                    QQRTCD = '*'
      *
     C                   END                                                    HFLD = SCATFF
     c                   endsr
     C************************************************************
     C*  SUBROUTINE TO GET PHYSICIAN/HOSPITAL NAME AND ADDRESS   *
     C************************************************************
     C     GETNM         BEGSR
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PDNAME        XXATNM
     C     PDFSN         IFNE      *BLANK
     C                   CAT       ',':0         XXATNM
     C                   CAT       PDFSN:1       XXATNM
     C                   END
     C*
     C                   ENDIF
     C                   ENDSR
