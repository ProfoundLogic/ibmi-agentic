     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H*****************************************************************
     H*  N6150R - Automated Case Services Creation
     H*
     H*****************************************************************
     H*                                                               *
     H*  Called from: N6150C - Automated CS Creation - Override       *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 06/08/2007 by c:62 for Project 139MHK             *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  05/26/09 C:62 PROJECT 68524                                  *
     H*                REDEFINE THE CLAIM STATUS.                     *
     H*                                                               *
     H*  01/24/08 C:62 PROJECT 139MIT                                 *
     H*                MAKE ADDITIONAL CHANGES                        *
     H*                                                               *
     H*****************************************************************
     FFMCML1    IF   E             DISK    INFDS(DBFDS1)
     FFMCHL2    IF   E           K DISK
     FFAMSL5    UF A E           K DISK    INFDS(DBFDS2)
     FFAMSL3    IF   E           K DISK
     F                                     RENAME(PMS01:PMS02)
     FNDTML1    IF   E           K DISK
     F*** BEGIN ADD *** 01/24/08 **************************************
     FWMCXL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)

     D*** END   ADD *** 01/24/08 **************************************
     D*================================================================
     D* Data Structures
     D*================================================================
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*================================================================
     D* Program constants.
     D*================================================================
     D COMAPI          C                   CONST('FA007R')
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     F*** END   ADD *** 01/24/08 **************************************

     I*
     I*** BEGIN ADD *** 01/24/08 **************************************
     IPCM02
     I*** BEGIN ADD *** 05/26/09 **************************************
     I              CMSTAT                      CWSTAT
     I*** END   ADD *** 05/26/09 **************************************
     I              CMOSCL                      CWOSCL

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@FRDT            8
     C                   PARM                    @@TODT            8
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     CHKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    NNENDT
     C*
     C     CHKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*** BEGIN ADD *** 01/24/08 **************************************
     C*
     C     CMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     CXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    KKWCF#
     C*** END   ADD *** 01/24/08 **************************************
     C*
     C     MSKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKMSCD
     C*
     C     MSKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    KKSEQ#
     C*
     C     MSKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     TMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*================================================================
     C* LIKE DEFINES
     C*================================================================
     C     *LIKE         DEFINE    CMDT          XXFRDT
     C     *LIKE         DEFINE    CMDT          XXTODT
     C     *LIKE         DEFINE    MSMSCD        KKMSCD
     C     *LIKE         DEFINE    MSSEQ#        KKSEQ#
     C     *LIKE         DEFINE    MSSEQ#        XXSEQ#
     C     *LIKE         DEFINE    MSMSCD        XXMSCD
     C*** BEGIN ADD *** 01/24/08 **************************************
     C     *LIKE         DEFINE    CMFND         KKWCF#
     C*** END   ADD *** 01/24/08 **************************************
     C*================================================================
     C*
     C* EXECUTE MAIN SUBROUTINE
     C*
     C                   EXSR      MAINSR
     C*
     C*================================================================
     C* MAINSR - MAIN SUBROUTINE
     C*================================================================
     C     MAINSR        BEGSR
     C*
     C                   CALL      COMAPI        COMPL                  99
     C*
     C                   MOVE      @@FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   MOVE      @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   EXSR      GETNMB
     C                   MOVE      NMBGDT        XXENDT
     C                   EXSR      GETMTE
     C                   Z-ADD     NNENDT        NMENDT            7 0
     C*
     C* Reading thru the case master file
     C*
     C                   READ      PCM01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C* Has the claim been open during the time period
     C*
     C*** BEGIN ADD *** 01/24/08 **************************************
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1WCF#        KKWCF#
     C                   ELSE
     C                   Z-ADD     *ZEROS        KKWCF#
     C                   ENDIF
     C*
     C     CMKY          CHAIN     PCM02                              99
     C     *IN99         IFEQ      *ON
     C                   CLEAR                   CMLSCV
     C                   ENDIF
     C*
     C                   EXSR      CHKSTS
     C*** END   ADD *** 01/24/08 **************************************
     C**********           EXSR CHKOPN
     C*** BEGIN CHG *** 01/24/08 **************************************
     C********** CMSTAT    IFEQ 'O'
     C********** CMSTAT    OREQ 'R'
     C     PROCES        IFEQ      *ON
     C*** END   CHG *** 01/24/08 **************************************
     C*
     C* Is this a Catastrophic claim?
     C*
     C     CMCFLG        IFEQ      'Y'
     C*
     C* Catastrophic claim
     C*
     C                   EXSR      WWCCM
     C                   ELSE
     C*
     C* Telephonic Case Management
     C*
     C                   EXSR      WWTCM
     C                   ENDIF
     C*
     C* On-site Case Management
     C*
     C                   EXSR      WWOCM
     C                   ENDIF
     C*
     C                   READ      PCM01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* BILSVC - Bill services
     C*================================================================
     C     BILSVC        BEGSR
     C*
     C                   CALL      'FA036R'
     C                   PARM      'W'           ##CALL            1
     C                   PARM      DBRRN2        ##RRN             9 0          FAMSP
     C                   PARM      DBRRN1        ##RRN2            9 0          FMCMP
     C                   PARM      MSMSCD        ##MSCD            4
     C                   PARM      MSBGDT        ##BGDT            7 0
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    ##MSCG            9 2
     C*
     C                   ENDSR
     C*================================================================
     C* CHKAGE - Check to see if this claim is more than 60 days old.
     C*================================================================
     C     CHKAGE        BEGSR
     C*
     C                   MOVE      *OFF          AGED              1
     C*
     C                   Z-ADD     CMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DAT1
     C*
     C                   Z-ADD     NMENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DAT2
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    ##DAT1            8 0
     C                   PARM                    ##DAT2            8 0
     C                   PARM      *ZEROS        ##DIFF            5 0
     C                   PARM      *BLANKS       ##ERR             1
     C*
     C     ##DIFF        IFGT      60
     C                   MOVE      *ON           AGED
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CHKCMA - Check to see if this claim has been assigned a
     C*          case manager for this time period.
     C*================================================================
     C     CHKCMA        BEGSR
     C*
     C                   MOVE      *OFF          ASSGND            1
     C     CMCMAD        IFNE      *ZEROS
     C     CMCMAD        IFLE      NMENDT
     C     CMCMCD        ANDEQ     *ZEROS
     C     CMCMAD        ORLE      NMENDT
     C     CMCMCD        ANDGE     NMBGDT
     C                   MOVE      *ON           ASSGND
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 01/24/08 **************************************
     C*================================================================
     C* CHKCMC - Check to see if the case management close date is
     C*          less than the period begin date.
     C*================================================================
     C     CHKCMC        BEGSR
     C*
     C                   MOVE      *OFF          CLOSED            1
     C     CMCMCD        IFLT      NMBGDT
     C     CMCMCD        ANDNE     *ZEROS
     C                   MOVE      *ON           CLOSED
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 01/24/08 **************************************
     C*================================================================
     C* CHKNEW - Check to see if this has ever been billed before.
     C*================================================================
     C     CHKNEW        BEGSR
     C*
     C                   MOVE      *ON           NEW               1
     C     TMKY          CHAIN     PTM01                              99
     C     *IN99         DOWEQ     *OFF
     C     TMMSCG        IFEQ      1250
     C                   MOVE      *OFF          NEW
     C                   LEAVE
     C                   ENDIF
     C     TMKY          READE     PTM01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* CHKOPN - Check to see if this claim has been open in the
     C*          time period
     C*================================================================
     C     CHKOPN        BEGSR
     C*
     C                   MOVE      *OFF          OPNSVC            1
     C*
     C     CHKY1         SETGT     PCH01
     C     CHKY2         READPE    PCH01                                  99
     C     *IN99         DOWEQ     *OFF
     C     CHEFDT        IFLE      NNENDT
     C*
     C                   MOVE      *OFF          CLOSED            1
     C     CHEFDT        IFGE      XXFRDT
     C     CHOCST        IFEQ      'O'
     C                   MOVE      *ON           OPNSVC
     C                   LEAVE
     C                   ELSE
     C     CHOCST        IFEQ      'C'
     C     CHEFDT        ANDGT     XXFRDT
     C                   MOVE      *ON           OPNSVC
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C     CHOCST        IFEQ      'O'
     C                   MOVE      *ON           OPNSVC
     C                   LEAVE
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     CHKY2         READPE    PCH01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*** BEGIN ADD *** 01/24/08 **************************************
     C*================================================================
     C* CHKSTS - Check the status and see if open or reoppenned. If
     C*          reopenned the reopen days have to be greater than 10.
     C*================================================================
     C     CHKSTS        BEGSR
     C*
     C                   MOVE      *OFF          PROCES            1
     C     CMSTAT        IFEQ      'R'
     C*
     C     CXKY          CHAIN     PCX01                              99
     C     *IN99         IFEQ      *OFF
     C     CXRODY        IFGT      10
     C                   MOVE      *ON           PROCES
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C     CMSTAT        IFEQ      'O'
     C                   MOVE      *ON           PROCES
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 01/24/08 **************************************
     C*================================================================
     C* CLSCCM - Close open CCM services
     C*================================================================
     C     CLSCCM        BEGSR
     C*
     C                   MOVE      *OFF          EXISTS            1
     C*** BEGIN CHG *** 01/24/08 **************************************
     C**********           MOVEL'CCM'     KKMSCD    P
     C                   MOVEL(P)  'CAT'         KKMSCD
     C*** END   CHG *** 01/24/08 **************************************
     C     MSKY          CHAIN     PMS01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     MSBGDT        IFLE      XXTODT
     C     MSENDT        ANDEQ     *ZEROS
     C*
     C                   MOVEL     '04'          MSSTS
     C                   MOVEL     'C'           MSSTAT
     C                   MOVE      MSBGDT        XXENDT            7
     C                   EXSR      GETMTE
     C                   Z-ADD     NNENDT        MSENDT
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        MSUSER
     C                   MOVEL     Q1PGM         MSPGM
     C                   Z-ADD     DSTIME        MSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MSDT
     C                   ELSE
     C                   MOVE      MSBGDT        XXENDT
     C                   EXSR      GETMTE
     C     MSBGDT        IFGT      XXTODT
     C     MSBGDT        ANDLT     NNENDT
     C                   MOVE      *ON           EXISTS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   UPDATE    PMS01
     C     MSKY          READE     PMS01                                  99
     C                   ENDDO
     C*** BEGIN ADD *** 01/24/08 **************************************
     C                   MOVEL(P)  'CAT3'        KKMSCD
     C     MSKY          CHAIN     PMS01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     MSBGDT        IFLE      XXTODT
     C     MSENDT        ANDEQ     *ZEROS
     C*
     C                   MOVEL     '04'          MSSTS
     C                   MOVEL     'C'           MSSTAT
     C                   MOVE      MSBGDT        XXENDT            7
     C                   EXSR      GETMTE
     C                   Z-ADD     NNENDT        MSENDT
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        MSUSER
     C                   MOVEL     Q1PGM         MSPGM
     C                   Z-ADD     DSTIME        MSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MSDT
     C                   ELSE
     C                   MOVE      MSBGDT        XXENDT
     C                   EXSR      GETMTE
     C     MSBGDT        IFGT      XXTODT
     C     MSBGDT        ANDLT     NNENDT
     C                   MOVE      *ON           EXISTS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   UPDATE    PMS01
     C     MSKY          READE     PMS01                                  99
     C                   ENDDO
     C*** END   ADD *** 01/24/08 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* CLSOCM - Close open OCM services
     C*================================================================
     C     CLSOCM        BEGSR
     C*
     C                   MOVE      *OFF          EXISTS
     C                   MOVEL(P)  'OCM'         KKMSCD
     C     MSKY          CHAIN     PMS01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     MSBGDT        IFLE      XXTODT
     C     MSENDT        ANDEQ     *ZEROS
     C*
     C                   MOVEL     '04'          MSSTS
     C                   MOVEL     'C'           MSSTAT
     C                   MOVE      MSBGDT        XXENDT
     C                   EXSR      GETMTE
     C                   Z-ADD     NNENDT        MSENDT
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        MSUSER
     C                   MOVEL     Q1PGM         MSPGM
     C                   Z-ADD     DSTIME        MSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MSDT
     C                   ELSE
     C                   MOVE      MSBGDT        XXENDT
     C                   EXSR      GETMTE
     C     MSBGDT        IFGT      XXTODT
     C     MSBGDT        ANDLT     NNENDT
     C                   MOVE      *ON           EXISTS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   UPDATE    PMS01
     C     MSKY          READE     PMS01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* CLSTCM - Close open TCM services
     C*================================================================
     C     CLSTCM        BEGSR
     C*
     C                   MOVE      *OFF          EXISTS
     C                   MOVEL(P)  'TCM'         KKMSCD
     C     MSKY          CHAIN     PMS01                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     MSBGDT        IFLE      XXTODT
     C     MSENDT        ANDEQ     *ZEROS
     C*
     C                   MOVEL     '04'          MSSTS
     C                   MOVEL     'C'           MSSTAT
     C                   MOVE      MSBGDT        XXENDT
     C                   EXSR      GETMTE
     C                   Z-ADD     NNENDT        MSENDT
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        MSUSER
     C                   MOVEL     Q1PGM         MSPGM
     C                   Z-ADD     DSTIME        MSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MSDT
     C                   ELSE
     C                   MOVE      MSBGDT        XXENDT
     C                   EXSR      GETMTE
     C     MSBGDT        IFGT      XXTODT
     C     MSBGDT        ANDLT     NNENDT
     C                   MOVE      *ON           EXISTS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   UPDATE    PMS01
     C     MSKY          READE     PMS01                                  99
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* GETMTE - Get the Month-end Date
     C*================================================================
     C     GETMTE        BEGSR
     C*
     C     2             SUBST     XXENDT:4      MONTH             2
     C     3             SUBST     XXENDT:1      YEAR              3
     C*
     C                   SELECT
     C     MONTH         WHENEQ    '01'
     C                   MOVE      '0131'        XXENDT
     C     MONTH         WHENEQ    '02'
     C     YEAR          IFEQ      '104'
     C     YEAR          OREQ      '108'
     C     YEAR          OREQ      '112'
     C     YEAR          OREQ      '116'
     C     YEAR          OREQ      '120'
     C                   MOVE      '0229'        XXENDT
     C                   ELSE
     C                   MOVE      '0228'        XXENDT
     C                   ENDIF
     C     MONTH         WHENEQ    '03'
     C                   MOVE      '0331'        XXENDT
     C     MONTH         WHENEQ    '04'
     C                   MOVE      '0430'        XXENDT
     C     MONTH         WHENEQ    '05'
     C                   MOVE      '0531'        XXENDT
     C     MONTH         WHENEQ    '06'
     C                   MOVE      '0630'        XXENDT
     C     MONTH         WHENEQ    '07'
     C                   MOVE      '0731'        XXENDT
     C     MONTH         WHENEQ    '08'
     C                   MOVE      '0831'        XXENDT
     C     MONTH         WHENEQ    '09'
     C                   MOVE      '0930'        XXENDT
     C     MONTH         WHENEQ    '10'
     C                   MOVE      '1031'        XXENDT
     C     MONTH         WHENEQ    '11'
     C                   MOVE      '1130'        XXENDT
     C     MONTH         WHENEQ    '12'
     C                   MOVE      '1231'        XXENDT
     C                   ENDSL
     C*
     C                   MOVE      XXENDT        NNENDT            7 0
     C*
     C                   ENDSR
     C*================================================================
     C* GETNMB - Get Next Months Begin Date
     C*================================================================
     C     GETNMB        BEGSR
     C*
     C                   MOVE      XXFRDT        XXBGDT            7
     C     2             SUBST     XXBGDT:4      MONTH
     C*
     C                   SELECT
     C     MONTH         WHENEQ    '01'
     C                   MOVE      '0201'        XXBGDT
     C     MONTH         WHENEQ    '02'
     C                   MOVE      '0301'        XXBGDT
     C     MONTH         WHENEQ    '03'
     C                   MOVE      '0401'        XXBGDT
     C     MONTH         WHENEQ    '04'
     C                   MOVE      '0501'        XXBGDT
     C     MONTH         WHENEQ    '05'
     C                   MOVE      '0601'        XXBGDT
     C     MONTH         WHENEQ    '06'
     C                   MOVE      '0701'        XXBGDT
     C     MONTH         WHENEQ    '07'
     C                   MOVE      '0801'        XXBGDT
     C     MONTH         WHENEQ    '08'
     C                   MOVE      '0901'        XXBGDT
     C     MONTH         WHENEQ    '09'
     C                   MOVE      '1001'        XXBGDT
     C     MONTH         WHENEQ    '10'
     C                   MOVE      '1101'        XXBGDT
     C     MONTH         WHENEQ    '11'
     C                   MOVE      '1201'        XXBGDT
     C     MONTH         WHENEQ    '12'
     C                   MOVE      '0101'        XXBGDT
     C     3             SUBST     XXBGDT:1      YEAR
     C                   MOVE      YEAR          YEARN             3 0
     C                   ADD       1             YEARN
     C                   MOVEL     YEARN         XXBGDT
     C                   ENDSL
     C*
     C                   MOVE      XXBGDT        NMBGDT            7 0
     C*
     C                   ENDSR
     C*================================================================
     C* OPNCCM - Open CCM services
     C*================================================================
     C     OPNCCM        BEGSR
     C*
     C     EXISTS        IFEQ      *OFF
     C*** BEGIN CHG *** 01/24/08 **************************************
     C**********           MOVEL'CCM'     XXMSCD
     C                   EXSR      CHKAGE
     C     AGED          IFEQ      *ON
     C                   MOVEL(P)  'CAT3'        XXMSCD
     C                   ELSE
     C                   MOVEL(P)  'CAT'         XXMSCD
     C                   ENDIF
     C*** END   ADD *** 01/24/08 **************************************
     C*** BEGIN CHG *** 01/24/08 **************************************
     C**********           EXSR CHKNEW
     C********** NEW       IFEQ *ON
     C     XXMSCD        IFEQ      'CAT'
     C*** END   CHG *** 01/24/08 **************************************
     C                   Z-ADD     1250          ##MSCG
     C                   ELSE
     C                   Z-ADD     1000          ##MSCG
     C                   ENDIF
     C                   EXSR      WRTMSP
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPNOCM - Open OCM services
     C*================================================================
     C     OPNOCM        BEGSR
     C*
     C     EXISTS        IFEQ      *OFF
     C                   MOVEL(P)  'OCM'         XXMSCD
     C                   Z-ADD     500           ##MSCG
     C                   EXSR      WRTMSP
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OPNTCM - Open TCM services
     C*================================================================
     C     OPNTCM        BEGSR
     C*
     C     EXISTS        IFEQ      *OFF
     C                   MOVEL(P)  'TCM'         XXMSCD
     C                   Z-ADD     250           ##MSCG
     C                   EXSR      WRTMSP
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTMSP - Create FAMSP Record
     C*================================================================
     C     WRTMSP        BEGSR
     C*
     C* Get next available Sequence number
     C*
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     MSKY1         SETGT     PMS02
     C     MSKY2         READPE    PMS02                                  99
     C     *IN99         IFEQ      *OFF
     C     MSSEQ#        ADD       1             XXSEQ#
     C                   ELSE
     C                   Z-ADD     1             XXSEQ#
     C                   ENDIF
     C*
     C                   CLEAR                   PMS01
     C*
     C                   Z-ADD     CMCO#         MSCO#
     C                   MOVE      CMSUB#        MSSUB#
     C                   MOVE      CMFND         MSFND
     C                   MOVE      CMFYR         MSFYR
     C                   MOVE      CMCASE        MSCASE
     C                   MOVEL     XXMSCD        MSMSCD
     C                   MOVE      XXSEQ#        MSSEQ#
     C                   MOVEL     'O'           MSSTAT
     C*
     C                   MOVE      NMBGDT        MSBGDT
     C                   MOVEL     CMCMNU        MSSTAF
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        MSUSER
     C                   MOVEL     Q1USER        MSIUSR
     C                   MOVEL     Q1PGM         MSPGM
     C                   MOVEL     Q1PGM         MSIPGM
     C                   MOVEL     DSTIME        MSTIME
     C                   MOVEL     DSTIME        MSITIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MSDT
     C                   MOVEL     SRDTO         MSIDT
     C*
     C                   WRITE     PMS01
     C*
     C* Create bill service or update an existing bill service
     C*
     C                   EXSR      BILSVC
     C*
     C                   ENDSR
     C*================================================================
     C* WWCCM - Work with Catastrophic Claim Services
     C*================================================================
     C     WWCCM         BEGSR
     C*
     C                   EXSR      CLSCCM
     C                   EXSR      OPNCCM
     C*
     C                   ENDSR
     C*================================================================
     C* WWOCM - Work with Occupational Claim Services
     C*================================================================
     C     WWOCM         BEGSR
     C*
     C                   EXSR      CLSOCM
     C                   EXSR      CHKCMA
     C     ASSGND        IFEQ      *ON
     C                   EXSR      OPNOCM
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WWTCM - Work with Telephonic Claim Services
     C*================================================================
     C     WWTCM         BEGSR
     C*
     C                   EXSR      CLSTCM
     C                   EXSR      CHKAGE
     C*** BEGIN ADD *** 01/24/08 **************************************
     C                   EXSR      CHKCMC
     C*** END   ADD *** 01/24/08 **************************************
     C     AGED          IFEQ      *ON
     C     CMDNY         ANDNE     'Y'
     C*** BEGIN DEL *** 01/24/08 **************************************
     C********** CMCVFL    ANDNE'Y'
     C*** END   DEL *** 01/24/08 **************************************
     C*** BEGIN ADD *** 01/24/08 **************************************
     C     CMCLAD        ANDNE     'CCM'
     C     CLOSED        ANDEQ     *OFF
     C     CMLSCV        ANDNE     '31'
     C     CMLSCV        ANDNE     '34'
     C     CMLSCV        ANDNE     '37'
     C*** END   ADD *** 01/24/08 **************************************
     C                   EXSR      OPNTCM
     C                   ENDIF
     C*
     C                   ENDSR
