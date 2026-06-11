     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE C0071R02 - Claims Maintenance
     H*****************************************************************
     H*  DESCRIPTION:  This Program will allow maintenance on claims  *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    C0071R                                                     *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  08/19/09 C:107  Support: PC063/62963 & PC099/68539           *
     H*                  Program was sporadically outputing the wrong *
     H*                  CO# to the claims history file. Changed to   *
     H*                  output the same as claims master update.     *
     H*                                                               *
     H*  01/08/07 C:15   Support: PC063/55795                         *
     H*                  Added code so the claim status history record*
     H*                  will be written in add mode.                 *
     H*                                                               *
     H*  03/28/06 C:132  PROJECT: PC094 SEQ#: 47288                   *
     H*                  If Case Status changes on screen, then force *
     H*                  the Effective Date of Change to be the       *
     H*                  System Date in both Output files.            *
     H*                                                               *
     H*****************************************************************
     H*
     FC0071F02  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FCMCMP     UF A E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     FCMCML     IF   E           K DISK
     FCMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FCMPML     IF   E           K DISK
     FCMUPL     IF   E           K DISK
     FCMUPL2    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FCTABL     IF   E           K DISK
     FCARPL     IF   E           K DISK
     FCDCBL     UF   E           K DISK
     FCDCSL     O  A E           K DISK
     FCDRHP     O  A E           K DISK
      /COPY SCOPYMT01D
     D ST              S              2    DIM(52) CTDATA PERRCD(26)
     D MSG             S             60    DIM(3) CTDATA PERRCD(1)
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Work with Claims')
     D COMAPI          C                   CONST('SA005R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##CONM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  POLICY MASTER FILE
     C*
     C     PMKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKPOL#
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKPOL#
     C                   KFLD                    KKCLM#
     C*
     C     CMKEY1        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCLM#
     C*
     C*  REQUEST PAYMENT FILE - TO CHECK FOR PENDING PAYMENTS
     C*
     C     GNKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCLM#
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKEY2        KLIST
     C                   KFLD                    KKSTAF            3
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CMSTAT        BFSTAT
     C     *LIKE         DEFINE    CMPOL#        KKPOL#
     C     *LIKE         DEFINE    CMCLM#        KKCLM#
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMPOL#        QQPOL#
     C     *LIKE         DEFINE    CMCLM#        QQCLM#
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   DO
     C*** DATE OF LOSS
     C                   Z-ADD     SCLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCLSDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7090
     C                   LEAVE
     C                   END
     C*
     C                   Z-ADD     SCLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXLSDT            7 0
     C*
     C*  DATE OF LOSS MUST BE DURING AN EFFECTIVE PERIOD
     C*
     C*  EDIT ACCIDENT TIME
     C*
     C                   MOVE      SCLSTM        SRTIME
     C                   CALL      'SRTME'
     C                   PARM                    SRTIME            6
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7290
     C                   LEAVE
     C                   END
     C*
     C*  EDIT DATE REPORTED
     C*
     C                   Z-ADD     SCRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRPDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        7390
     C                   LEAVE
     C                   END
     C*
     C                   Z-ADD     SCRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRPDT            7 0
     C     XXRPDT        IFLT      XXLSDT
     C                   SETON                                        7490
     C                   LEAVE
     C                   END
     C*
     C*  SUPERVISOR STAFF CODE MUST BE ON FILE
     C*
     C                   MOVEL     SCSUPR        KKSTAF
     C     UPKEY2        CHAIN     PUP02                              75
     C     *IN75         IFEQ      '1'
     C                   SETON                                        90
     C                   LEAVE
     C                   END
     C*
     C*  CLAIMS EXAMINER STAFF CODE MUST BE ON FILE
     C*
     C     SCADJR        IFNE      *BLANKS
     C                   MOVEL     SCADJR        KKSTAF
     C     UPKEY2        CHAIN     PUP02                              76
     C     *IN76         IFEQ      '1'
     C                   SETON                                        90
     C                   LEAVE
     C                   END
     C                   END
     C*
     C*  FIELD ADJUSTER STAFF CODE MUST BE ON FILE
     C*
     C     SCFEAD        IFNE      *BLANK
     C                   MOVEL     SCFEAD        KKSTAF
     C     UPKEY2        CHAIN     PUP02                              77
     C     *IN77         IFEQ      '1'
     C                   SETON                                        90
     C                   LEAVE
     C                   END
     C                   END
     C*
     C*  COUNTY CODE MUST BE ON FILE
     C*
     C                   MOVEL     SCCNTY        XXKEY
     C     TBKEY         CHAIN     PCY01                              81
     C     *IN81         IFEQ      '1'
     C                   SETON                                        90
     C                   LEAVE
     C                   END
     C     CYSTAT        IFEQ      'I'
     C                   SETON                                        8090
     C                   LEAVE
     C                   END
     C*
     C*  THE STATE OF LOSS MUST BE VALID
     C*
     C     SCST          IFNE      *BLANK
     C     SCST          LOOKUP    ST                                     60
     C     *IN60         IFEQ      '0'
     C                   SETON                                        8290
     C                   LEAVE
     C                   END
     C                   END
     C*
     C*  STATUS MUST BE "O" OR "C" OR "R"
     C*
     C     SCSTAT        IFNE      'O'
     C     SCSTAT        ANDNE     'C'
     C     SCSTAT        ANDNE     'R'
     C                   SETON                                        8390
     C                   LEAVE
     C                   END
     C*
     C* STATUS ON NEW CLAIM MUST BE OPEN (O)
     C*
     C     BFSTAT        IFEQ      *BLANK
     C     SCSTAT        IFEQ      'C'
     C     SCSTAT        OREQ      'R'
     C                   SETON                                        8490
     C                   LEAVE
     C                   END
     C                   END
     C*
     C* CLAIM CANNOT CHANGE FROM OPEN (O) TO REOPENED (R)
     C*
     C     BFSTAT        IFEQ      'O'
     C     SCSTAT        ANDEQ     'R'
     C                   SETON                                        8590
     C                   LEAVE
     C                   END
     C*
     C* CLAIM CANNOT CHANGE FROM CLOSED (C) TO OPENED (O)
     C*
     C     BFSTAT        IFEQ      'C'
     C     SCSTAT        ANDEQ     'O'
     C                   SETON                                        8690
     C                   LEAVE
     C                   END
     C*
     C* CLAIM CANNOT CHANGE FROM REOPENED (R) TO OPEN (O)
     C*
     C     BFSTAT        IFEQ      'R'
     C     SCSTAT        ANDEQ     'O'
     C                   SETON                                        5090
     C                   LEAVE
     C                   END
     C*
     C*** CANNOT CLOSE CLAIM WITH PAYMENTS PENDING
     C*
     C     SCSTAT        IFEQ      'C'
     C                   MOVE      SCCLM#        KKCLM#
     C     GNKEY         CHAIN     PRP01                              65
     C     *IN65         IFEQ      '0'
     C                   SETON                                        5190
     C                   LEAVE
     C                   END
     C                   END
     C*
     C*  EDIT EFFECTIVE DATE
     C*
     C     SCEFDT        IFNE      *ZERO
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        8790
     C                   LEAVE
     C                   END
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT            7 0
     C     XXEFDT        IFLT      XXLSDT
     C                   SETON                                        8890
     C                   LEAVE
     C                   ENDIF
     C                   END
     C*
     C*  EXP. AND DATE REQUIRED IF STATUS CHANGES
     C*
     C     SCSTAT        IFNE      BFSTAT
     C     SCEFDT        IFEQ      *ZERO
     C                   SETON                                        8990
     C                   LEAVE
     C                   END
      ***** BEG ADD ***** 03/28/06 ***************************************
      * If Effective date not equal system date is error, post error
     C     SCEFDT        IFNE      DSDATE
     C                   SETON                                        3190
     C                   LEAVE
     C                   END
      ***** END ADD ***** 03/28/06 ***************************************
     C     SCEXPL        IFEQ      *BLANKS
     C                   SETON                                        9190
     C                   LEAVE
     C                   END
     C                   END
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCSUPR'
     C                   CALL      'C0507R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      *BLANKS       QQSTAF            3
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         IFEQ      '*'
     C     QQSTAF        ANDNE     *BLANKS
     C                   MOVE      QQSTAF        SCSUPR
     C                   MOVE      '1'           *IN21
     C                   ELSE
     C                   MOVE      '1'           *IN20
     C                   END
     C*
     C     HFLD          WHENEQ    'SCADJR'
     C                   CALL      'C0507R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      *BLANKS       QQSTAF
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      '*'
     C     QQSTAF        ANDNE     *BLANKS
     C                   MOVE      QQSTAF        SCADJR
     C                   MOVE      '1'           *IN22
     C                   ELSE
     C                   MOVE      '1'           *IN21
     C                   END
     C*
     C     HFLD          WHENEQ    'SCFEAD'
     C                   CALL      'C0507R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      *BLANKS       QQSTAF
     C                   PARM                    QQRTC
     C*
     C     QQRTC         IFEQ      '*'
     C     QQSTAF        ANDNE     *BLANKS
     C                   MOVE      QQSTAF        SCFEAD
     C                   MOVE      '1'           *IN23
     C                   ELSE
     C                   MOVE      '1'           *IN22
     C                   END
     C*
     C     HFLD          WHENEQ    'SCCNTY'
     C                   CALL      'C2221R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      SCCNTY        QQKEY            10
     C                   PARM      *BLANK        QQRTC
     C*
     C     QQRTC         IFEQ      '*'
     C     QQKEY         ANDNE     *BLANKS
     C                   MOVEL     QQKEY         SCCNTY
     C                   MOVE      '1'           *IN18
     C                   ELSE
     C                   MOVE      '1'           *IN17
     C                   END
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     SCCLM1        IFNE      *BLANKS
     C                   MOVEL     SCCLM1        SCCLM#
     C                   ELSE
     C                   MOVEL     CMCLM#        SCCLM#
     C                   ENDIF
     C                   MOVE      *BLANKS       SCEFDT
     C*
     C                   Z-ADD     CMLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCLSDT
     C                   MOVE      CMLSTM        SCLSTM
     C                   Z-ADD     CMRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRPDT
     C                   MOVE      CMSUPR        SCSUPR
     C                   MOVE      CMADJR        SCADJR
     C                   MOVE      CMFEAD        SCFEAD
     C                   MOVEL     CMDSLS        SCDSLS
     C                   MOVE      CMCONT        SCINC1
     C                   MOVE      CMAREA        SCAREA
     C                   MOVE      CMPHN#        SCPHN#
     C                   MOVE      CMLOC         SCLOC
     C                   MOVE      CMCTY         SCCTY
     C                   MOVE      CMST          SCST
     C                   MOVE      CMCNTY        SCCNTY
     C                   MOVEL     CMZIP         SCCZP1
     C                   MOVE      CMZIP         SCCZP2
     C                   MOVE      CMCAT#        SCCAT#
     C                   MOVE      CMSTAT        SCSTAT
     C*
     C     @@MODE        IFEQ      'A'
     C                   TIME                    DSDTTM
     C                   MOVEL     DSDATE        SCEFDT
     C                   MOVEL(P)  MSG(2)        SCEXPL
     C                   MOVE      'O'           SCSTAT
     C                   MOVEL(P)  SCPOL1        KKPOL#
     C*** Begin add *** 01/08/07 *************************************
     C                   MOVE      *BLANK        BFSTAT
     C*** End add ***** 01/08/07 *************************************
     C*
     C                   ELSE
     C*                    MOVE *BLANKS   SCEFDT
     C                   MOVE      CMSTAT        SCSTAT
     C                   MOVE      *BLANKS       SCEXPL
     C                   MOVEL(P)  CMPOL#        KKPOL#
     C*** Begin add *** 01/08/07 *************************************
     C                   MOVE      SCSTAT        BFSTAT
     C*** End add ***** 01/08/07 *************************************
     C                   ENDIF
     C*
     C*** Begin add *** 01/08/07 *************************************
     C*******************  MOVE SCSTAT    BFSTAT
     C*** End add ***** 01/08/07 *************************************
     C*
     C                   MOVE      ##CO#         KKCO#
     C*          SCPOL1    IFEQ *BLANKS
     C*                    ELSE
     C*                    ENDIF
     C*
     C     PMKEY         CHAIN     PPM01                              60
     C*
     C*   SET INDICATOR TO DISPLAY POLICY CANCELLATION DATE
     C*   IF CANCELATION FLAG(PMCANC) IS EQUAL TO "C" OR "P"
     C*
     C     PMCANC        IFEQ      'C'
     C                   MOVE      '1'           *IN03
     C                   ELSE
     C                   MOVE      '0'           *IN03
     C                   END
     C*
     C     PMCANC        IFEQ      'P'
     C                   MOVE      '1'           *IN05
     C                   ELSE
     C                   MOVE      '0'           *IN05
     C                   END
     C***        PMCANC    IFEQ 'Q'
     C***                  MOVE '1'       *IN06
     C***                  ELSE
     C***                  MOVE '0'       *IN06
     C***                  END
     C*
     C                   MOVEL     PMPOL#        SCPOL#
     C                   MOVEL(P)  PMFIN         SCFIN
     C                   MOVE      PMAGCO        SCAGT#
     C                   Z-ADD     PMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPMDT
     C                   Z-ADD     PMEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C     PMCNDT        IFNE      0
     C                   Z-ADD     PMCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCNDT
     C                   END
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVE      ##CO#         CMCO#
     C                   MOVE      SCPOL#        CMPOL#
     C                   MOVE      SCCLM#        CMCLM#
     C                   MOVE      SCLSTM        CMLSTM
     C                   Z-ADD     SCLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMLSDT
     C                   Z-ADD     SCRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMRPDT
     C                   MOVE      SCSUPR        CMSUPR
     C                   MOVE      SCADJR        CMADJR
     C                   MOVE      SCFEAD        CMFEAD
     C                   MOVE      SCDSLS        CMDSLS
     C                   MOVE      SCINC1        CMCONT
     C                   MOVE      SCAREA        CMAREA
     C                   MOVE      SCPHN#        CMPHN#
     C                   MOVE      SCLOC         CMLOC
     C                   MOVE      SCCTY         CMCTY
     C                   MOVE      SCST          CMST
     C                   MOVE      SCCNTY        CMCNTY
     C                   MOVEL     SCCZP1        CMZIP
     C                   MOVE      SCCZP2        CMZIP
     C*****                MOVE SCCAUS    CMCAUS
     C                   MOVE      SCCAT#        CMCAT#
     C*****                Z-ADDSCDIDT    SRDTI
     C*****                CALL 'SRCVTDAT'DTCYMD
     C*****                MOVELSRDTO     CMDIDT
     C                   MOVE      SCSTAT        CMSTAT
     C                   MOVEL(P)  SCFIN         CMFIN
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C*  MOVE FIELDS FOR STATUS CHANGE HISTORY
     C*
     C     SCSTAT        IFNE      BFSTAT
      ***** BEG ADD ***** 03/28/06 ***************************************
      * If status of claim changes, force the effective date to be the
      * system Date, so both output files have same date as status change.
     C                   MOVE      DSDATE        SCEFDT
      *
      ***** END ADD ***** 03/28/06 ***************************************
      ***** BEG DEL ***** 08/19/09 ***************************************
     C********             MOVE UPCO#     CSCO#
      ***** END DEL ***** 08/19/09 ***************************************
      ***** BEG ADD ***** 08/19/09 ***************************************
     C                   MOVE      ##CO#         CSCO#
      ***** END ADD ***** 08/19/09 ***************************************
     C                   MOVE      SCCLM#        CSCLM#
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSEFDT
     C                   MOVEL     SCEXPL        CSEXPL
     C                   MOVE      SCSTAT        CSOCST
     C                   MOVE      Q1USER        CSUSER
     C                   MOVE      Q1PGM         CSPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSDT
     C                   Z-ADD     DSTIME        CSTIME
     C                   WRITE     PCS01
     C                   END
     C*
     C*  IF STATUS WAS CHANGED AND THIS IS NOT A NEW CLAIM THEN WRITE
     C*  THE CHANGES TO THE RESERVE HIST FILE THEN ZERO OUT RESERVES IN
     C*  CLAIM RESERVE FILE IF THE CLAIM IS BEING CLOSED.
     C*
     C     SCSTAT        IFNE      BFSTAT
     C     BFSTAT        ANDNE     *BLANKS
     C     SCSTAT        ANDNE     'R'
     C*
     C                   MOVE      SCCLM#        KKCLM#
     C     GNKEY         CHAIN     PCB01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      UPCO#         RHCO#
     C                   MOVE      SCCLM#        RHCLM#
     C                   MOVE      CBCLID        RHCLID
     C                   MOVE      CBCC          RHCC
     C                   MOVE      CBLOC#        RHLOC#
     C                   MOVE      CBPRM#        RHPRM#
     C                   MOVE      CBCOVF        RHCOVF
     C                   MOVE      CBSUBL        RHSUBL
     C                   MOVE      CBCLAS        RHCLAS
     C                   MOVE      CBCOVL        RHCOVL
     C                   MOVE      CBITM#        RHITM#
     C                   MOVE      CBFRM         RHFRM
     C                   MOVE      CBRTGP        RHRTGP
     C                   MOVE      CBCVBO        RHCVBO
     C                   MOVE      CBVAL         RHVAL
     C                   MOVE      CBTOLC        RHTOLC
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RHTRDT
     C     SCSTAT        IFEQ      'C'
     C                   MOVE      MSG(3)        RHEXPL
     C                   END
     C                   MOVE      CBRT01        RHRT01
     C                   MOVE      CBRT02        RHRT02
     C                   MOVE      CBRT03        RHRT03
     C                   MOVE      CBRT04        RHRT04
     C                   MOVE      CBRT05        RHRT05
     C                   Z-SUB     CBRR01        RHRR01
     C                   Z-SUB     CBRR02        RHRR02
     C                   Z-SUB     CBRR03        RHRR03
     C                   Z-SUB     CBRR04        RHRR04
     C                   Z-SUB     CBRR05        RHRR05
     C                   MOVE      *BLANKS       RHSTAT
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RHDT
     C                   Z-ADD     DSTIME        RHTIME
     C*
     C                   WRITE     PRH01
     C*
     C     SCSTAT        IFEQ      'C'
     C                   Z-ADD     0             CBRR01
     C                   Z-ADD     0             CBRR02
     C                   Z-ADD     0             CBRR03
     C                   Z-ADD     0             CBRR04
     C                   Z-ADD     0             CBRR05
     C                   MOVE      *BLANKS       CBSTAT
     C                   MOVE      Q1USER        CBUSER
     C                   MOVE      Q1PGM         CBPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CBDT
     C                   Z-ADD     DSTIME        CBTIME
     C*
     C                   UPDATE    PCB01
     C                   END
     C*
     C     GNKEY         READE     PCB01                                  60
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C     *INKJ         CASEQ     *ON           KJDO                     90
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   MOVE      *ON           *INLR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     UPKEY         CHAIN     PUP01                              99
     C                   Z-ADD     UPCO#         ##CO#
     C                   MOVE      ##CO#         KKCO#
     C*
     C                   MOVE      *HIVAL        KKCLM#
     C*
     C     CMKEY1        SETGT     PCM02
     C     KKCO#         READPE    PCM02                                  99
     C                   MOVE      CMCLM#        SCLCLM
     C*
     C     @@MODE        IFEQ      'A'
     C*
     C                   MOVE      *BLANKS       SCCLM1
     C                   MOVE      *BLANKS       SCPOL1
     C*
     C     *IN90         DOUEQ     *OFF
     C*
     C                   EXFMT     WDW01
     C* F3/F12
     C     *INKC         IFEQ      *ON
     C     *INKL         OREQ      *ON
     C                   RETURN
     C                   ENDIF
     C* F10
     C     *INKJ         IFEQ      *ON
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCCLM1'
     C*
     C*   CLAIMANT SEARCH
     C*
     C                   MOVE      SCPOL1        QQPOL#
     C                   MOVE      SCCLM1        QQCLM#
     C                   CALL      'C0701R'
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQPOL#
     C                   PARM                    QQCLM#
     C                   PARM                    QQRTC             1
     C                   MOVE      QQPOL#        SCPOL1
     C                   MOVE      QQCLM#        SCCLM1
     C*
     C     QQRTC         IFEQ      'B'
     C     QQCLM#        OREQ      *BLANK
     C     QQPOL#        OREQ      *BLANK
     C                   MOVE      '1'           *IN90
     C                   ITER
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCPOL1'
     C*
     C                   MOVE      *BLANK        QQPOL#
     C                   MOVE      'C'           QQCALL
     C                   CALL      'C0531R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQPOL#
     C                   PARM                    QQRTC
     C*
     C                   MOVE      QQPOL#        SCPOL1
     C*
     C     QQRTC         IFEQ      'B'
     C     QQPOL#        OREQ      *BLANK
     C                   MOVE      '1'           *IN90
     C                   ITER
     C                   ENDIF
     C*
     C* END WDW SEARCH
     C                   ENDSL
     C*
     C                   MOVE      '1'           *IN90
     C                   ITER
     C                   ENDIF
     C*
     C*  CLAIM NUMBER CANNOT BE BLANK
     C*
     C     SCCLM1        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVE      '1'           *IN20
     C                   ITER
     C                   ENDIF
     C     SCPOL1        IFEQ      *BLANKS
     C                   MOVE      '1'           *IN90
     C                   MOVE      '1'           *IN21
     C                   ITER
     C                   ENDIF
     C*
     C                   MOVE      SCPOL1        KKPOL#
     C     PMKEY         CHAIN     PPM01                              90
     C     *IN90         IFEQ      *ON
     C                   MOVE      *ON           *IN22
     C                   ITER
     C                   ENDIF
     C*
     C                   MOVE      SCCLM1        KKCLM#
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN23
     C                   ITER
     C                   ENDIF
     C*
     C                   ENDDO
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   MOVE      *ON           OUTFLG
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      CMCMP
     C                   MOVE      *ON           OUTFLG
     C                   ENDSR
**  TABLE ST
ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
**  TABLE MSG
ORIGINAL RESERVES
ORIGINAL OPEN OF CLAIM
CLAIM CLOSED
