     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0079R02 - WORK WITH ADDITIONAL NAMED INSURED
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION: THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW  *
     H*               OR ADD RECORDS IN THE ADDITIONAL NAMED INSURED  *
     H*               FILE.                                           *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0079R - WORK WITH ADDITIONAL NAMED INSUREDS               *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H* CREATED: 01/16/01 PROGRAMMER: C:61  PROJECT WC996             *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/03/05 C:132 SUPPORT WA114 46526                            *
     H*                Allow Delete Date to be input on Maintenance,  *
     H*                always show Delete Date field.                 *
     H*                                                               *
     H* 03/15/04  C:132 PROJECT: WA099  SEQ: 41718                    *
     H*                 Change Position of SCMODE on SCR01 from line  *
     H*                 three to line four so does not overlap SCFNAM *
     H*                                                               *
     H* 05/02/03  C:132 PROJECT: WC996                                *
     H*                 Add second Name File for contiunation of name *
     H*                                                               *
     H* 02/22/02  C:90  PROJECT: WC577                                *
     H*                 Correct loading of status field.              *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* INDICATORS                                                    *
     H*                                                               *
     H* 45      Deletion Date entered.                                *
     H* 63      DELETE MODE                                           *
     H*                                                               *
     H* SCR01                                                         *
     H* 20      Invalid deletion date                                 *
     H* 21      Deletion date < effective date                        *
     H* 22      Invalid effective date                                *
     H* 23      Name not entered.                                     *
     H* 71      FEIN not entered.                                     *
     H*                                                               *
     H*****************************************************************
      *
     FW0079F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
      *
      *  Physical file definition (for maintaining record)
     FWMANP     UF A E             DISK
     F                                     RENAME(PAN01:DBRCDF)
     FWMANL     IF   E           K DISK
     F                                     RENAME(PAN01:PAN02)
      *
      * Employer master
     FWMEMP     IF   E             DISK
      *
      * Fund master
     FWMFNL     IF   E           K DISK
      *
      ***** BEG ADD ***** 05/02/03 ************************************
      *
      * Workers Compensation Control File
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      ***** END ADD ***** 05/02/03 ************************************
      *
     D***PY SCOPYMT01I
      *
      *================================================================
      *  /COPY required constants
      *================================================================
      /COPY SCOPYMT01D
     D OPTNM           C                   CONST('Additional Named Ins-
     D                                     ured')
     D COMAPI          C                   CONST('WA001R')
     D #OFSCR          C                   CONST(1)
      *
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RRN2            9 0
     C                   PARM      *INKL         @1KL              1
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
      *
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
     C     ANKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
      *
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    ANEFDT        XXEFDT
     C     *LIKE         DEFINE    ANDLDT        XXDLDT
     C     *LIKE         DEFINE    ANSEQ#        SVSEQ#
      *
     C/COPY SCOPYMT01C
      *
      *
      *
      *================================================================
      * CHGSTS - SUBROUTINE FOR CHANGING STATUS TO INACTIVE
      *================================================================
      *
     C     CHGSTS        BEGSR
      *
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
      *
     C     ANSTAT        IFNE      'I'
     C                   MOVE      'I'           ANSTAT
      *
     C                   ELSE
      *
     C                   MOVE      'A'           ANSTAT
     C                   ENDIF                                                  SVSTAT
      *
     C                   MOVEL     Q1USER        ANUSER
     C                   MOVEL     Q1PGM         ANPGM
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ANDT
      *
     C                   MOVEL     DSTIME        ANTIME
     C                   UPDATE    DBRCDF
     C                   ENDIF                                                  *IN99
      *
     C                   ENDSR                                                  CHGSTS
      *
      *
      *
      *================================================================
      * DLTPRE - Pre-processing before delete occurs
      *================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR                                                  DLTPRE
      *
      *
      *
      *================================================================
      * DSPLYR - Processing after displaying information
      *================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR                                                  DSPLYR
      *
      *
      *
      *================================================================
      * ERRCK1 - Error checking for srceen 1 (SCR01)
      *================================================================
     C     ERRCK1        BEGSR
      *
      ***** BEG DEL ***** 05/03/05 ************************************
     C********   *IN63     IFEQ *OFF
      ***** END DEL ***** 05/03/05 ************************************
      *
      * NAME MUST NOT BE BLANK
      *
     C  N90SCNAME        IFEQ      *BLANKS
     C                   SETON                                        23  90
     C                   ENDIF
      *
      *
      * FEIN must be entered
      *
     C  N90SCFEIN        IFEQ      *ZERO
     C                   SETON                                        71  90
     C                   ENDIF
      *
      *  EFFECTIVE DATE MUST BE VALID
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
      *
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        22  90
     C                   ENDIF                                                  SRERR=*ON
      *
      ***** BEG CHG ***** 05/03/05 ************************************
     C**********           ELSE
     C     SCDLDT        IFNE      *ZEROS
      ***** END CHG ***** 05/03/05 ************************************
      *
      *  DELETION DATE MUST BE VALID
      *
     C                   Z-ADD     SCDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDLDT
      *
     C  N90SRERR         IFEQ      *ON
     C                   SETON                                        20  90
     C                   ENDIF                                                  SRERR=*ON
      *
      *  DELETION DATE CANNOT BE LESS THAN EFFECTIVE DATE.
      *
     C     *IN90         IFEQ      *OFF
      *
     C                   Z-ADD     SCDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDLDT
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
      *
     C     XXDLDT        IFLT      XXEFDT
     C     XXDLDT        ANDNE     *ZERO
     C                   SETON                                        21  90
     C                   ENDIF                                                  XXDLDT<XXEFDT
      *
     C                   ENDIF                                                  *IN90 = *OFF
      *
     C                   ENDIF                                                  *IN63 = *OFF
      *
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for srceen 2 (SCR02)
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * ERRCK3 - Error checking for srceen 3 (SCR03)
      *================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR                                                  ERRCK3
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVEL     ANNAME        SCNAME
      *
      ***** BEG ADD ***** 05/02/03 ************************************
     C                   MOVEL     ANNAM2        SCNAM2
      *
     C   09
     CAN 69SCNAM2        IFEQ      *BLANKS
     C                   SETOFF                                       69
     C                   ENDIF
      *
      ***** END ADD ***** 05/02/03 ************************************
      *
     C                   Z-ADD     ANFEIN        SCFEIN
      *
     C                   Z-ADD     ANEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
      *
     C                   Z-ADD     ANDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDLDT
      *
      ***** BEG DEL ***** 05/03/05 ************************************
      * ONLY SHOW "DELETION DATE"
      * IF NOT ZERO
      **********
     C********** SCDLDT    IFNE *ZEROS
     C**********           MOVE *OFF      *IN45
      **********
     C**********           ELSE
      **********
     C**********           MOVE *ON       *IN45
     C**********           ENDIF
      ***** END DEL ***** 05/03/05 ************************************
      *
      **DON'T*DISPLAY DELETION DATE IF IN ADD MODE.
      *
     C     @@MODE        IFEQ      'A'
     C                   MOVE      *ON           *IN45
     C                   ENDIF
      *
      ***** BEG DEL ***** 05/03/05 ************************************
      **IF*DELETING AN ADDITIONAL INSURED
      **ALWAYS*SHOW "DELETION DATE".
      **********
     C********** *IN63     IFEQ *ON
     C**********           MOVE *OFF      *IN45
     C**********           ENDIF
      ***** END DEL ***** 05/03/05 ************************************
      *
     C                   MOVE      ANSTAT        SCSTAT
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * MVSCDB - Move screen fields to data base fields
      *================================================================
     C     MVSCDB        BEGSR
      *
     C     @@MODE        IFEQ      'A'
     C                   Z-ADD     *ZERO         SVSEQ#
      *
     C     ANKY          SETLL     PAN02
      *
     C     *IN60         DOUEQ     *ON
     C     ANKY          READE     PAN02                                  60
      *
     C     ANSEQ#        IFGT      SVSEQ#
     C                   Z-ADD     ANSEQ#        SVSEQ#
     C                   ENDIF
      *
     C                   ENDDO
      *
     C     SVSEQ#        ADD       1             ANSEQ#
     C                   ENDIF
      *
     C                   Z-ADD     EMCO#         ANCO#
     C                   Z-ADD     EMFND         ANFND
     C                   Z-ADD     EMEMP#        ANEMP#
     C                   Z-ADD     EMDIV         ANDIV
      *
     C                   MOVEL     SCNAME        ANNAME
      *
      ***** BEG ADD ***** 05/02/03 ************************************
     C                   MOVEL     SCNAM2        ANNAM2
      ***** END ADD ***** 05/02/03 ************************************
      *
     C                   Z-ADD     SCFEIN        ANFEIN
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ANEFDT
      *
     C                   MOVE      Q1USER        ANUSER
     C                   MOVE      Q1PGM         ANPGM
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ANDT
      *
     C                   Z-ADD     DSTIME        ANTIME
      *
      **** BEGIN ADD **** 02/22/02 ************************************
     C                   MOVE      SCSTAT        ANSTAT
      **** END   ADD **** 02/22/02 ************************************
      *
     C     *IN40         IFEQ      *ON
     C                   MOVE      'A'           ANSTAT
     C                   ENDIF
      *
      **** BEGIN DEL **** 02/22/02 ************************************
     C******************** MOVE SCSTAT    ANSTAT
      **** END   DEL **** 02/22/02 ************************************
      *
     C                   Z-ADD     SCDLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ANDLDT
      *
     C                   ENDSR                                                  MVSCDB
      *
      *
      *
      *================================================================
      * OTHERR - Processing for other modes
      *================================================================
     C     OTHERR        BEGSR
     C                   ENDSR                                                  OTHERR
      *
      *
      *
      *================================================================
      * OTHFKS - Processing for other functions keys
      *================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C     @@RRN2        CHAIN     PEM01                              60
      *
      * COMPRESS EMP#/DIV/NAME
      *
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
      *
     C                   Z-ADD     EMFND         SCFND
     C                   MOVEL(P)  XXEDN         SCEDN
      *
      * GROUP INFORMATION
      *
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL(P)  FNNAME        SCFNAM
      *
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
      *
      ***** BEG DEL ***** 05/03/05 ************************************
     C**********           MOVE *OFF      *IN63
      **********
     C********** @@MODE    IFEQ 'E'
     C**********           MOVE *ON       *IN63
     C**********           MOVE 'C'       @@MODE
     C**********           ENDIF                           @@MODE = E
      **********
     C**********           MOVE *ZEROS    SCDLDT
      ***** END DEL ***** 05/03/05 ************************************
      *
      ***** BEG ADD ***** 05/02/03 ************************************
      * If system flag on, set for second name field
     C     'WC'          CHAIN     SWC01                              60
     C     *IN60         IFEQ      *OFF
     C     WCFL16        ANDEQ     'Y'
     C                   SETON                                        69
     C                   ENDIF
      ***** END ADD ***** 05/02/03 ************************************
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * PREOTH - Pre-processing for other modes
      *================================================================
     C     PREOTH        BEGSR
      *
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
      *
     C                   ENDSR                                                  PREOTH
      *
      *
      *
      *================================================================
      * UPDATR - Update data base record and after processing
      *================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
      *
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C                   ENDIF
      *
     C                   ENDSR                                                  UPDATR
      *
      *
      *
      *================================================================
      * WRITER - Write data base record and after processing
      *================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
      *
     C                   MOVE      Q1USER        ANIUSR
     C                   MOVE      Q1PGM         ANIPGM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ANIDT
      *
     C                   WRITE     DBRCDF
     C                   FEOD      WMANP
     C                   ENDSR                                                  WRITER
      *
      *
      *
