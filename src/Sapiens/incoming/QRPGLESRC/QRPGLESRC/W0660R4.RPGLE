     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W0660R4 - S/R PAYROLL INFORMATION - ADDITIONAL LOAD
     H*****************************************************************
     H*                                                               *
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO CHANGE, VIEW *
     H*                OR ADD ADDITIONAL LOAD TO S/R PAYROLL FILE.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0660R02 - S/R PAYROLL INFORMATION                         *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 11/14/02  PROGRAMMER: C:65  PROJECT: WC1099         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N40 - ADDITIONAL CLASS CODE ENTRY                             *
     H*  85 - SCREEN WAS UPDATE WHILE USER WAS IN IT                  *
     H*  90 - GENERAL ERROR INDICATOR                                 *
     H*  91 - GENERAL POSITION CURSOR                                 *
     H*  94 - ROLL UP/DOWN PAST LAST RECORD                           *
     H*                                                               *
     H*****************************************************************
     FW0660F4   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:DBRCDF2)
     F                                     INFDS(DBFDS2)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWASRP     UF   E             DISK
     F                                     RENAME(PSR01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWASRL6    IF   E           K DISK
     F                                     RENAME(PSR01:DBRCDF)
     F                                     INFDS(DBFDS)
     FWMFNL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMPXL1    IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWMPRL1    IF   E           K DISK
      *
      /COPY SCOPYWW03D
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(8) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(8) ALT(OF)
     D*
     D*** BEGIN CHG ***  06/01/06  ************************************
     D*********/COPY SCOPYWW02E
     D*
     D*********/COPY SCOPYWW02I
     D***PY SCOPYWW03E
     D*
     D***PY SCOPYWW03I
     D*** END CHG *****  06/01/06  ************************************
     D*
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)                               EFF.DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Territory Differenti-
     D                                     al Entry')
     D ACDPGM          C                   CONST('STDEFAULT')
     D AUDIT           C                   CONST('Audited')
     D EST             C                   CONST('Estimated')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D USPAPI          C                   CONST('W0006R')
     D SF1PAG          C                   CONST(12)

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RPMY            6 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##MODE            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      EMFND         ##FND
     C                   PARM      EMEMP#        ##EMP#            9 0
     C                   PARM      EMDIV         ##DIV             5 0
     C                   PARM                    ##FYR             3 0
     C     SCAGT#        PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C     SCPROD        PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    KKRPMY            5 0
     C*
     C     KEY1A         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    KKRPMY
     C                   KFLD                    KKCLAS
     C                   KFLD                    KKSPEC
     C                   KFLD                    KKEFDT
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  GROUP MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
      *
      *  PAYROLL DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    SRCLAS
     C                   KFLD                    SREXT
     C                   KFLD                    ##ST              2
     C*** BEGIN ADD ***  06/01/06  ************************************
     C                   KFLD                    SREFDT
     C     PXKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    SRCLAS
     C                   KFLD                    SREXT
     C                   KFLD                    ##ST              2
     C*** END ADD *****  06/01/06  ************************************
      *
      * RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C*
     C*  PAYROLL MASTER
     C*
     C     PRKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    SCCLAS
     C                   KFLD                    XXEFDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    SCCLAS
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    SRCLAS        KKCLAS
     C     *LIKE         DEFINE    SRSPEC        KKSPEC
     C     *LIKE         DEFINE    SREFDT        KKEFDT
     C     *LIKE         DEFINE    SREFDT        XXEFDT
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    SCEFDT        X1EFDT
     C*
     C                   Z-ADD     0             I1                3 0
     C*** BEGIN CHG ***  06/01/06  ************************************
     C*********/COPY SCOPYWW02C
     C/COPY SCOPYWW03C
     C*** END CHG *****  06/01/06  ************************************
     C*
     C*================================================================
     C* ERRCKA - Error check all subfile records (entire screen)
     C*================================================================
     C*
     C     ERRCKA        BEGSR
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR                                                  ERRCKA
     C*
     C*================================================================
     C* ERRCKC - Error check all control record fields
     C*================================================================
     C*
     C     ERRCKC        BEGSR
     C                   ENDSR                                                  ERRCKC
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C*  ADDITIONAL LOAD CANNOT BE LESS THAN ZERO
     C*
     C  N90SCADLD        IFLT      *ZERO
     C                   SETON                                        74  90
     C                   ENDIF                                                  SCEAPR<*ZERO
     C*
     C*  CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90SCDBRR        CHAIN(N)  PFRCDF                             99
     C*
     C  N90SRDT          IFGT      SAVEDT
     C     SRDT          OREQ      SAVEDT
     C     SRTIME        ANDGT     SAVETM
     C                   SETON                                        85  90
     C                   ENDIF                                                  EPDT>SAVEDT
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   Z-ADD     *ZEROS        SCCLAS
     C                   MOVE      *BLANKS       SCSPEC
     C                   Z-ADD     SCCLAS        KKCLAS
     C                   Z-ADD     *ZEROS        KKEFDT
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   Z-ADD     0             KKCLAS
     C                   Z-ADD     *ZEROS        KKEFDT
     C                   MOVE      *BLANKS       KKSPEC
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCCLAS        KKCLAS
     C                   MOVE      SCSPEC        KKSPEC
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   SCR01S
     C*
     C                   Z-ADD     DBRRN         SCDBRR
     C*
     C                   Z-ADD     SREFDT        XXEFDT
     C                   Z-ADD     SREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   Z-ADD     SRCO#         SCCO#
     C                   Z-ADD     SRFND         SCFND
     C                   Z-ADD     SREMP#        SCEMP#
     C                   Z-ADD     SRDIV         SCDIV
     C                   Z-ADD     SRFYR         SCFYR
     C*
     C                   Z-ADD     SRCLAS        SCCLAS
     C                   MOVE      SRSPEC        SCSPEC
     C                   MOVE      SREXT         SCEXT
     C                   Z-ADD     SRADLD        SCADLD
     C*
     C                   MOVEL     *BLANKS       SCDESC
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C                   MOVE      *BLANKS       ##ST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N60              MOVEL     PXDESC        SCDESC
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXST          ##ST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C  N60              MOVEL     PXDESC        SCDESC
     C                   ENDIF
     C     SCDESC        IFEQ      *BLANKS
     C                   MOVEL     PRDESC        SCDESC
     C                   ENDIF                                                  SCDESC
     C*
     C                   MOVE      *ON           *IN08
     C                   MOVE      *ON           *IN40
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C*
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     SCADLD        SRADLD
     C*
     C                   ENDSR                                                  MVSCDB
     C*
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C     OTHOPT        BEGSR
     C                   ENDSR                                                  OTHOPT
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   MOVEA     '00000000'    *IN(21)
     C                   EXSR      CLRSF1
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09    INQUIRY
     C     @@MODE        COMP      '2'                                    09    INQUIRY
     C     @@MODE        COMP      '3'                                    09    INQUIRY
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C*
     C                   Z-ADD     @@RPMY        SCRPMY
     C                   Z-ADD     @@RPMY        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKRPMY
     C*
     C     @@RRN         CHAIN     DBRCDF2                            99
     C*
     C                   Z-ADD     ELFYR         SCFYR
     C                   Z-ADD     ELFND         SC1FND
     C                   Z-ADD     ELFYR         SC1FYR
     C*
     C     ELFAF         IFEQ      'A'
     C                   MOVEL(P)  AUDIT         SCPYST
     C                   ELSE
     C                   MOVEL(P)  EST           SCPYST
     C                   ENDIF                                                  ELFAF=A
     C*
     C*  GET EMPLOYER MASTER RECORD
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C                   Z-ADD     ELFND         SCFND
     C                   Z-ADD     ELEMP#        SCEMP#
     C                   Z-ADD     ELDIV         SCDIV
     C*
     C*  COMPRESS EMP#/DIV/NAME
     C*
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        XXEMP#
     C                   PARM      EMDIV         XXDIV
     C                   PARM      EMNAME        XXNAME
     C                   PARM                    XXEDN            80
     C*
     C                   MOVEL     XXEDN         SCEDN
     C*
     C                   MOVE      *OFF          *IN41
     C                   Z-ADD     0             SCCXDT
     C                   Z-ADD     1             I1
      *
     C     *ZERO         LOOKUP    EF(I1)                                 61
      *
     C     *IN61         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN61 IFEQ *ON
      *
     C     I1            IFNE      0
      *
     C     CX(I1)        IFNE      *ZERO
     C                   Z-ADD     CX(I1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCXDT
     C                   MOVE      *ON           *IN41
     C                   ENDIF                                                  CX,I1 IFNE *ZERO
      *
     C                   ENDIF                                                  I1 IFNE 0
     C*
     C*  GET GROUP INFORMATION
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*  GET AGENT RECORDS
     C*
     C                   CALL      AGTAPI        AGTPL
     C*
     C                   MOVEL     ##ANAM        SCANAM
     C                   MOVEL     ##PNAM        SCPNAM
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST              2
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
     C     RETRY         TAG
     C*
     C     KEY1          READE     DBRCDF                                 95
     C*
     C     *IN95         IFEQ      *OFF
     C                   MOVE      *BLANKS       ##ST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXST          ##ST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN95 IFEQ
     C*
     C     PXCODE        IFEQ      *ZERO
     C     *IN95         ANDEQ     *OFF
     C                   GOTO      RETRY
     C                   ENDIF
     C*
     C                   ENDSR                                                  READ
     C*
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
     C     RETRYP        TAG
     C*
     C     KEY1          READPE    DBRCDF                                 95
     C*
     C     *IN95         IFEQ      *OFF
     C                   MOVE      *BLANKS       ##ST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXST          ##ST
     C*** BEGIN CHG ***  06/01/06  ************************************
     C********** PXKY      CHAINPPX01                60
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C*** END CHG *****  06/01/06  ************************************
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN95 IFEQ
     C*
     C     PXCODE        IFEQ      *ZERO
     C     *IN95         ANDEQ     *OFF
     C                   GOTO      RETRYP
     C                   ENDIF
     C*
     C                   ENDSR                                                  READP
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C     KEY1A         SETLL     DBRCDF                             96
     C*
     C     *IN96         IFEQ      *ON
     C                   Z-ADD     *ZEROS        DBRRN
     C                   ENDIF                                                  *IN96=*ON
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*===============================================================
     C* UPDATR - SUBROUTINE FOR UPDATING DATA BASE RECORDS
     C*===============================================================
     C*
     C     UPDATR        BEGSR
     C*
     C     SCDBRR        CHAIN     PFRCDF                             60
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        SRUSER
     C                   MOVEL     Q1PGM         SRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRDT
     C                   MOVEL     DSTIME        SRTIME
     C*
     C                   UPDATE    PFRCDF
     C*
     C                   ENDSR                                                  UPDATR
     C*
     C*===============================================================
     C* WRITER - SUBROUTINE FOR WRITEING DATA BASE RECORDS
     C*===============================================================
     C*
     C     WRITER        BEGSR
     C*
     C                   ENDSR                                                  WRITER
     C*
** Column headings
Opt
** Description--------------MISN
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
203F3=Exit                  YYN
205F5=Refresh               YYY
212F12=Cancel               YYN
214F14=DARTS                NNN
3
