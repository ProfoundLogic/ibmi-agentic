     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  01/16/09 C:227 PROJECT: OK019                                *
     H*                 OK 2008 UPDATES                               *
     H*                                                               *
     H*  01/21/08 C:47  PROJECT: MS015                                *
     H*                 MS: ACTIVATE SV92A4                           *
     H*                                                               *
     H*  07/20/05 C:79  PROJECT: 10MES                                *
     H*                 SC: ACTIVATE SV92A1, SV92A2                   *
     H*                                                               *
     H*  01/23/04 C:79  PROJECT: KS014                                *
     H*                 KS: ACTIVATE FAC FEE, PROF/TECH COMP          *
     H*                                                               *
     H*  02/25/03 C:79  PROJECT: FS256                                *
     H*                 USE FLEXIBLE HEADING FILE FDSFP               *
     H*                                                               *
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                               *
     H*  05/05/00 C:62 PROJECT:FSMNT                                  *
     H*                CHANGE OPTNM.                                  *
     H*                                                               *
     H* 12/02/99 C:79 PROJ:48MDA                                      *
     H*               1. Activate SV92A4 to outlier days              *
     H*                                                               *
     H* 11/04/99 C:79 PROJ:KS012                                      *
     H*               1. Activate SV92A4 to peer group lower limit    *
     H*               2. Activate SV92A5 to peer group upper limit    *
     H*                                                               *
     H* 12/16/98 C:79 PROJ:MS006                                      *
     H*               1. Activate SV92A1 to hold ASC facility fee     *
     H*               2. Activate SV92A2 to hold Prof comp amount     *
     H*               3. Activate SV92A3 to hold Tech comp amount     *
     H*****************************************************************
     FF2320F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMSVP     UF A E             DISK
     F                                     RENAME(PSV01:DBRCDF)
     FFMSVL1    IF   E           K DISK
     FSTABL     IF   E           K DISK
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*** BEG ADD *** 02/25/03 ****************************************
     FFDSFL     IF   E           K DISK
     F*** END ADD *** 02/25/03 ****************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D                 DS
     D  SCCMSV                 1     11
     D  SCRVCD                 1      3
     D  SCCPT4                 4      8
     D  SCUNIT                 9     11
     D*
     D DBFDS           DS
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*** BEGIN CHG *** 05/05/00 **************************************
     D**********    'Service Code Mai-    C         OPTNM
     D**********    'ntenance'
     D OPTNM           C                   CONST('Service Code Inf-
     D                                     ormation')
     D*** END   CHG *** 05/05/00 **************************************
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*** BEG DEL *** 02/25/03 ****************************************
     D************* 'Length of stay-      C         LENSTY
     D************* ' threshold . . :'
     D************* 'DRG low trimpoint-   C         LOWTRM
     D************* '  . . . . . :'
     D************* 'DRG high trimpoint-  C         HITRM
     D************* ' . . . . . :'
     D*** END DEL *** 02/25/03 ****************************************
     D*** BEG ADD *** 02/25/03 ****************************************
     D***** MRAHD           C                   CONST('MRA amount . . . .-
     D*****                                      . . . . . :')
     D MRAHD           C                   CONST('MRA amount        -
     D                                                :')
     I*** END ADD *** 02/25/03 ****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C*
     C* SEARCH PARM LIST
     C*
     C     ACDPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     SVKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCMSV
     C*** BEG ADD *** 06/29/98 ***************************************
     C                   KFLD                    KKEFDT
     C*** END ADD *** 06/29/98 ***************************************
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           KKKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##TBID
     C                   KFLD                    KKGKEY
     C*** BEG ADD *** 02/25/03 ***************************************
     C*
     C     SFKY          KLIST
     C                   KFLD                    K1ST
     C*** END ADD *** 02/25/03 ***************************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C*** BEG ADD *** 06/29/98 **************************************
     C     *LIKE         DEFINE    SVEFDT        KKEFDT
     C*** END ADD *** 06/29/98 **************************************
     C     *LIKE         DEFINE    SVCO#         ##CO#
     C     *LIKE         DEFINE    SVCMSV        KKCMSV
     C     *LIKE         DEFINE    STKEY         KKST
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTKEY         KKGKEY
     C*** BEG ADD *** 02/25/03 ***************************************
     C     *LIKE         DEFINE    SFST          K1ST
     C*** END ADD *** 02/25/03 ***************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* CHGSTS - SUBROUTINE FOR CHANGING STATUS
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C     SVSTAT        IFEQ      'I'
     C                   MOVE      'A'           SVSTAT
     C                   ELSE
     C                   MOVE      'I'           SVSTAT
     C                   ENDIF                                                  SVSTAT
     C*
     C                   MOVEL     Q1USER        SVUSER
     C                   MOVEL     Q1PGM         SVPGM
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SVDT
     C*
     C                   MOVEL     DSTIME        SVTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN99
     C*
     C                   ENDSR                                                  CHGSTS
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
     C* SERVICE CODE MUST NOT EQUAL BLANKS (IF IN ADD MODE)
     C*
     C   40
     CANN90SCCMSV        IFEQ      *BLANKS
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCKEY
     C*
     C* CHECK TO BE SURE SERVICE CODE HAS NOT BEEN USED PREVIOUSLY
     C*
     C                   MOVEL     SCCMSV        KKCMSV
     C*
     C   40
     CANN90SVKY          CHAIN     PSV01                              99
     C   40
     CANN90
     CANN99              SETON                                        72  90
     C*
     C* EFFECTIVE DATE MUST BE VALID.
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C*
     C                   SETON                                        7990
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* SERVICE CODE DESCRIPTION CANNOT BE BLANK
     C*
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        73  90
     C                   ENDIF                                                  SCDESC
     C*
     C* MUST HAVE VALID INPATIENT COVERAGE CODE
     C*
     C  N90SCFL01        IFNE      'N'
     C     SCFL01        ANDNE     'C'
     C*
     C                   SETON                                        7490
     C*
     C                   ENDIF
     C*
     C* MUST HAVE VALID OUTPATIENT COVERAGE CODE
     C*
     C  N90SCFL02        IFNE      'N'
     C     SCFL02        ANDNE     'C'
     C*
     C                   SETON                                        7590
     C*
     C                   ENDIF
     C*
     C* MUST HAVE VALID SERVICE TYPE
     C*
     C     @@ST          IFEQ      'LA'
     C*
     C                   MOVEL     'LASV'        ##TBID
     C*
     C                   ELSE
     C*
     C                   MOVEL     'SVCT'        ##TBID
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCTYPE        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N90
     CAN 99              SETON                                        7690
     C*** BEGIN DEL *** 03/03/98 **************************************
     C*
     C* MUST HAVE VALID TYPE OF CHARGE
     C*
     C**********           MOVEL'CHGT'    ##TBID
     C**********           MOVELSCTSRV    KKGKEY    P
     C*
     C********** GTKY      CHAINPGT02                99
     C*
     C*
     C***33N90*99          SETON                     7790
     C*** END   DEL *** 03/03/98 **************************************
     C*
     C* CHECK TO BE SURE SCREEN HAS NOT BEEN UPDATED SINCE ENTRY
     C*
     C  N90SVDT          IFGT      SAVEDT
     C     SVDT          OREQ      SAVEDT
     C     SVTIME        ANDGT     SAVETM
     C                   SETON                                        9078
     C                   ENDIF
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C* WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C* SERVICE TYPE SEARCH
     C*
     C     HFLD          WHENEQ    'SCTYPE'
     C     @@ST          ANDEQ     'LA'
     C*
     C                   MOVEL     'LASV'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        81
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SCTYPE
     C                   SETON                                        82
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCTYPE'
     C     @@ST          ANDNE     'LA'
     C*
     C                   MOVEL     'SVCT'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        81
     C*
     C                   ELSE
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C                   MOVEL     GTKEY         SCTYPE
     C*** BEG CHG *** 06/29/98 ************************************
     C******************** MOVELGTKEY     SCTYPD
     C                   MOVEL     GTDESC        SCTYPD
     C*** END CHG *** 06/29/98 ************************************
     C                   SETON                                        82
     C*
     C                   ENDIF
     C*** BEGIN DEL *** 03/03/98 **************************************
     C*
     C* TYPE OF CHARGE
     C*
     C********** HFLD      WHEQ 'SCTSRV'
     C*
     C**********           MOVEL'CHGT'    ##TBID
     C*
     C**********           CALL 'F2380R'  ACDPL
     C*
     C********** *INKL     IFEQ *ON
     C********** ##RRN     OREQ *ZEROS
     C*
     C**********           SETON                     82
     C*
     C**********           ELSE
     C*
     C********** ##RRN     CHAINPGT01                99
     C*
     C**********           MOVELGTKEY     SCTSRV
     C**********           MOVELGTDESC    SCTSRD
     C*
     C**********           ENDIF
     C*** END   DEL *** 03/03/98 **************************************
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MKSTHD - Creates the state header
     C*================================================================
     C     MKSTHD        BEGSR
     C*
     C* DISPLAY STATE CODE AND STATE NAME IN HEADER
     C*
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          SCST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C                   MOVEL(P)  STNAME        SCSTNM
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     SVCMSV        SCCMSV
     C*
     C                   Z-ADD     SVEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVEL     SVDESC        SCDESC
     C                   MOVE      SVALOW        SCALOW
     C                   MOVE      SVINPC        SCINPC
     C                   MOVE      SVOTPC        SCOTPC
     C                   MOVEL     SVFL01        SCFL01
     C                   MOVEL     SVFL02        SCFL02
     C                   MOVEL     SVTYPE        SCTYPE
      *** BEG ADD *** 12/16/98 ****************************************
     C*** BEG CHG *** 02/25/03 ****************************************
     C******************** Z-ADDSV92A1    SCFACF
     C******************** Z-ADDSV92A2    SCPROF
     C******************** Z-ADDSV92A3    SCTECH
     C                   Z-ADD     SV92A1        SC92A1
     C                   Z-ADD     SV92A2        SC92A2
     C                   Z-ADD     SV92A3        SC92A3
     C*** END CHG *** 02/25/03 ****************************************
      *** END ADD *** 12/16/98 ****************************************
      *** BEG ADD *** 11/04/99 ****************************************
      *** BEG ADD *** 12/02/99 ****************************************
     C*
     C     @@ST          IFEQ      'GA'
     C*
     C*** BEG CHG *** 02/25/03 ****************************************
     C******************** Z-ADDSV92A4    SC92A4
     C                   Z-ADD     SV92A4        SC92A1
     C*** END CHG *** 02/25/03 ****************************************
     C*
     C                   ENDIF                                                  @@ST IFEQ GA
      *** END ADD *** 12/02/99 ****************************************
     C*
     C     @@ST          IFEQ      'KS'
     C*
     C*** BEG CHG *** 02/25/03 ****************************************
     C******************** Z-ADDSV92A4    SC92A4
     C******************** Z-ADDSV92A5    SC92A5
     C                   Z-ADD     SV92A4        SC92A1                         LOW TRIM
     C                   Z-ADD     SV92A5        SC92A2                         HIGH TRIM
     C*** END CHG *** 02/25/03 ****************************************
     C*** BEG ADD *** 01/23/04 ****************************************
     C                   Z-ADD     SV92A1        SC92A3                         FAC FEE
     C                   Z-ADD     SV92A2        SC92A4                         PROF COMP
     C                   Z-ADD     SV92A3        SC92A5                         TECH COMP
     C*** END ADD *** 01/23/04 ****************************************
     C*
     C                   ENDIF                                                  @@ST IFEQ KS
      *** END ADD *** 11/04/99 ****************************************
     C*
     C     @@ST          IFEQ      'LA'
     C*
     C                   MOVEL     'LASV'        ##TBID
     C*
     C                   ELSE
     C*
     C                   MOVEL     'SVCT'        ##TBID
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCTYPE        KKGKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C  N99              MOVEL     GTDESC        SCTYPD
     C   99              MOVEL     *BLANKS       SCTYPD
     C*
     C*** BEGIN DEL *** 03/03/98 **************************************
     C**********           MOVELSVTSRV    SCTSRV
     C*
     C**********           MOVEL'CHGT'    ##TBID
     C**********           MOVELSCTSRV    KKGKEY    P
     C*
     C********** GTKY      CHAINPGT02                99
     C*
     C**N99*****           MOVELGTDESC    SCTSRD
     C***99*****           MOVEL*BLANKS   SCTSRD
     C*** END   DEL *** 03/03/98 **************************************
     C*
     C                   MOVEL     SVSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANKS       SCCMSV
     C*** BEG ADD *** 06/29/98 **************************************
     C                   MOVE      '000'         SCUNIT
     C*** END ADD *** 06/29/98 **************************************
     C                   MOVE      *ZEROS        SCEFDT
     C                   MOVEL     *BLANKS       SCDESC
     C                   MOVE      *ZEROS        SCALOW
     C                   MOVE      *ZEROS        SCINPC
     C                   MOVE      *ZEROS        SCOTPC
     C                   MOVEL     *BLANKS       SCFL01
     C                   MOVEL     *BLANKS       SCFL02
     C                   MOVEL     *BLANKS       SCTYPE
     C*** BEGIN DEL *** 03/03/98 **************************************
     C**********           MOVEL*BLANKS   SCTSRV
     C*** END   DEL *** 03/03/98 **************************************
     C                   MOVEL     'A'           SCSTAT
      *** BEG ADD *** 12/16/98 ****************************************
     C*** BEG CHG *** 02/25/03 ****************************************
     C******************** MOVE *ZEROS    SCFACF
     C******************** MOVE *ZEROS    SCPROF
     C******************** MOVE *ZEROS    SCTECH
     C                   MOVE      *ZEROS        SC92A1
     C                   MOVE      *ZEROS        SC92A2
     C                   MOVE      *ZEROS        SC92A3
     C*** END CHG *** 02/25/03 ****************************************
      *** END ADD *** 12/16/98 ****************************************
      *** BEG ADD *** 11/04/99 ****************************************
     C                   MOVEL     *ZEROS        SC92A4
     C                   MOVEL     *ZEROS        SC92A5
      *** END ADD *** 11/04/99 ****************************************
     C*
     C                   ENDIF
     C*
     C*** BEG ADD *** 01/21/08 ****************************************
     C*
     C     @@ST          IFEQ      'MS'
     C                   Z-ADD     SV92A4        SC92A4
     C                   ENDIF                                                  @@ST IFEQ MS
     C*** BEG ADD *** 01/21/08 ****************************************
     C*
     C*** BEG ADD *** 01/16/09 ****************************************
     C     @@ST          IFEQ      'OK'
     C                   Z-ADD     SVRELW        SCRELW
     C                   ENDIF                                                  @@ST IFEQ OK
     C*** END ADD *** 01/16/09 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     ##CO#         SVCO#
     C                   MOVEL     SCCMSV        SVCMSV
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SVEFDT
     C*
     C                   MOVEL     SCDESC        SVDESC
     C                   MOVE      SCALOW        SVALOW
     C                   MOVE      SCINPC        SVINPC
     C                   MOVE      SCOTPC        SVOTPC
     C                   MOVEL     SCFL01        SVFL01
     C                   MOVEL     SCFL02        SVFL02
     C                   MOVEL     SCTYPE        SVTYPE
     C*** BEGIN DEL *** 03/03/98 **************************************
     C**********           MOVELSCTSRV    SVTSRV
     C*** END   DEL *** 03/03/98 **************************************
     C                   MOVEL     SCSTAT        SVSTAT
      *** BEG ADD *** 12/16/98 ****************************************
     C*** BEG CHG *** 02/25/03 ****************************************
     C******************** Z-ADDSCFACF    SV92A1
     C******************** Z-ADDSCPROF    SV92A2
     C******************** Z-ADDSCTECH    SV92A3
     C                   Z-ADD     SC92A1        SV92A1
     C                   Z-ADD     SC92A2        SV92A2
     C                   Z-ADD     SC92A3        SV92A3
     C*** END CHG *** 02/25/03 ****************************************
      *** END ADD *** 12/16/98 ****************************************
      *** BEG ADD *** 12/02/99 ****************************************
     C*
     C     @@ST          IFEQ      'GA'
     C*
     C*** BEG CHG *** 02/25/03 ****************************************
     C******************** Z-ADDSC92A4    SV92A4
     C                   Z-ADD     SC92A1        SV92A4
     C*** END CHG *** 02/25/03 ****************************************
     C*
     C                   ENDIF                                                  @@ST IFEQ GA
      *** END ADD *** 12/02/99 ****************************************
      *** BEG ADD *** 11/04/99 ****************************************
     C*
     C     @@ST          IFEQ      'KS'
     C*
     C*** BEG CHG *** 02/25/03 ****************************************
     C******************** Z-ADDSC92A4    SV92A4
     C******************** Z-ADDSC92A5    SV92A5
     C                   Z-ADD     SC92A1        SV92A4
     C                   Z-ADD     SC92A2        SV92A5
     C*** END CHG *** 02/25/03 ****************************************
     C*** BEG ADD *** 01/23/04 ****************************************
     C                   Z-ADD     SC92A3        SV92A1                         FAC FEE
     C                   Z-ADD     SC92A4        SV92A2                         PROF COMP
     C                   Z-ADD     SC92A5        SV92A3                         TECH COMP
     C*** END ADD *** 01/23/04 ****************************************
     C*
     C                   ENDIF                                                  @@ST IFEQ KS
      *** END ADD *** 11/04/99 ****************************************
     C*
     C*** BEG ADD *** 01/21/08 ****************************************
     C*
     C     @@ST          IFEQ      'MS'
     C                   Z-ADD     SC92A4        SV92A4
     C                   ENDIF                                                  @@ST IFEQ MS
     C*** BEG ADD *** 01/21/08 ****************************************
     C*
     C*** BEG ADD *** 01/16/09 ****************************************
     C     @@ST          IFEQ      'OK'
     C                   Z-ADD     SCRELW        SVRELW
     C                   ENDIF                                                  @@ST IFEQ OK
     C*** END ADD *** 01/16/09 ****************************************
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
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*** BEGIN ADD *** 08/15/00 **************************************
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1
     C                   PARM      *BLANK        ##FBIF            1
     C                   PARM      *BLANK        ##FL01            1
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*** END   ADD *** 08/15/00 **************************************
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C* Executing the sub-routine to make the state header.
     C*
     C                   EXSR      MKSTHD
     C*** BEG ADD *** 02/25/03 ****************************************
     C*
     C* Executing the sub-routine to assign flexible headings per state
     C*
     C                   EXSR      SVCDHD
     C*** END ADD *** 02/25/03 ****************************************
     C*
     C*  CHECK FOR A/R STATUS
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           MOVEL'FB'      KKKEY     P
     C*
     C********** FBKY      CHAINSFB01                99
     C*
     C********** *IN99     IFEQ '0'
     C*** END   DEL *** 08/15/00 **************************************
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** FBFL06    ANDEQ'Y'
     C     ##NTWK        IFEQ      'Y'
     C*** END   CHG *** 08/15/00 **************************************
     C*
     C                   SETON                                        33
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       33
     C*
     C                   ENDIF                                                  END FBFL06
     C*
     C*  CHECK TO SEE IF STATE IS LA
     C*
     C     @@ST          IFEQ      'LA'
     C*
     C                   SETON                                        34
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       34
     C*
     C                   ENDIF                                                  @@ST = LA
     C*** BEG ADD 01/16/09 ********************************************
     C     @@ST          IFEQ      'OK'
     C                   SETON                                        37
     C                   ELSE
     C                   SETOFF                                       37
     C                   ENDIF                                                  @@ST = OK
     C*** END ADD 01/16/09 ********************************************
     C*
      *** BEG ADD *** 12/02/99 ****************************************
     C*
     C     @@ST          IFEQ      'GA'
     C*
     C*** BEG CHG *** 02/25/03 ****************************************
     C******************** SETON                     35
     C                   SETON                                        32
     C*** END CHG *** 02/25/03 ****************************************
     C*** BEG DEL *** 02/25/03 ****************************************
     C******************** MOVELLENSTY    SCN4HD    P
     C*** END DEL *** 02/25/03 ****************************************
     C*
     C                   ENDIF                                                  @@ST IFEQ GA
      *** END ADD *** 12/02/99 ****************************************
     C*** BEG ADD 11/04/99 ********************************************
     C*  CHECK TO SEE IF STATE IS KS
     C*
     C     @@ST          IFEQ      'KS'
     C*
     C*** BEG CHG *** 02/25/03 ****************************************
     C******************** SETON                     3536
     C                   SETON                                        3231
     C*** END CHG *** 02/25/03 ****************************************
     C*** BEG ADD *** 01/23/04 ****************************************
     C                   SETON                                        343536
     C*** END ADD *** 01/23/04 ****************************************
     C*** BEG DEL *** 02/25/03 ****************************************
     C**********           MOVELLOWTRM    SCN4HD    P
     C**********           MOVELHITRM     SCN5HD    P
     C*** END DEL *** 02/25/03 ****************************************
     C*
     C                   ENDIF                                                  @@ST = KS
     C*** END ADD 11/04/99 ********************************************
     C*
     C*** BEG ADD *** 02/25/03 ****************************************
     C     @@ST          IFEQ      'MS'
     C                   SETON                                        3231
     C                   SETON                                        34
     C*** BEG ADD *** 01/21/08 ****************************************
     C                   SETON                                        35
     C*** END ADD *** 01/21/08 ****************************************
     C                   ENDIF                                                  @@ST = MS
     C*** END ADD *** 02/25/03 ****************************************
     C*
     C*** BEG ADD *** 07/20/05 ****************************************
     C     @@ST          IFEQ      'SC'
     C                   SETON                                        3231
     C                   ENDIF                                                  @@ST = MS
     C*** END ADD *** 07/20/05 ****************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
     C*** BEG ADD *** 02/25/03 ****************************************
     C*================================================================
     C* SVCDHD - Assign flexible headings per state
     C*================================================================
     C     SVCDHD        BEGSR
     C*
     C                   MOVE      SCST          K1ST
     C*
     C     SFKY          CHAIN     PSF01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVE(P)   SFHALW        SCHALW
     C                   MOVE(P)   SF92H1        SC92H1
     C                   MOVE(P)   SF92H2        SC92H2
     C                   MOVE(P)   SF92H3        SC92H3
     C                   MOVE(P)   SF92H4        SC92H4
     C                   MOVE(P)   SF92H5        SC92H5
     C*
     C                   ELSE
     C*****              MOVE      MRAHD         SCHALW
     C                   MOVEL     MRAHD         SCHALW
     C                   MOVE      *BLANKS       SC92H1
     C                   MOVE      *BLANKS       SC92H2
     C                   MOVE      *BLANKS       SC92H3
     C                   MOVE      *BLANKS       SC92H4
     C                   MOVE      *BLANKS       SC92H5
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ENDSR
     C*** END ADD *** 02/25/03 ****************************************
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        SVUSER
     C                   MOVEL     Q1PGM         SVPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SVDT
     C*
     C                   MOVEL     DSTIME        SVTIME
     C*
     C                   UPDATE    DBRCDF
     C*
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C*
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        SVUSER
     C                   MOVEL     Q1PGM         SVPGM
     C                   MOVEL     Q1USER        SVIUSR
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SVDT
     C*
     C                   MOVEL     SRDTO         SVIDT
     C*
     C                   MOVEL     DSTIME        SVTIME
     C*
     C                   WRITE     DBRCDF
     C*
     C                   FEOD      FMSVP
     C*
     C                   ENDSR
