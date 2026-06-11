     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE N91DQR - TYPE DQ DISCOUNT CODE MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*    ALLOWS ENTRY OF SPECIFIC DQ DISCOUNT CODE INFORMATION      *
     H*    (I.E. SV CODE, SV MODs., PROC CODE, PROC MODs, FEE etc.    *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    N0920R                                                     *
     H*                                                               *
     H*  PROGRAMMER: C:212              REQ/PROJ: 114MQM              *
     H*  CREATED: 09/02/08                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H*****************************************************************
     FN91DQF    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     F                                     SFILE(WDW01S:RRN2)
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNMBDL5    UF A E           K DISK
     FNXDSL     UF A E           K DISK
     FNDCOL5    UF   E           K DISK
     F                                     RENAME(PCO01:PCO02)

     D UA              S             11    DIM(1) CTDATA PERRCD(1)
     D TD              S             70    DIM(1) CTDATA PERRCD(1)
     D ARR1            S              1    DIM(26) CTDATA PERRCD(26)
     D ARR2            S              1    DIM(10) CTDATA PERRCD(10)
     D CONST1          S             30    DIM(1) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIM                  1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D XXGNCD          DS
     D  XX1ST                  1      1
     D  XX2ND                  2      2
     D  XX3RD                  3      3
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  BILILNG CODE TABLE FILE
     C*
     C     BDKY1         KLIST
     C                   KFLD                    XXCODE            3
     C                   KFLD                    YMEFDT
     C                   KFLD                    XXTYPE            2
     C                   KFLD                    XXRCDT            1
     C*
     C     BDKY2         KLIST
     C                   KFLD                    XXCODE            3
     C                   KFLD                    YMEFDT
     C                   KFLD                    XXTYPE
     C*
     C*  CARVE-OUT TRANSACTION CODE FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCODE
     C                   KFLD                    YMEFDT
     C                   KFLD                    XXRCDE           10
     C*
     C     COKY2         KLIST
     C                   KFLD                    XXCODE
     C                   KFLD                    YMEFDT
     C*
     C     *LIKE         DEFINE    UPDT          YMEFDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCODE            3
     C                   PARM                    QQEFDT            8
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQEFDT        XXEFDT            8 0
     C                   MOVE      QQCODE        XXCODE
     C                   MOVE      'DQ'          XXTYPE
     C                   MOVE      'D'           XXRCDT
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         YMEFDT
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      'D'           XXLOST            1
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*** MOVE IN VALUES FROM PARMS TO SCREEN.
     C*
     C                   MOVE      'DQ'          SCTYPE
     C                   MOVEL     TD(1)         SCTDSC
     C                   MOVE      *BLANKS       SCRCDE
     C                   MOVE      *BLANKS       XXRCDE
     C*
 XXX C*** GET RECORD IF IT EXISTS, ELSE ITS AN ADD
     C*
     C     QQCODE        CHAIN     PDS01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVE      DSCODE        XXCODE
     C                   ENDIF
     C*
     C     BDKY1         CHAIN     PBD01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PBD01E
     C                   MOVE      BDCODE        SCCODE
     C                   MOVE      BDSTAT        SCSTAT
     C                   MOVE      BDCOM1        SCCOM1
     C                   MOVE      BDCOM2        SCCOM2
     C                   Z-ADD     BDAM01        SCAM01
     C                   Z-ADD     BDAM02        SCAM02
     C                   MOVE      BDLMRA        SCLMRA
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      UA(1)         SCMODE
     C*
     C                   ELSE                                                   *IN40 IFEQ '0'
     C*
     C                   MOVE      XXCODE        SCCODE
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C                   Z-ADD     *ZEROS        SCAM01
     C                   Z-ADD     *ZEROS        SCAM02
     C                   MOVE      *BLANKS       SCLMRA
     C                   Z-ADD     YMEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C                   MOVE      UA(1)         SCMODE
 XXX C                   MOVE      'A'           SCSTAT
     C*
     C                   Z-ADD     0             FLAG1             1 0
     C                   Z-ADD     1             I                 2 0
     C                   Z-ADD     1             J                 2 0
     C                   Z-ADD     0             K                 2 0
     C                   MOVEL     CONST1(1)     CONST2           30
     C                   EXSR      WRT2DB
     C                   GOTO      TERM
     C                   END
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*===============================================================
     C* INITIALIZATION    AREA
     C*
     C     TOP           TAG
     C                   EXSR      INZSR
     C*
     C*===============================================================
     C*
     C     MAINLN        TAG
     C*
     C*===============================================================
     C* SCREEN EXECUTION AREA
     C*
     C                   EXSR      SCREXC
     C*
     C*===============================================================
     C* HANDLE F KEYS IF PRESSED
     C*
     C     *IN11         CASEQ     '1'           FKEYS
     C                   END                                                      END *IN11
     C*
     C*   CHECK FOR USER SELECTION TO REPOSITION
     C*
     C     SCRCDE        IFNE      *BLANKS
     C*
     C                   MOVEL     SCRCDE        XXRCDE
     C                   MOVE      *BLANKS       SCRCDE
     C                   GOTO      TOP
     C                   END                                                    END SCRCDE
     C*
     C                   Z-ADD     RRN2          SVRRN2            5 0
     C*
     C                   READC     WDW01S                                 50
 XXX C     *IN50         IFEQ      '0'
     C*      PROCESSING IF SCRREN CHANGE OCCURS.
     C                   END                                                    END 0 *IN50
     C*
     C*===============================================================
     C* TERMINATION OF PROGRAM
     C*
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
     C/EJECT
     C*====================================================================*
     C* * *   INZSR SUBROUTINE                                    I N I T ***
     C*====================================================================*
     C     INZSR         BEGSR
     C*
     C     COKY          SETLL     PCO02                              60
     C*
     C*** CLEAR SUBFILE
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C*
     C                   EXSR      LOAD                                         LOAD RCDS FROM FILE
     C*
     C*
     C                   ENDSR
     C*====================================================================*
     C* * *   LOAD SUBROUTINE                                    L O A D ***
     C*====================================================================*
     C     LOAD          BEGSR
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN05
     C                   MOVE      '0'           *IN32
     C                   MOVE      '0'           *IN08
     C*
     C*   Position Cursor flag - used to check if the 1st subfile RRN
     C*     of the current page needs to be saved.
     C*
     C                   MOVE      'Y'           PCFL              1
     C*
     C*  *  *  *  LOAD THE SUBFILE SCREEN WITH UPTO 13 LINES
     C                   DO        9             X                 5 0
     C*
     C     COKY2         READE     PCO02                                  60
     C*
     C     *IN60         IFEQ      '0'
     C                   EXCEPT    PCO02E
     C*
     C                   ADD       1             RRN2              5 0
     C                   MOVEL     CO10A1        SC10A1
     C                   MOVEL     COSMOD        SCSMOD
     C                   MOVEL     COSMD2        SCSMD2
     C                   MOVEL     CO10A2        SC10A2
     C                   MOVEL     COPMOD        SCPMOD
     C                   MOVEL     COPMD2        SCPMD2
     C                   Z-ADD     COFEE         SCFEE
     C*
     C*   Save 1st subfile RRN of the current page
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2            5 0
     C                   MOVE      'N'           PCFL
     C                   END
     C                   WRITE     WDW01S
     C*
     C                   ELSE
     C*
     C                   MOVE      *BLANKS       SC10A1
     C                   MOVE      *BLANKS       SCSMOD
     C                   MOVE      *BLANKS       SCSMD2
     C                   MOVE      *BLANKS       SC10A2
     C                   MOVE      *BLANKS       SCPMOD
     C                   MOVE      *BLANKS       SCPMD2
     C                   Z-ADD     *ZEROS        SCFEE
     C                   ADD       1             RRN2
     C*
     C*   Save 1st subfile RRN of the current page
     C*
     C     PCFL          IFEQ      'Y'
     C                   Z-ADD     RRN2          XXRRN2
     C                   MOVE      'N'           PCFL
     C                   END
     C                   WRITE     WDW01S
     C*
     C                   END
     C                   END
     C*
     C*** TO POSITION SCREEN AND CURSOR
     C*
     C                   Z-ADD     XXRRN2        #@$2
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C     SCREXC        BEGSR
     C*===============================================================
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01C'
     C     Q2RECF        OREQ      'WDW01S'
     C                   MOVE      '1'           *IN01
     C                   WRITE     WDW01
     C                   EXFMT     WDW01C
     C                   MOVE      '0'           *IN01
     C     SVRRN2        IFNE      0
     C                   Z-ADD     SVRRN2        RRN2
     C                   Z-ADD     0             SVRRN2
     C                   END
     C                   END
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C     WRT2DB        BEGSR
     C*===============================================================
     C*
     C     QQCODE        CHAIN     PDS01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVE      DSCODE        XXGNCD
     C                   EXSR      WRTPBD
     C                   ELSE
     C     QQCODE        CHAIN     PBD01                              98
     C     *IN98         IFEQ      '1'
     C                   MOVE      QQCODE        XXGNCD
     C                   ELSE
     C                   EXSR      GENCOD
     C                   ENDIF
     C                   EXSR      WRTPDS
     C                   EXSR      WRTPBD
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C     GENCOD        BEGSR
     C*
     C* THIS SR GENERATES A DISCOUNT CODE
     C*===============================================================
     C*
     C     *IN98         DOWEQ     '0'
     C     FLAG1         ANDNE     1
     C                   EXSR      GENCD1
     C     XXGNCD        CHAIN     PBD01                              98
     C     *IN98         IFEQ      '1'
     C                   LEAVE
     C                   ENDIF
     C                   END
     C*
     C                   ENDSR
     C*===============================================================
     C     GENCD1        BEGSR
     C*
     C* THIS SR GENERATES A NEW CODE ON EVERY CALL
     C*===============================================================
     C*
     C     K             IFNE      26
     C                   ADD       1             K
     C                   ELSE
     C                   Z-ADD     1             K
     C     J             IFNE      26
     C                   ADD       1             J
     C                   ELSE
     C                   Z-ADD     1             J
     C     I             IFNE      10
     C                   ADD       1             I
     C                   ELSE
     C                   MOVE      1             FLAG1
     C                   CLEAR                   XXGNCD
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     FLAG1         IFNE      1
     C                   MOVE      ARR2(I)       XX1ST
     C                   MOVE      ARR1(J)       XX2ND
     C                   MOVE      ARR1(K)       XX3RD
     C                   ENDIF
     C*
     C                   ENDSR
     C*===============================================================
     C     WRTPDS        BEGSR
     C*===============================================================
     C*
     C                   CLEAR                   PDS01
     C*
     C                   MOVE      QQCODE        DSFCOD
     C                   MOVE      XXGNCD        DSCODE
     C                   MOVE      'A'           DSSTAT
     C                   MOVE      Q1USER        DSUSER
     C                   MOVE      Q1PGM         DSPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DSDT
     C*
     C                   MOVE      DSTIM         DSTIME
     C*
     C                   WRITE     PDS01
     C*
     C                   ENDSR
     C*===============================================================
     C     WRTPBD        BEGSR
     C*===============================================================
     C*
     C                   CLEAR                   PBD01
     C*
     C                   MOVE      XXGNCD        BDCODE
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDEFDT
     C*
     C                   MOVE      *BLANKS       COMMNT
     C     QQCODE        CAT       CONST2        COMMNT           50
     C     'NJ:'         CAT       COMMNT        COMMNT
     C                   MOVEL     COMMNT        BDCOM1
     C*
     C                   MOVE      XXTYPE        BDTYPE
     C                   MOVE      'D'           BDRCDT
     C                   MOVE      'A'           BDSTAT
     C                   MOVE      Q1USER        BDUSER
     C                   MOVE      Q1PGM         BDPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BDDT
     C*
     C                   MOVE      DSTIM         BDTIME
     C                   MOVE      Q1USER        BDIUSR
     C                   MOVE      QQCODE        BDMADG
     C*
     C                   WRITE     PBD01
     C*
     C                   ENDSR
     C*===============================================================
     C     FKEYS         BEGSR
     C*===============================================================
     C*
     C     *INKA         IFEQ      '1'
     C                   END
     C*
     C     *INKB         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F3 = EXIT                          *
     C****************************************
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END                                                    END *INKC
     C*
     C     *INKD         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F5 = REFRESH                       *
     C****************************************
     C     *INKE         IFEQ      '1'
     C*
     C*   Clear subfile
     C*
     C                   MOVE      '1'           *IN04
     C                   WRITE     WDW01C
     C                   MOVE      '0'           *IN04
     C*
     C                   Z-ADD     0             RRN2
     C                   Z-ADD     0             XXRRN2
     C*
     C     COKY          SETLL     PCO02                              60
     C*
     C                   EXSR      LOAD
     C*
     C                   MOVEL     *BLANKS       Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C                   GOTO      MAINLN
     C                   END
     C*
     C     *INKH         IFEQ      '1'
     C                   END
     C*
     C     *INKI         IFEQ      '1'
     C                   END
     C*
     C     *INKJ         IFEQ      '1'
     C                   END
     C*
     C     *INKK         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   F12 = CANCEL                       *
     C****************************************
     C     *INKL         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C     Q2RECF        OREQ      'WDW01S'
     C     Q2RECF        OREQ      'WDW01C'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C                   END                                                    END *INKL
     C*
     C     *INKM         IFEQ      '1'
     C                   END
     C*
     C     *INKN         IFEQ      '1'
     C                   END
     C*
     C     *INKP         IFEQ      '1'
     C                   END
     C*
     C     *INKQ         IFEQ      '1'
     C                   END
     C*
     C     *INKR         IFEQ      '1'
     C                   END
     C*
     C     *INKS         IFEQ      '1'
     C                   END
     C*
     C     *INKT         IFEQ      '1'
     C                   END
     C*
     C     *INKV         IFEQ      '1'
     C                   END
      *
     C     *INKW         IFEQ      '1'
     C                   END
     C*
     C****************************************
     C*   *IN18 -  ROLLUP REQUEST            *
     C****************************************
     C     *IN18         IFEQ      '1'                                          ROLLUP
     C     *IN32         CABEQ     '1'           MAINLN                         NO ROLL IF EOF
     C                   EXSR      LOAD
     C                   GOTO      MAINLN
     C                   END                                                    END 1 *IN18 ROLLUP
     C*
     C                   GOTO      MAINLN
     C                   ENDSR
     C*
     O*
     OPCO02     E            PCO02E
     OPBD01     E            PBD01E
**  TABLE UA
- DISPLAY -
**  TABLE TD
NETWORK DISC: CARVE OUT PER CODE
**  TABLE ARR1
ABCDEFGHIJKLMNOPQRSTUVWXYZ
**  TABLE ARR2
#@$%¢&*?+-
**  TABLE CONST1
-NEW NJ NETWORK
