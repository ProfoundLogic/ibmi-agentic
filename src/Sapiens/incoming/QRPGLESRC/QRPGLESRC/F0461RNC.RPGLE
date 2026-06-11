     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H*  Program: F0461RNC                                            *
     H*  Description: Work With EOMBs Header Info update/display      *
     H*               North Calorina VERSION/HOSPITAL BILLS           *
     H*                                                               *
     H*  Created:  C:296  02/26/2014                                  *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*                                                               *
     H*****************************************************************
     FF0461FNC  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*  Physical file definition (for maintaining record)
     FFAEUP     UF A E             DISK
     F                                     RENAME(PEU01:DBRCDF)
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     FFAEAL1    UF   E           K DISK
     F*
     FFMCDL     IF   E           K DISK
     FFDCALF    IF   E           K DISK
     FFMDIL     IF   E           K DISK
     F*  GENERIC TABLE FILE
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*  PROCEDURE CODE MASTER FILE
     FFMPRL     IF   E           K DISK
     F*  RESONABLE AND CUSTOMARY FILE
     FFMRCP     IF   E             DISK
     FFMRCL1    IF   E           K DISK
     F                                     RENAME(PRC01:PRC02)
     FFTDGP     IF   E             DISK
     FFTDGL     IF   E           K DISK
     F                                     RENAME(PDG01:PDG02)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Hospital (UB) -
     D                                     Header Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(2)
     D*
     D*   Data structure of FL License # to make sure first two
     D*     characters of license # are alpha
     D                 DS
     D  SCLIC#                 1      9
     D  SCLIC1                 1      1
     D  SCLIC2                 2      2
     D  SCLIC3                 3      9
     D NT0             C                   CONST('Field was changed fo-
     D                                     r EDI correction:')
     D NT1             C                   CONST('Original Value:')
     D FL1const        C                   CONST('Referring provider s-
     D                                     tate lic#')
     D FL2const        C                   CONST('Drg code')
     D FL3const        C                   CONST('Discharge date')
     D                 DS
     D  XXNOT1                 1     70
     D  XXNT1                  1     38
     D  XXNT2                 39     70
     D                 DS
     D  XXNOT2                 1     70
     D  XXNT21                 1     16
     D  XXNT22                17     70


     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FOR FAEUP
     C                   PARM                    @@RRN2            9 0          FOR FAEOP
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     APIPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      EOSUB#        #2SUB#            5 0
     C                   PARM      EOFND         #2FND             3 0
     C                   PARM      EOFYR         #2FYR             3 0
     C                   PARM      EOCASE        #2CASE            7 0
     C                   PARM      EOEOB#        #2EOB#            9 0
     C                   PARM      EORCN         #2RCN             3 0
     C                   PARM                    #2DSEQ            5 0
     C                   PARM      'H'           #2CALL            1
     C                   PARM                    #2OVAL           70
     C                   PARM                    #2NVAL           70
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*   KEY LIST FOR PAYMENT PLAN UPDATE
     C*
     C     PTKEY         KLIST
     C                   KFLD                    EUCO#
     C                   KFLD                    EUSUB#
     C                   KFLD                    EUFND
     C                   KFLD                    EUFYR
     C                   KFLD                    EUCASE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXDIA
     C*
     C*  KEY LIST FOR UPDATING EAEDI
     C*
     C     EAKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* Key list for Generic Table File
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*
     C* KEY LIST FOR PROCEDURE CODE MASTER FILE
     C*
     C     PRKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C                   KFLD                    KKEFDT
     C*
     C     PRKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C*
     C* KEY LIST FOR REASONABLE AND CUSTOMARY FILE
     C*
     C     RCKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCPT4
     C*
     C*  Key List for Diagnosis Code File
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C     DGKY          KLIST
     C                   KFLD                    KKDRG#
     C                   KFLD                    KKEFDT
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CACO#         QQCO#
     C     *LIKE         DEFINE    CASUB#        KKSUB#
     C     *LIKE         DEFINE    CAFND         KKFND
     C     *LIKE         DEFINE    CAFYR         KKFYR
     C     *LIKE         DEFINE    CACASE        KKCASE
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    CACPT4        KKCPT4
     C     *LIKE         DEFINE    CASS#         KKSS#
     C     *LIKE         DEFINE    CAEOB#        KKEOB#
     C     *LIKE         DEFINE    EUDIA1        XXDIA
     C     *LIKE         DEFINE    EUDIA1        XXDIA1
     C     *LIKE         DEFINE    DILOST        XXLOST
     C     *LIKE         DEFINE    DIICD9        XXICD9
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    DIICD9        ##ICD9
     C     *LIKE         DEFINE    DILOST        ##LOST
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C     *LIKE         DEFINE    SCPCM         HLPCM
     C     *LIKE         DEFINE    SCPDSC        HLDESC
     C     *LIKE         DEFINE    PREFDT        KKEFDT
     C     *LIKE         DEFINE    CAFDAT        HLRCVD
     C     *LIKE         DEFINE    CAACDT        HLACDT
     C     *LIKE         DEFINE    CAACDT        HLSYDT
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    EOBLST        KKBLST
     C     *LIKE         DEFINE    EUDIA1        HLDIA1
     C     *LIKE         DEFINE    EUDIA1        HLDIA2
     C     *LIKE         DEFINE    EUDIA1        HLDIA3
     C     *LIKE         DEFINE    EUDIA1        HLDIA4
     C     *LIKE         DEFINE    DILOST        HLLOS1
     C     *LIKE         DEFINE    DILOST        HLLOS2
     C     *LIKE         DEFINE    DILOST        HLLOS3
     C     *LIKE         DEFINE    DILOST        HLLOS4
     C     *LIKE         DEFINE    EUDCN         HLTSS
     C     *LIKE         DEFINE    EOAST         EDITST
     C     *LIKE         DEFINE    DGDRG#        KKDRG#
     C     *LIKE         DEFINE    EUFLIC        XOFLIC
     C     *LIKE         DEFINE    EUDRG#        XODRG#
     C     *LIKE         DEFINE    EUDCDT        HLDCDT
     C     *LIKE         DEFINE    SCDCDT        XODCDT
     C     *LIKE         DEFINE    EUADDT        HLADDT
     C     *LIKE         DEFINE    EUADDT        HLSCPF
     C     *LIKE         DEFINE    EUADDT        XXADDT
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
     C* VALIDATE DISCHARGE DATE
     C*
     C     EUICN         IFEQ      'IR'
     C     EUICN         OREQ      'IP'
     C     EUICN         OREQ      'IM'
     C     EUICN         OREQ      'IS'
     C     SCDCDT        IFEQ      *ZEROS
     C                   SETON                                        8190
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDCDT        IFNE      *ZEROS
     C                   Z-ADD     SCDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCDCDT
     C                   ENDIF
     C                   ENDIF                                                  SCDCDT NE *ZEROS
     C*
     C* DISCHARGE DATE CANNOT BE LESS THAN ADMISSION DATE
     C*
     C                   Z-ADD     SCDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLDCDT
     C*
     C     HLDCDT        IFNE      *ZEROS
     C     HLDCDT        ANDLT     EUADDT
     C                   SETON                                        9083
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*
     C* Procedure Code Method is required
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        906746
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C
     C* Validate Procedure Code Method
     C
     C     SCPCM         IFNE      '0'
     C     SCPCM         ANDNE     '4'
     C     SCPCM         ANDNE     '5'
     C     SCPCM         ANDNE     '9'
     C                   SETON                                        906846
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPCM NE '0'
     C*
     C* If Proc Code Method =0 Principal Procedure Code MUST be BLANK
     C
     C     SCPCM         IFEQ      '0'
     C     SCPRC1        ANDNE     *BLANKS
     C                   eval      indPRC1A1 = '1'
     C                   SETON                                        9039
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C
     C* Validate procedure codes base on procedure code methods entered
     C* if Proc Code Method NE 0 THEN Princ Procedure Code Required
     C     SCPCM         IFNE      '0'
     C
     C     SCPCM         IFEQ      '4'
     C                   MOVEL     SCPRC1        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        906939
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPCM=4
     C
     C     SCPCM         IFEQ      '5'
     C                   MOVEL(P)  SCPRC1        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        906939
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPCM=5
     C
     C     SCPCM         IFEQ      '9'
     C                   MOVEL(P)  SCPRC1        KKICD9
     C                   MOVE      SCLS1         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        906939
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPCM =9
     C
     C
     C                   ENDIF                                                  SCPCM NE 0
     C
     C*  Make sure that Type of Service Status is "F" or "I"
     C*
     C     SCTSS         IFEQ      *BLANKS
     C                   SETON                                        907237
     C                   ELSE
     C     SCTSS         IFEQ      'F'
     C     SCTSS         OREQ      'I'
     C                   SETOFF                                       72
     C     SCTSS         IFEQ      'F'
     C                   MOVEL     'FINAL'       SCTSSD
     C                   ENDIF
     C     SCTSS         IFEQ      'I'
     C                   MOVEL     'INTRIM'      SCTSSD
     C                   ENDIF
     C                   ELSE
     C                   SETON                                        907237
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Make sure that Data Subm flag is either "Y" or "N" & not
     C*
     C     SCRLIC        IFEQ      *BLANKS
     C                   SETON                                        7790
     C                   GOTO      CK1END
     C                   ENDIF
     C* VALIDATE DRG#
     C*
     C     EUICN         IFEQ      'IR'
     C     EUICN         OREQ      'IP'
     C     EUICN         OREQ      'IM'
     C     EUICN         OREQ      'IS'
     C     SCDRG#        IFEQ      *ZEROS
     C     *IN48         IFEQ      *ON
     C                   SETON                                        9079
     C                   ENDIF
     C                   ELSE
     C     SCDRG#        IFNE      0
     C     SCDRG#        CHAIN     PDG02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9080
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99=*ON
     C                   ENDIF                                                  SCDRG#<>0
     C                   ENDIF                                                  SCDRG#=0
     C                   ENDIF
     C*
     C* VALIDATE TYPE OF ADMISSION
     C*
     C                   MOVEL(P)  'NCTA'        KKTBID
     C                   MOVEL(P)  SCADTY        KKKEY
     C*
     C     GTKY          CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *ON
     C     GTSTAT        OREQ      'I'
     C                   SETON                                        9051
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C* VALIDATE ADMISSION DATE IF ENTERED
     C*
     C     SCADDT        IFNE      *ZEROS
     C                   Z-ADD     SCADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9081
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SRDTO         SCADDT
     C                   ENDIF
     C                   ENDIF                                                  SCADDT NE *ZEROS
     C                   Z-ADD     SCADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLADDT
     C* ADMISSION DATE CANNOT BE LESS THAN ACCIDENT DATE
     C*
     C     HLADDT        IFNE      *ZEROS
     C     HLADDT        ANDLT     EOACDT
     C                   SETON                                        9089
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*
     C* ADMISSION DATE CANNOT BE GREATER THAN SYSTEM DATE
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSYDT
     C*
     C     HLADDT        IFNE      *ZEROS
     C     HLADDT        ANDGT     HLSYDT
     C                   SETON                                        9050
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C* ADMISSION DATE CANNOT BE GREATER THAN STATEMENT COVERS FROM
     C*
     C*    HLADDT        IFNE      *ZEROS
     C*    HLADDT        ANDGT     HLSCPF
     C*                  SETON                                        9067
     C*                  GOTO      CK1END
     C*                  ENDIF                                                  HLADDT NE *ZEROS
     C***  SCADDT        IFNE      *ZEROS
     C***  SCADHR        ANDEQ     *ZEROS
     C* Admission hr is required for all inpatient except for skilled nursing (b
     C***  SCICN         IFEQ      'IR'
     C***  SCICN         OREQ      'IP'
     C***  SCICN         OREQ      'IM'
     C***  SCICN         OREQ      'IS'
     C***                MOVEL     SCBLTY        XXBLT             2
     C***  XXBLT         IFLT      '21'
     C***  XXBLT         ORGT      '22'
     C***
     C***                SETON                                        9093
     C***                GOTO      CK1END
     C***                ENDIF                                                  XXBLT  <> '21' - '22
     C***
     C***                ELSE
     C***                MOVEL     SCBLTY        XXBLTY            1
     C***  XXBLTY        IFGE      '1'
     C***  XXBLTY        ANDLE     '3'
     C***  XXBLTY        OREQ      '7'
     C***                SETON                                        9093
     C***                GOTO      CK1END
     C***                ENDIF                                                  XXBLTY IFGE
     C***                ENDIF                                                  INPATIENT BILL
     C***                ENDIF                                                  SCADDT IFNE
     C* ADMIT HOUR AND DISCHARGE HOUR MUST BE BETWEEN 1 AND 24
     C* IF ENTERED
     C*
     C     SCADHR        IFNE      *ZEROS
     C     SCADHR        IFGT      24
     C                   SETON                                        9051
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCADHR LT 1
     C                   ENDIF                                                  SCADHR NE *ZEROS
     C* DISCHARGE DATE CANNOT BE LESS THAN ADMISSION DATE
     C*
     C*
     C                   Z-ADD     SCDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLDCDT
     C*
     C     HLDCDT        IFNE      *ZEROS
     C     HLDCDT        ANDLT     HLADDT
     C                   SETON                                        9059
     C                   GOTO      CK1END
     C                   ENDIF                                                  HLADDT NE *ZEROS
     C*
     C* VALIDATE DRG#
     C*
     C     *IN09         IFNE      *ON
     C***  *IN29         ANDNE     *ON
     C***  SCICN         ANDEQ     'IM'
     C***  SCICN         OREQ      'IP'
     C***  SCICN         OREQ      'IR'
     C***  SCICN         OREQ      'IS'
     C     SCDRG#        IFEQ      0
      **** REQUIRED ONLY WHEN ADMISSION DATE > 03/01/2008
     C                   Z-ADD     SCADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXADDT
     C     XXADDT        IFGE      1080301
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ELSE
     C     SCDRG#        CHAIN     PDG02                              99
     C     *IN99         IFEQ      *ON
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C* Patient account number cannot not be blank.
      /Free
         if SCACCT = ' ' ;
            *in90 = '1' ;
            *IN54 = '1' ;
      /End-Free
     C                   GOTO      CK1END
      /Free
         endif ;
      /End-Free
     C*
     C* Provider required when last/group name is present.
      /Free
         if SCRLSN <> ' ' and SCRFEI = 0 ;
            *in90 = '1' ;
            *IN56 = '1' ;
      /End-Free
     C                   GOTO      CK1END
      /Free
         endif ;
      /End-Free
     C*
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* Setoff all indicator in screen 2.
     C                   Setoff                                       2123
     C                   eval      indPRC2A2 = '0'
     C                   eval      indPRC3A2 = '0'
     C                   eval      indPRC4A2 = '0'
     C                   eval      indPRC3A1 = '0'
     C                   eval      indPRC4A1 = '0'
     C                   eval      indDIA1   = '0'
     C                   eval      indDIA2   = '0'
     C                   eval      indDIA3   = '0'
     C                   eval      indDIA4 = '0'
     C* PRINCIPLE PROCEDURE DATE IS REQUIRED IF PRINCIPLE PROC IS NOT BLANK
     C*
     C*    SCPRC1        IFNE      *BLANKS
     C*    SCPRD1        ANDEQ     *ZEROS
     C*                  SETON                                        9023
     C*                  GOTO      CK2END
     C*                  ENDIF
     C*
     C*
     C* VALIDATE THE PRINCIPLE PROCEDURE DATE
     C*
     C*                  Z-ADD     SCPRD1        SRDTI
     C*                  MOVE      *BLANKS       SRDTO
     C*                  MOVE      *BLANKS       SRERR
     C*                  CALL      'SRCVTDAT'    DATCHK
     C*    SRERR         IFEQ      '1'
     C*                  SETON                                        9021
     C*                  GOTO      CK2END
     C*                  ELSE
     C*                  MOVEL     SRDTO         SCPRD1
     C*                  ENDIF
     C*
     C* IF Proc Code Method =0 Secondary Procedure Code MUST be BLANK
     C*
     C     SCPCM         IFEQ      '0'
     C     SCPRC2        ANDNE     *BLANKS
     C                   SETON                                        906238
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C* IF Proc Code Method =0 second Procedure Code MUST be BLANK
     C
     C     SCPCM         IFEQ      '0'
     C     SCPRC2        ANDNE     *BLANKS
     C                   eval      indPRC2A2 = '1'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF
     C* IF Proc Code Method =0 third Procedure Code MUST be BLANK
     C
     C     SCPCM         IFEQ      '0'
     C     SCPRC3        ANDNE     *BLANKS
     C                   eval      indPRC3A2 = '1'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* IF Proc Code Method =0 fourth Procedure Code MUST be BLANK
     C
     C     SCPCM         IFEQ      '0'
     C     SCPRC4        ANDNE     *BLANKS
     C                   eval      indPRC4A2 = '1'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF                                                  SCPCM = *BLANKS
     C*
     C* IF Proc Code Method NE 0 THEN Secondary Code is optional
     C     SCPRC2        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '4'
     C                   MOVEL     SCPRC2        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        906338
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C     SCPRC2        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '5'
     C                   MOVEL(P)  SCPRC2        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        906338
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C     SCPRC2        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C                   MOVEL(P)  SCPRC2        KKICD9
     C                   MOVE      SCLS2         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        906338
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C* IF Proc Code Method NE 0 THEN Third Code is optional
     C     SCPRC3        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '4'
     C                   MOVEL     SCPRC3        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   eval      indPRC3A1 = '1'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C     SCPRC3        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '5'
     C                   MOVEL(P)  SCPRC3        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   eval      indPRC3A1 = '1'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C     SCPRC3        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C                   MOVEL(P)  SCPRC3        KKICD9
     C                   MOVE      SCLS3         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   eval      indPRC3A1 = '1'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C
     C
     C* IF Proc Code Method NE 0 THEN Fourth Code is optional
     C     SCPRC4        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '4'
     C                   MOVEL     SCPRC4        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *ON
     C                   eval      indPRC4A1 = '1'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C     SCPRC4        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '5'
     C                   MOVEL(P)  SCPRC4        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *ON
     C                   eval      indPRC4A1 = '1'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C
     C     SCPRC4        IFNE      *BLANKS
     C     SCPCM         ANDEQ     '9'
     C                   MOVEL(P)  SCPRC4        KKICD9
     C                   MOVE      SCLS4         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *ON
     C                   eval      indPRC4A1 = '1'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SCPRC2 NE *BLANKS
     C*
     C* Principle diag. code cannot be blank -
     C*
     C     SCDIA1        IFEQ      *BLANKS
     C                   eval      indDIA1a = '1'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C* Checking Prin. diag. code -
     C*
     C     SCDIA1        IFNE      *BLANKS
     C                   MOVEL     SCDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   eval      indDIA1 = '1'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     DIDSC1        SCDSC1
     C                   ENDIF
     C                   ENDIF
     C* Checking Admit diag. code -
     C*
     C     SCDIAD        IFNE      *BLANKS
     C                   MOVEL     SCDIAD        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        7190
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     DIDSC1        SCDSAD
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Checking 2nd diag. code -
     C*
     C     SCDIA2        IFNE      *BLANKS
     C                   MOVEL     SCDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   eval      indDIA2 = '1'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     DIDSC1        SCDSC2
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Checking 3rd diag. code -
     C*
     C     SCDIA3        IFNE      *BLANKS
     C                   MOVEL     SCDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   eval      indDIA3 = '1'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     DIDSC1        SCDSC3
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Checking 4th diag. code -
     C*
     C     SCDIA4        IFNE      *BLANKS
     C                   MOVEL     SCDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   eval      indDIA4 = '1'
     C                   SETON                                        90
     C                   GOTO      CK2END
     C                   ELSE
     C                   MOVEL     DIDSC1        SCDSC4
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C     CK2END        ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   eval      indPRC3F = '0'
     C                   eval      indPRC4F = '0'
     C                   eval      indDIA1F = '0'
     C                   eval      indDIADF = '0'
     C                   eval      indDIA2F = '0'
     C                   eval      indDIA3F = '0'
     C                   eval      indDIA4F = '0'
     C                   SELECT
     C*
     C* TYPE OF ADMISSION
     C*
     C     HFLD          WHENEQ    'SCADTY'
     C                   MOVEL(P)  'NCTA'        ##TBID
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM                    ##TBID
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     GTSTAT        ANDNE     'I'
     C                   MOVEL     GTKEY         SCADTY
     C                   MOVEL     GTDESC        SCATDS
     C                   SETON                                        9041
     C*
     C                   ELSE
     C                   SETON                                        9039
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9039
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C*
     C* PROCEDURE CODE METHOD SEARCH
     C*
     C     HFLD          WHENEQ    'SCPCM'
     C                   MOVEL     SCPCM         HLPCM
     C                   MOVEL     SCPDSC        HLDESC
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C                   PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'PCMT'        ##TBID
     C
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL     HLPCM         SCPCM
     C                   MOVEL     HLDESC        SCPDSC
     C                   SETON                                        9046
     C
     C                   ELSE
     C
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL(P)  GTKEY         SCPCM
     C                   MOVEL(P)  GTDESC        SCPDSC
     C                   SETON                                        9039      pos cursor
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C                   ENDIF                                                  ##KL='1'
     C
     C* PROCEDURE CODE SEARCHES
     C
     C     HFLD          WHENEQ    'SCPRC1'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        905039
     C                   ENDIF
     C
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9039
     C                   ENDIF                                                  SCPCM = '0'
     C
     C     SCPCM         IFEQ      '5'
     C                   MOVEL     SCPRC1        HLPRC1
     C                   MOVEL     SCP1DS        HLP1DS
     C                   MOVE      SCLS1         HLLOS1
     C                   Z-ADD     0             ##RRN
     C                   CALL      'F2310R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN             9 0
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLPRC1        SCPRC1
     C                   MOVEL(P)  HLP1DS        SCP1DS
     C                   MOVE      HLLOS1        SCLS1
     C                   SETON                                        9039
     C                   ELSE
     C     ##RRN         CHAIN     PRC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      *BLANKS       SCLS1
     C                   MOVEL(P)  RCCPT4        SCPRC1
     C                   MOVEL(P)  RCDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   SETON                                        9038
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '5'
     C
     C     SCPCM         IFEQ      '4'
     C                   MOVEL     SCPRC1        HLPRC1
     C                   MOVEL     SCP1DS        HLP1DS
     C*****                MOVE EOAST     ##ST    2
     C                   MOVE      EDITST        ##ST              2
     C                   MOVE      *BLANKS       ##CPT4           15
     C                   MOVE      *BLANKS       ##DESC           40
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CPT4
     C                   PARM                    ##DESC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   MOVEL(P)  HLPRC1        SCPRC1
     C                   MOVEL(P)  HLP1DS        SCP1DS
     C                   SETON                                        9039
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC1
     C                   MOVEL(P)  ##DESC        SCP1DS
     C                   MOVE      *BLANKS       SCLS1
     C                   SETON                                        9038
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC1        HLPRC1            7
     C                   MOVEL     SCLS1         HLLOS1
     C                   MOVEL     SCP1DS        HLP1DS           40
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      ' '           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C                   MOVEL(P)  HLPRC1        SCPRC1
     C                   MOVEL     HLLOS1        SCLS1
     C                   MOVEL(P)  HLP1DS        SCP1DS
     C                   SETON                                        9039
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC1
     C                   MOVE      ##LOST        SCLS1
     C                   MOVEL(P)  SCPRC1        KKICD9
     C                   MOVE      SCLS1         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *INPP = *OFF
     C                   SETON                                        9038
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C     HFLD          WHENEQ    'SCPRC2'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        905338
     C                   ENDIF
     C
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9038
     C                   ENDIF                                                  SCPCM = '0'
     C
     C     SCPCM         IFEQ      '5'
     C                   MOVEL     SCPRC2        HLPRC2
     C                   MOVEL     SCP2DS        HLP2DS
     C                   MOVE      SCLS2         HLLOS2
     C                   Z-ADD     0             ##RRN
     C                   CALL      'F2310R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLPRC2        SCPRC2
     C                   MOVEL(P)  HLP2DS        SCP2DS
     C                   MOVE      HLLOS2        SCLS2
     C                   SETON                                        9038
     C                   ELSE
     C     ##RRN         CHAIN     PRC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      *BLANKS       SCLS2
     C                   MOVEL(P)  RCCPT4        SCPRC2
     C                   MOVEL(P)  RCDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   eval      indPRC3F = '1'
     C                   SETON                                        90
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '5'
     C*
     C     SCPCM         IFEQ      '4'
     C                   MOVEL     SCPRC2        HLPRC2
     C                   MOVEL     SCP2DS        HLP2DS
     C                   MOVE      EDITST        ##ST              2
     C                   MOVE      *BLANKS       ##CPT4           15
     C                   MOVE      *BLANKS       ##DESC           40
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CPT4
     C                   PARM                    ##DESC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   MOVEL(P)  HLPRC2        SCPRC2
     C                   MOVEL(P)  HLP2DS        SCP2DS
     C                   SETON                                        9038
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC2
     C                   MOVEL(P)  ##DESC        SCP2DS
     C                   MOVE      *BLANKS       SCLS2
     C                   eval      indPRC3F = '1'
     C                   SETON                                        90
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC2        HLPRC2            7
     C                   MOVE      SCLS2         HLLOS2
     C                   MOVEL     SCP2DS        HLP2DS           40
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      ' '           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     ##KL          OREQ      '1'
     C                   MOVEL(P)  HLPRC2        SCPRC2
     C                   MOVEL     HLLOS2        SCLS2
     C                   MOVEL(P)  HLP2DS        SCP2DS
     C                   SETON                                        9038
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC2
     C                   MOVE      ##LOST        SCLS2
     C                   MOVEL(P)  SCPRC2        KKICD9
     C                   MOVE      SCLS2         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   eval      indPRC3F = '1'
     C                   SETON                                        90
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C     HFLD          WHENEQ    'SCRLIC'
     C                   SETON                                        9078
     C     HFLD          WHENEQ    'SCDCDT'
     C                   SETON                                        9084
     C*
     C     HFLD          WHENEQ    'SCRCVD'
     C                   SETON                                        9034
     C*
     C     HFLD          WHENEQ    'SCTSS'
     C                   Z-ADD     0             ##RRN
     C                   MOVE      SCTSS         HLTSS
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C                   PARM      'SVTZ'        ##TBID
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLTSS         SCTSS
     C                   SETON                                        9037
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCTSS
     C                   MOVEL     GTDESC        SCTSSD
     C                   SETON                                        9031
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*   SCDRG# SEARCH
     C     HFLD          WHENEQ    'SCDRG#'
     C*
     C                   CALL      'F2354R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      0             ##RRN
     C                   PARM      0             KKDRG#
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PDG01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DGDRG#        SCDRG#
     C                   MOVEL     DGDESC        SCDGDS
     C                   SETON                                        36
     C                   ENDIF                                                  *IN99=*OFF
     C                   ELSE
     C                   SETON                                        49
     C                   ENDIF                                                  #RRN<>0
     C     HFLD          WHENEQ    'SCPRD1'
     C                   SETON                                        23
     C*
     C     HFLD          WHENEQ    'SCPRC3'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   SETON                                        905338
     C                   ENDIF
     C
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9038
     C                   ENDIF                                                  SCPCM = '0'
     C
     C     SCPCM         IFEQ      '5'
     C                   MOVEL     SCPRC3        HLPRC3
     C                   MOVEL     SCP3DS        HLP3DS
     C                   MOVE      SCLS3         HLLOS3
     C                   Z-ADD     0             ##RRN
     C                   CALL      'F2310R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLPRC3        SCPRC3
     C                   MOVEL(P)  HLP3DS        SCP3DS
     C                   MOVE      HLLOS3        SCLS3
     C                   eval      indPRC3F = '1'
     C                   SETON                                        90
     C                   ELSE
     C     ##RRN         CHAIN     PRC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      *BLANKS       SCLS3
     C                   MOVEL(P)  RCCPT4        SCPRC3
     C                   MOVEL(P)  RCDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   eval      indPRC4F = '1'
     C                   SETON                                        90
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '5'
     C*
     C     SCPCM         IFEQ      '4'
     C                   MOVEL     SCPRC3        HLPRC3
     C                   MOVEL     SCP3DS        HLP3DS
     C                   MOVE      EDITST        ##ST              2
     C                   MOVE      *BLANKS       ##CPT4           15
     C                   MOVE      *BLANKS       ##DESC           40
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CPT4
     C                   PARM                    ##DESC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   MOVEL(P)  HLPRC3        SCPRC3
     C                   MOVEL(P)  HLP3DS        SCP3DS
     C                   eval      indPRC3F = '1'
     C                   SETON                                        90
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC3
     C                   MOVEL(P)  ##DESC        SCP3DS
     C                   MOVE      *BLANKS       SCLS3
     C                   eval      indPRC4F = '1'
     C                   SETON                                        90
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC3        HLPRC3            7
     C                   MOVE      SCLS3         HLLOS3
     C                   MOVEL     SCP3DS        HLP3DS           40
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      ' '           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     ##KL          OREQ      '1'
     C                   MOVEL(P)  HLPRC3        SCPRC3
     C                   MOVEL     HLLOS3        SCLS3
     C                   MOVEL(P)  HLP3DS        SCP3DS
     C                   eval      indPRC3F = '1'
     C                   SETON                                        90
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC3
     C                   MOVE      ##LOST        SCLS3
     C                   MOVEL(P)  SCPRC3        KKICD9
     C                   MOVE      SCLS3         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   eval      indPRC4F = '1'
     C                   SETON                                        90
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C                   ENDIF                                                  SCPCM = '9'
     C* Search for proc code-4.
     C     HFLD          WHENEQ    'SCPRC4'
     C*
     C* PROCEDURE CODE MUST BE ENTERED TO DO THIS SEARCH
     C*
     C     SCPCM         IFEQ      *BLANKS
     C                   eval      indPRC4F = '1'
     C                   SETON                                        9053
     C                   ENDIF
     C
     C     SCPCM         IFEQ      '0'
     C                   SETON                                        9024
     C                   ENDIF                                                  SCPCM = '0'
     C
     C     SCPCM         IFEQ      '5'
     C                   MOVEL     SCPRC4        HLPRC4
     C                   MOVEL     SCP4DS        HLP4DS
     C                   MOVE      SCLS4         HLLOS4
     C                   Z-ADD     0             ##RRN
     C                   CALL      'F2310R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C*
     C     ##KL          IFEQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVEL(P)  HLPRC4        SCPRC4
     C                   MOVEL(P)  HLP4DS        SCP4DS
     C                   MOVE      HLLOS4        SCLS4
     C                   eval      indPRC4F = '1'
     C                   SETON                                        90
     C                   ELSE
     C     ##RRN         CHAIN     PRC01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      *BLANKS       SCLS4
     C                   MOVEL(P)  RCCPT4        SCPRC4
     C                   MOVEL(P)  RCDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   eval      indDIA1F = '1'
     C                   SETON                                        90
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '5'
     C
     C     SCPCM         IFEQ      '4'
     C                   MOVEL     SCPRC4        HLPRC4
     C                   MOVEL     SCP4DS        HLP4DS
     C                   MOVE      EDITST        ##ST              2
     C                   MOVE      *BLANKS       ##CPT4           15
     C                   MOVE      *BLANKS       ##DESC           40
     C                   CALL      'F2305R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ST
     C                   PARM                    ##CPT4
     C                   PARM                    ##DESC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CPT4        OREQ      *BLANKS
     C                   MOVEL(P)  HLPRC4        SCPRC4
     C                   MOVEL(P)  HLP4DS        SCP4DS
     C                   eval      indPRC4F = '1'
     C                   SETON                                        90
     C                   ELSE
     C                   MOVEL(P)  ##CPT4        SCPRC4
     C                   MOVEL(P)  ##DESC        SCP4DS
     C                   MOVE      *BLANKS       SCLS4
     C                   eval      indDIA1F = '1'
     C                   SETON                                        90
     C                   ENDIF                                                  ##KL = '1'
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '9'
     C                   MOVEL     SCPRC4        HLPRC4            7
     C                   MOVE      SCLS4         HLLOS4
     C                   MOVEL     SCP4DS        HLP4DS           40
     C                   MOVE      *BLANKS       ##ICD9
     C                   CALL      'F2010R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##ICD9
     C                   PARM      ' '           ##LOST
     C*
     C     ##ICD9        IFEQ      *BLANKS
     C     ##KL          OREQ      '1'
     C                   MOVEL(P)  HLPRC4        SCPRC4
     C                   MOVEL     HLLOS4        SCLS4
     C                   MOVEL(P)  HLP4DS        SCP4DS
     C                   eval      indDIA1F = '1'
     C                   SETON                                        90
     C                   ELSE
     C                   MOVEL(P)  ##ICD9        SCPRC4
     C                   MOVE      ##LOST        SCLS4
     C                   MOVEL(P)  SCPRC4        KKICD9
     C                   MOVE      SCLS4         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   eval      indDIA1F = '1'
     C                   SETON                                        90
     C                   ENDIF                                                  ##ICD9 = *BLANKS
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C* Searching Prin. diagnosis code -
     C*
     C     HFLD          WHENEQ    'SCDIA1'
     C                   MOVEL     SCDIA1        HLDIA1
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C     *INKL         IFEQ      *OFF
     C     XXICD9        ANDNE     *BLANKS
     C                   MOVEL     XXICD9        SCDIA1
     C*                  MOVEL     SCDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC1
     C                   eval      indDIADF = '1'
     C                   SETON                                        90
     C                   ELSE
     C                   MOVEL     HLDIA1        SCDIA1
     C                   MOVEL     SCDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC1
     C                   eval      indDIA1F = '1'
     C                   SETON                                        90
     C                   ENDIF
     C*
     C* Searching admit diagnosis code -
     C*
     C     HFLD          WHENEQ    'SCDIAD'
     C                   MOVEL     SCDIAD        HLDIA1
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C     *INKL         IFEQ      *OFF
     C     XXICD9        ANDNE     *BLANKS
     C                   MOVEL     XXICD9        SCDIAD
     C                   MOVEL     SCDIAD        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSAD
     C                   eval      indDia2F = '1'
     C                   SETON                                        90
     C                   ELSE
     C                   MOVEL     HLDIA1        SCDIAD
     C                   MOVEL     SCDIAD        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSAD
     C                   eval      indDIADF = '1'
     C                   SETON                                        90
     C                   ENDIF
     C*
     C* Searching diagnosis field 2 -
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C                   MOVEL     SCDIA2        HLDIA1
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C     *INKL         IFEQ      *OFF
     C     XXICD9        ANDNE     *BLANKS
     C                   MOVEL     XXICD9        SCDIA2
     C                   MOVEL     SCDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC2        SCDSC2
     C                   eval      indDIA3F = '1'
     C                   SETON                                        90
     C                   ELSE
     C                   MOVEL     HLDIA1        SCDIA2
     C                   MOVEL     SCDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC2        SCDSC2
     C                   eval      indDIA2F = '1'
     C                   SETON                                        90
     C                   ENDIF
     C*
     C* Searching diagnosis field 3 -
     C*
     C     HFLD          WHENEQ    'SCDIA3'
     C                   MOVEL     SCDIA3        HLDIA1
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C     *INKL         IFEQ      *OFF
     C     XXICD9        ANDNE     *BLANKS
     C                   MOVEL     XXICD9        SCDIA3
     C                   MOVEL     SCDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC3
     C                   eval      indDIA4F = '1'
     C                   SETON                                        90
     C                   ELSE
     C                   MOVEL     HLDIA1        SCDIA3
     C                   MOVEL     SCDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC3
     C                   eval      indDIA3F = '1'
     C                   SETON                                        90
     C                   ENDIF
     C*
     C* Searching diagnosis field 4 -
     C*
     C     HFLD          WHENEQ    'SCDIA4'
     C                   MOVEL     SCDIA4        HLDIA1
     C                   MOVE      *BLANKS       XXICD9
     C                   MOVE      *BLANKS       XXLOST
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C     *INKL         IFEQ      *OFF
     C     XXICD9        ANDNE     *BLANKS
     C                   MOVEL     XXICD9        SCDIA4
     C                   MOVEL     SCDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC4
     C                   SETON                                        90
     C                   ELSE
     C                   MOVEL     HLDIA1        SCDIA4
     C                   MOVEL     SCDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC4
     C                   eval      indDIA4F = '1'
     C                   SETON                                        90
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCACCT'
     C                   SETON                                        52
     C     HFLD          WHENEQ    'SCRFEI'
     C                   SETON                                        55
     C*
     C                   ENDSL
     C                   SETON                                        90
     C*
     C*
     C*
     C*
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KKDO - F11=EOMB/Notes
     C*================================================================
     C     KKDO          BEGSR
     C*
     C                   CALL      'F0355R'
     C                   PARM      *BLANKS       ##CALL
     C                   PARM      *BLANKS       ##KC
     C                   PARM      *BLANKS       ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      @@RRN2        ##RRN2            9 0
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   SCRLSN
     C                   CLEAR                   SCRFSN
     C                   CLEAR                   SCRFEI
     C*
     C                   MOVEL     EUCO#         KKCO#
     C                   MOVEL     EUSUB#        KKSUB#
     C                   MOVEL     EUFND         KKFND
     C                   MOVEL     EUFYR         KKFYR
     C                   MOVEL     EUCASE        KKCASE
     C*
     C                   MOVEL(P)  EUSUB#        XXSUB#            5
     C                   MOVEL(P)  EUFND         XXFND             3
     C                   MOVEL(P)  EUFYR         XXFYR             3
     C                   MOVEL(P)  EUCASE        XXCASE            7

     C*
     C     CAKY          CHAIN     PCA01                              60
     C                   MOVEL     CASS#         KKSS#
     C*
     C     CDKY          CHAIN     PCD01                              60
     C*
     C                   MOVE      EOEOB#        SCEOMB
     C                   MOVE      EOCNTL        SCCNTL
     C*
     C                   MOVEL     EUDCN         SCTSS
     C     SCTSS         IFEQ      'F'
     C                   MOVEL     'FINAL'       SCTSSD
     C                   ENDIF
     C     SCTSS         IFEQ      'I'
     C                   MOVEL     'INTRIM'      SCTSSD
     C                   ENDIF
     C*
     C***** Check database field and move into screen diag code**
     C*
     C     EUDIA1        IFNE      *BLANKS
     C                   MOVEL     EUDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DIDSC1        SCDSC1
     C                   MOVEL     EUDIA1        SCDIA1
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC1
     C                   MOVE      *BLANKS       SCDIA1
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C     EUADDI        IFNE      *BLANKS
     C                   MOVEL     EUADDI        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DIDSC1        SCDSAD
     C                   MOVEL     EUADDI        SCDIAD
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSAD
     C                   MOVE      *BLANKS       SCDIAD
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C     EUDIA2        IFNE      *BLANKS
     C                   MOVEL     EUDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DIDSC1        SCDSC2
     C                   MOVEL     EUDIA2        SCDIA2
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC2
     C                   MOVE      *BLANKS       SCDIA2
     C                   ENDIF
     C                   ENDIF
     C*
     C     EUDIA3        IFNE      *BLANKS
     C                   MOVEL     EUDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DIDSC1        SCDSC3
     C                   MOVEL     EUDIA3        SCDIA3
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC3
     C                   MOVE      *BLANKS       SCDIA3
     C                   ENDIF
     C                   ENDIF
     C*
     C     EUDIA4        IFNE      *BLANKS
     C                   MOVEL     EUDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     DIDSC1        SCDSC4
     C                   MOVEL     EUDIA4        SCDIA4
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC4
     C                   MOVE      *BLANKS       SCDIA4
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Get Proc Code Method, Princ Proc, & Other Proc
     C*
     C                   MOVEL     EUPCM         SCPCM
     C                   MOVE      'PCMT'        KKTBID
     C                   MOVEL(P)  SCPCM         KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCPDSC
     C                   ENDIF                                                  *IN99 = *OFF
     C
     C                   MOVEL     EUPRC1        SCPRC1
     C                   MOVE      EUPRC1        SCLS1
     C     SCPCM         IFEQ      '4'
     C     SCPRC1        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC1        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC1        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC1        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C     SCPCM         IFEQ      '9'
     C     SCPRC1        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC1        KKICD9
     C                   MOVE      SCLS1         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP1DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C                   MOVEL     EUPRC2        SCPRC2
     C                   MOVE      EUPRC2        SCLS2
     C     SCPCM         IFEQ      '4'
     C     SCPRC2        ANDNE     *BLANKS
     C                   MOVEL     SCPRC2        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC2        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC2        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C     SCPCM         IFEQ      '9'
     C     SCPRC2        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC2        KKICD9
     C                   MOVE      SCLS2         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP2DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '9'
     C                   MOVEL     EURLIC        SCRLIC
     C                   MOVEL     EURLIC        XOFLIC
     C                   MOVEL     EUDGDS        SCDGDS
     C*
     C                   SETOFF                                       48
     C     EUICN         IFEQ      'IR'
     C     EUICN         OREQ      'IP'
     C     EUICN         OREQ      'IM'
     C     EUICN         OREQ      'IS'
     C                   SETON                                        48
     C                   ENDIF                                                  EUICN
     C*
     C                   Z-ADD     EUDRG#        SCDRG#
     C                   Z-ADD     EUDRG#        XODRG#
     C     EUDRG#        CHAIN     PDG02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DGDESC        SCDGDS
     C                   ENDIF
     C                   Z-ADD     EUDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDCDT
     C                   MOVEL     SRDTO         XODCDT
     C                   MOVE      EURLSN        SCRLSN
     C                   MOVE      EURFSN        SCRFSN
     C                   MOVE      EURFEI        SCRFEI
     C***                Z-ADD     EUPAID        SCPAID
     C*
     C                   Z-ADD     EUPRD1        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPRD1
     C*
     C                   MOVEL     EUPRC3        SCPRC3
     C                   MOVE      EUPRC3        SCLS3
     C     SCPCM         IFEQ      '4'
     C     SCPRC3        ANDNE     *BLANKS
     C                   MOVEL     SCPRC3        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC3        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC3        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C     SCPCM         IFEQ      '9'
     C     SCPRC3        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC3        KKICD9
     C                   MOVE      SCLS3         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP3DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '9'
     C*
     C*
     C                   MOVEL     EUPRC4        SCPRC4
     C                   MOVE      EUPRC4        SCLS4
     C     SCPCM         IFEQ      '4'
     C     SCPRC4        ANDNE     *BLANKS
     C                   MOVEL     SCPRC4        KKCPT4
     C                   MOVE      *HIVAL        KKEFDT
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PRDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '4'
     C*
     C     SCPCM         IFEQ      '5'
     C     SCPRC4        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC4        KKCPT4
     C     RCKY          CHAIN     PRC02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = 5
     C*
     C     SCPCM         IFEQ      '9'
     C     SCPRC4        ANDNE     *BLANKS
     C                   MOVEL(P)  SCPRC4        KKICD9
     C                   MOVE      SCLS4         KKLOST
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  DIDSC1        SCP4DS
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  SCPCM = '9'
     C                   MOVE      EUADTY        SCADTY
     C*
     C                   Z-ADD     EUADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCADDT
     C*
     C                   MOVE      EUADHR        SCADHR
     C                   Z-ADD     EUDCHR        SCDCHR
     C                   MOVEL     EUACCT        SCACCT
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCDIA1        EUDIA1
     C                   MOVEL     SCDIAD        EUADDI
     C                   MOVEL     SCDIA2        EUDIA2
     C                   MOVEL     SCDIA3        EUDIA3
     C                   MOVEL     SCDIA4        EUDIA4
     C                   MOVEL     SCPCM         EUPCM
     C                   MOVEL     SCPRC1        EUPRC1
     C                   MOVE      SCLS1         EUPRC1
     C                   MOVEL     SCPRC2        EUPRC2
     C                   MOVE      SCLS2         EUPRC2
     C                   MOVEL     SCPRC3        EUPRC3
     C                   MOVE      SCLS3         EUPRC3
     C                   MOVEL     SCPRC4        EUPRC4
     C                   MOVE      SCLS4         EUPRC4
     C                   MOVEL     SCTSS         EUDCN
     C                   MOVEL     SCRLIC        EURLIC
     C                   MOVEL     SCRLIC        EORLIC
     C                   MOVEL(P)  SCDRG#        EUDRG#
     C     SCDRG#        CHAIN     PDG02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     DGDESC        EUDGDS
     C                   ENDIF
     C                   Z-ADD     SCDCDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUDCDT
     C*
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      Q1PGM         EAPGM
     C                   MOVE      DSTIME        EATIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   UPDATE    PEA01
     C*
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   MOVE      Q1USER        EUUSER
     C                   MOVE      Q1PGM         EUPGM
     C                   MOVE      DSTIME        EUTIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUDT
     C*
     C                   MOVE      Q1USER        EOUSER
     C                   MOVE      Q1PGM         EOPGM
     C                   MOVE      DSTIME        EOTIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   MOVE      DSTIME        EATIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   MOVE      SCRLSN        EURLSN
     C                   MOVE      SCRFSN        EURFSN
     C                   MOVE      SCRFEI        EURFEI
     C*
     C                   Z-ADD     SCPRD1        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUPRD1
     C                   MOVE      SCADTY        EUADTY
     C*
     C                   Z-ADD     SCADDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EUADDT
     C*
     C                   MOVE      SCADHR        EUADHR
     C                   Z-ADD     SCDCHR        EUDCHR
     C                   MOVEL(P)  SCACCT        EUACCT
     C*
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
     C     *INKJ         CASEQ     *ON           KJDO
     C     *INKK         CASEQ     *ON           KKDO
     C                   ENDCS
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
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C*
     C*
     C*  SET EDITST TO POLICY OR ADJ STATE BASED ON CPST FLAG
     C*
     C     EOCPST        IFEQ      'C'
     C                   MOVEL(P)  EOPOST        EDITST
     C                   ELSE
     C                   MOVEL(P)  EOAST         EDITST
     C                   ENDIF
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EORCN         KKRCN
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C                   CALL      'FA707R'
     C                   PARM                    ##RTCD
     C                   PARM                    @@RRN2
     C                   PARM                    #1L1
     C                   MOVEL     #1L1          SCHD3
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
     C*
     C     @@RRN2        CHAIN     PEO01                              60
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C                   UPDATE    PEO01
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   EXSR      SRWREP
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FAEUP
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   EXSR      SRWREP
     C                   ENDSR
     C*================================================================
     C* SRWREP - Write header note record
     C*================================================================
     C     SRWREP        BEGSR
     C* IF EITHER OF THE FIELDS IS CHANGED THEN CALL API
     C     EUFLIC        IFNE      XOFLIC
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL1const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XOFLIC        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  EUFLIC
     C* DRUG CODE IS CHANGED
     C     EUDRG#        IFNE      XODRG#
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL2const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XODRG#        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  EUDRG#
     C* DISCHARGE DATE IS CHANGED
     C     EUDCDT        IFNE      XODCDT
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL3const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XODCDT        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  EUDCDT
     C                   ENDSR
