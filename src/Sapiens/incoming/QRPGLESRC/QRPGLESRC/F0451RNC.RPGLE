     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*  Program: F0451RNC                                            *
     H*  Description: Work With EOMBs Header Info update/display      *
     H*               North Carolina VERSION/PHYSICIAN BILLS          *
     H*  Created: 02/26/14 C:296 Project: NC034                       *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H*                                                               *
     H*****************************************************************
     FF0451FNC  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAEHP     UF A E             DISK
     F                                     RENAME(PEH01:DBRCDF)
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     FFAEAL1    IF   E           K DISK
     F*
     FFMCDL     IF   E           K DISK
     FFDCALF    IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FFAESL4    UF   E           K DISK
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Physician (HCFA) -
     D                                     Header Information')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D CHGNUM          C                   CONST('SA020R')
     D NT0             C                   CONST('Field was changed fo-
     D                                     r EDI correction:')
     D NT1             C                   CONST('Original Value:')
     D NT2             C                   CONST('Billing provider sta-
     D                                     te lic#')
     D NT3             C                   CONST('Rendering provider N-
     D                                     PI# ')
     D                 DS
     D  XXNOT1                 1     70
     D  XXNT1                  1     38
     D  XXNT2                 39     70
     D                 DS
     D  XXNOT2                 1     70
     D  XXNT21                 1     16
     D  XXNT22                17     70

     I*** END ADD ****** 04/30/08 ************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          FOR FAEHP
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
     C*  Change number parameter list
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
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
     C                   KFLD                    EHCO#
     C                   KFLD                    EHSUB#
     C                   KFLD                    EHFND
     C                   KFLD                    EHFYR
     C                   KFLD                    EHCASE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    XXDIA
     C                   KFLD                    XXLOST
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
     C*  KEY LIST FOR EOMB CHANGE STATUS TRANSACTION FILE
     C*
     C     ESKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKBLST
     C                   KFLD                    KKRCN
     C*
     C*  KEY LIST FOR EOMB CHANGE STATUS TRANSACTION FILE
     C*
     C     ESKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKBLST
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
     C     *LIKE         DEFINE    EHDIA1        XXDIA
     C     *LIKE         DEFINE    EHDIA1        XXDIA1
     C     *LIKE         DEFINE    DILOST        XXLOST
     C     *LIKE         DEFINE    DIICD9        XXICD9
     C     *LIKE         DEFINE    CAFDAT        HLRCVD
     C     *LIKE         DEFINE    CAACDT        HLACDT
     C     *LIKE         DEFINE    CAACDT        HLSYDT
     C     *LIKE         DEFINE    ESRCN         KKRCN
     C     *LIKE         DEFINE    ESBLST        KKBLST
     C     *LIKE         DEFINE    EHDIA1        HLDIA
     C     *LIKE         DEFINE    DILOST        HLLOS
     C     *LIKE         DEFINE    DIDSC1        HLDSC
     C     *LIKE         DEFINE    DIDS1         HLDS
     C     *LIKE         DEFINE    EOAST         EDITST
     C     *LIKE         DEFINE    EHFLIC        XOFLIC
     C     *LIKE         DEFINE    EHRNPI        XORNPI
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
     C*  Make sure 1st Diag Code is populated & make sure that it is
     C*    a valid code; if 2nd, 3rd, & 4th Diag Codes are filled,
     C*    make sure they are valid.
     C*
     C     SCDIA1        IFEQ      *BLANKS
     C                   SETON                                        7090
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     SCDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        7190
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS1
     C                   MOVEL     DIDSC1        SCDSC1
     C                   MOVEL     DIDS1         SCDS1
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIA2        IFNE      *BLANKS
     C                   MOVEL     SCDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        7790
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS2
     C                   MOVEL     DIDSC1        SCDSC2
     C                   MOVEL     DIDS1         SCDS2
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIA3        IFNE      *BLANKS
     C                   MOVEL     SCDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        7890
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS3
     C                   MOVEL     DIDSC1        SCDSC3
     C                   MOVEL     DIDS1         SCDS3
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCDIA4        IFNE      *BLANKS
     C                   MOVEL     SCDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      *ON
     C     DISTAT        OREQ      'I'
     C                   SETON                                        7990
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL     DILOST        SCLOS4
     C                   MOVEL     DIDSC1        SCDSC4
     C                   MOVEL     DIDS1         SCDS4
     C                   ENDIF
     C                   ENDIF
     C*
     C*    SCFLIC        IFEQ      *BLANKS
     C*                  SETON                                        7290
     C*                  GOTO      CK1END
     C*                  ENDIF
     C*    SCRNLC        IFEQ      *BLANKS
     C*                  SETON                                        6990
     C*                  GOTO      CK1END
     C*                  ENDIF
     C*  Make sure that Data Subm flag is either "Y" or "N" & not
     C*    blank
     C*
     C*    SCTAPE        IFEQ      *BLANKS
     C*                  SETON                                        7590
     C*                  GOTO      CK1END
     C*                  ELSE
     C*    SCTAPE        IFEQ      'Y'
     C*    SCTAPE        OREQ      'N'
     C*    SCTAPE        OREQ      'R'
     C*                  SETOFF                                       76
     C*                  ELSE
     C*                  SETON                                        7690
     C*                  GOTO      CK1END
     C*                  ENDIF
     C*                  ENDIF
     C*
     C** Provider is required when last name is present.
      /Free
         if SCRLSN <> ' ' and SCRFEI = 0 ;
         *in90 = '1' ;
         *in25 = '1' ;
      /End-Free
     C                   GOTO      CK1END
      /Free
         endif;
      /End-Free
     C*
     C** Patients account number cannot be blank.
      /Free
         if SCACCT = ' ' ;
         *in27 = '1' ;
      /End-Free
     C                   GOTO      CK1END
      /Free
         endif ;
      /End-Free
     C     CK1END        ENDSR
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
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SETOFF                                       222324
     C                   SETOFF                                       26
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCDIA1'
     C                   MOVE      *BLANKS       XXICD9
     C**********           MOVE *BLANKS   XXLOST
     C                   MOVEL     SCDIA1        HLDIA
     C                   MOVEL     SCLOS1        HLLOS
     C                   MOVEL     SCDSC1        HLDSC
     C                   MOVEL     SCDS1         HLDS
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C                   SETON                                        90
     C     *INKL         IFEQ      *OFF
     C     XXICD9        ANDNE     *BLANKS
     C                   MOVEL     XXICD9        SCDIA1
     C                   MOVEL     XXLOST        SCLOS1
     C                   MOVEL     SCDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC1
     C  N60              MOVEL     DIDS1         SCDS1
     C                   SETON                                        36
     C                   ELSE
     C                   SETON                                        35
     C                   MOVEL     HLDIA         SCDIA1
     C                   MOVEL     HLLOS         SCLOS1
     C                   MOVEL     HLDSC         SCDSC1
     C                   MOVEL     HLDS          SCDS1
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIA2'
     C                   MOVE      *BLANKS       XXICD9
     C**********           MOVE *BLANKS   XXLOST
     C                   MOVEL     SCDIA2        HLDIA
     C                   MOVEL     SCLOS2        HLLOS
     C                   MOVEL     SCDSC2        HLDSC
     C                   MOVEL     SCDS2         HLDS
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C                   SETON                                        90
     C     *INKL         IFEQ      *OFF
     C     XXICD9        ANDNE     *BLANKS
     C                   MOVEL     XXICD9        SCDIA2
     C                   MOVEL     XXLOST        SCLOS2
     C                   MOVEL     SCDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC2
     C  N60              MOVEL     DIDS1         SCDS2
     C                   SETON                                        37
     C                   ELSE
     C                   SETON                                        36
     C                   MOVEL     HLDIA         SCDIA2
     C                   MOVEL     HLLOS         SCLOS2
     C                   MOVEL     HLDSC         SCDSC2
     C                   MOVEL     HLDS          SCDS2
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIA3'
     C                   MOVE      *BLANKS       XXICD9
     C**********           MOVE *BLANKS   XXLOST
     C                   MOVEL     SCDIA3        HLDIA
     C                   MOVEL     SCLOS3        HLLOS
     C                   MOVEL     SCDSC3        HLDSC
     C                   MOVEL     SCDS3         HLDS
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C                   SETON                                        90
     C     *INKL         IFEQ      *OFF
     C     XXICD9        ANDNE     *BLANKS
     C                   MOVEL     XXICD9        SCDIA3
     C                   MOVEL     XXLOST        SCLOS3
     C                   MOVEL     SCDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC3
     C  N60              MOVEL     DIDS1         SCDS3
     C                   SETON                                        38
     C                   ELSE
     C                   SETON                                        37
     C                   MOVEL     HLDIA         SCDIA3
     C                   MOVEL     HLLOS         SCLOS3
     C                   MOVEL     HLDSC         SCDSC3
     C                   MOVEL     HLDS          SCDS3
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCDIA4'
     C                   MOVE      *BLANKS       XXICD9
     C**********           MOVE *BLANKS   XXLOST
     C                   MOVEL     SCDIA4        HLDIA
     C                   MOVEL     SCLOS4        HLLOS
     C                   MOVEL     SCDSC4        HLDSC
     C                   MOVEL     SCDS4         HLDS
     C                   CALL      'F2010R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C*
     C                   SETON                                        90
     C     *INKL         IFEQ      *OFF
     C     XXICD9        ANDNE     *BLANKS
     C                   MOVEL     XXICD9        SCDIA4
     C                   MOVEL     XXLOST        SCLOS4
     C                   MOVEL     SCDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C  N60              MOVEL     DIDSC1        SCDSC4
     C  N60              MOVEL     DIDS1         SCDS4
     C                   SETON                                        39
     C                   ELSE
     C                   SETON                                        38
     C                   MOVEL     HLDIA         SCDIA4
     C                   MOVEL     HLLOS         SCLOS4
     C                   MOVEL     HLDSC         SCDSC4
     C                   MOVEL     HLDS          SCDS4
     C                   ENDIF
     C*
     C*    HFLD          WHENEQ    'SCTAPE'
     C*                  SETON                                        9068
     C*                  SETOFF                                       6967
     C*    HFLD          WHENEQ    'SCFLIC'
     C*                  SETON                                        9073
     C     HFLD          WHENEQ    'SCRNPI'
     C                   SETON                                        9074
     C*    HFLD          WHENEQ    'SCRNLC'
     C*                  SETON                                        9067
     C     HFLD          WHENEQ    'SCRLSN'
     C                   SETON                                        9022
     C     HFLD          WHENEQ    'SCRFSN'
     C                   SETON                                        9023
     C     HFLD          WHENEQ    'SCRFEI'
     C                   SETON                                        9024
     C     HFLD          WHENEQ    'SCACCT'
     C                   SETON                                        9026
     C*
     C                   ENDSL
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
     C                   CLEAR                   SCACCT
     C*
     C                   MOVEL     EHCO#         KKCO#
     C                   MOVEL     EHSUB#        KKSUB#
     C                   MOVEL     EHFND         KKFND
     C                   MOVEL     EHFYR         KKFYR
     C                   MOVEL     EHCASE        KKCASE
     C*
     C                   MOVEL(P)  EHSUB#        XXSUB#            5
     C                   MOVEL(P)  EHFND         XXFND             3
     C                   MOVEL(P)  EHFYR         XXFYR             3
     C                   MOVEL(P)  EHCASE        XXCASE            7
     C*
     C     CAKY          CHAIN     PCA01                              60
     C                   MOVEL     CASS#         KKSS#
     C*
     C     CDKY          CHAIN     PCD01                              60
     C*
     C                   MOVEL     EOEOB#        SCEOMB
     C                   MOVE      EOCNTL        SCCNTL
     C*
     C*                  MOVE      EHEDI         SCTAPE
     C*
     C     EHDIA1        IFNE      *BLANKS
     C                   MOVEL     EHDIA1        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC1
     C                   MOVEL     DILOST        SCLOS1
     C                   MOVEL     DIDS1         SCDS1
     C                   MOVEL     EHDIA1        SCDIA1
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC1
     C                   MOVE      *BLANKS       SCLOS1
     C                   MOVE      *BLANKS       SCDS1
     C                   MOVE      *BLANKS       SCDIA1
     C                   ENDIF
     C*
     C     EHDIA2        IFNE      *BLANKS
     C                   MOVEL     EHDIA2        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC2
     C                   MOVEL     DILOST        SCLOS2
     C                   MOVEL     DIDS1         SCDS2
     C                   MOVEL     EHDIA2        SCDIA2
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC2
     C                   MOVE      *BLANKS       SCLOS2
     C                   MOVE      *BLANKS       SCDS2
     C                   MOVE      *BLANKS       SCDIA2
     C                   ENDIF
     C*
     C     EHDIA3        IFNE      *BLANKS
     C                   MOVEL     EHDIA3        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC3
     C                   MOVEL     DILOST        SCLOS3
     C                   MOVEL     DIDS1         SCDS3
     C                   MOVEL     EHDIA3        SCDIA3
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC3
     C                   MOVE      *BLANKS       SCLOS3
     C                   MOVE      *BLANKS       SCDS3
     C                   MOVE      *BLANKS       SCDIA3
     C                   ENDIF
     C*
     C     EHDIA4        IFNE      *BLANKS
     C                   MOVEL     EHDIA4        XXDIA
     C     DIKEY         CHAIN     PDI01                              60
     C                   MOVEL     DIDSC1        SCDSC4
     C                   MOVEL     DILOST        SCLOS4
     C                   MOVEL     DIDS1         SCDS4
     C                   MOVEL     EHDIA4        SCDIA4
     C                   ELSE
     C                   MOVE      *BLANKS       SCDSC4
     C                   MOVE      *BLANKS       SCLOS4
     C                   MOVE      *BLANKS       SCDS4
     C                   MOVE      *BLANKS       SCDIA4
     C                   ENDIF
     C*                  MOVEL     EHFLIC        SCFLIC
     C                   MOVEL     EHRNPI        SCRNPI
     C                   MOVEL     EHFLIC        XOFLIC
     C                   MOVEL     EHRNPI        XORNPI
     C*                  MOVEL     EHRNLC        SCRNLC
     C*                  MOVEL     EHRFLC        SCRFLC
     C                   MOVE      EHRLSN        SCRLSN
     C                   MOVE      EHRFSN        SCRFSN
     C                   MOVE      EHRFEI        SCRFEI
     C*                  MOVE      EHRFEI        SCRFE2
     C                   MOVE      EHACCT        SCACCT
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCDIA1        EHDIA1
     C                   MOVEL     SCDIA2        EHDIA2
     C                   MOVEL     SCDIA3        EHDIA3
     C                   MOVEL     SCDIA4        EHDIA4
     C*
     C*                  MOVEL     SCFLIC        EHFLIC
     C                   MOVEL     SCRNPI        EHRNPI
     C*                  MOVEL     SCRNLC        EHRNLC
     C*                  MOVEL     SCRFLC        EHRFLC
     C*                  MOVEL     SCTAPE        EHEDI
     C*                  MOVEL     SCTAPE        EOTAPE
     C     EAKEY         CHAIN     PEA01                              60
     C*                  MOVEL     SCTAPE        EAEDI
     C*
     C                   MOVE      Q1USER        EHUSER
     C                   MOVE      Q1PGM         EHPGM
     C                   MOVE      DSTIME        EHTIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EHDT
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
     C                   MOVE      SCRLSN        EHRLSN
     C                   MOVE      SCRFSN        EHRFSN
     C                   MOVE      SCRFEI        EHRFEI
     C*                  MOVE      SCRFE2        EHRFEI
     C                   MOVE      SCACCT        EHACCT
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
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C*
     C                   MOVE      EOCO#         KKCO#
     C                   MOVE      EOSUB#        KKSUB#
     C                   MOVE      EOFND         KKFND
     C                   MOVE      EOFYR         KKFYR
     C                   MOVE      EOCASE        KKCASE
     C                   MOVE      EOEOB#        KKEOB#
     C                   MOVE      EORCN         KKRCN
     C*
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
     C                   MOVEL(P)  'D'           XXLOST
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
     C     ESKY          SETGT     PES01
     C     ESKY1         READPE    PES01                                  59
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C                   UPDATE    PEO01
     C  N59              UPDATE    PES01
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
     C                   FEOD      FAEHP
     C                   EXSR      SRWREP
     C                   ENDSR
     C*
     C*================================================================
     C* SRWREP - Write header note record
     C*================================================================
     C     SRWREP        BEGSR
     C* IF EITHER OF THE FIELDS IS CHANGED THEN CALL API
     C     EHFLIC        IFNE      XOFLIC
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  NT2           XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XOFLIC        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  EHFLIC
     C*
     C     EHRNPI        IFNE      XORNPI
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  NT3           XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XORNPI        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'H'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  EHRNPI
     C                   ENDSR
