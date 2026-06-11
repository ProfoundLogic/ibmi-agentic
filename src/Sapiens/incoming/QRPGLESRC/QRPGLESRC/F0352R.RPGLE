     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE F0352R - PHYSICIAN (HFCA) HEADER SUMMARY
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**                                                             **
     H**   PROGRAMMER: C:62                                          **
     H**   CREATED:   07/07/97      PROJECT: MC152                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  06/10/19 C:180 Support: 80081
     H*                 Remove Visibility property for all fields except
     H*                 buttons.
     H*
     H*  05/20/19 C:180 Project: MC427
     H*                 Modernize header screen.
     H*                 Change loading of diagnosis codes: Use ICdsc2 if
     H*                 populated.
     H*
     H*  09/05/17 C:180 Project: 114MHV
     H*                 Add CVT claim number.
     H*
     H*  04/04/17 C:212 Project: MC409
     H*                 Change Invoice to Vendor Bill in DSPF and processing
     H*                 Make it visible for base
     H*
     H*  09/15/15 C:079 Support: 090/37262
     H*                 Expand diag code field to 7A
     H*
     H*  05/03/10 C:220 PROJECT: 133MIW
     H*                 ADD NEW FEILD FOR LUBA-(INVOICE NUMBER)
     H*
     H*  09/22/08 C:189 PROJECT: 48IXA
     H*                 ASSIGN DOC ID/MEDICAL BILLS WHEN SCANNED
     H*
     H*
     H*  04/11/07 C:188 PROJECT: FL061
     H*                 ADDED SCPOST/SCPSNM FIELDS
     H*
     H*  09/20/02 C:79 PROJECT:NY006
     H*                ADD SCRGN# TO DISPLAY REGION/LOCALITY
     H*
     H*  06/08/01 C:62 PROJECT:FSMNT
     H*                COMPILE ONLY
     H*
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      16      PreCert installed                              **
     H**      25      Uses Locality/Region                           **
     H*****************************************************************
     H*
     FF0352F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFAEOP     UF   E             DISK
     F                                     RENAME(PEO01:DBRCDF)
     FFAEHL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMDIL     IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFTABL     IF   E           K DISK
     FSTABL     IF   E           K DISK
     FFTGTL     IF   E           K DISK
     FUMPCL6    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F*** Beg Add *** 09/15/15 ****************************************
     FFMICD10L1 IF   E           K DISK
     F*** End Add *** 09/15/15 ****************************************
     F*** Beg Add *** 09/05/17 ****************************************
     FWMCML1    IF   E           K DISK    USROPN RENAME(PCM01:PCM01X)Prefix(x)
     FFMCXL     IF   E           K DISK
     F*** End Add *** 09/05/17 ****************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D                 DS
     D  XXCNTY                 1      6
     D  P1ST                   1      2
     D  SCCNTY                 3      6
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Physician (HCFA) Hea-
     D                                     der Summary')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          for FAEOP
     C*
     C*  Company file/fund (group) retrieve program parameter list
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
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     DIKY          KLIST
     C                   KFLD                    KKICD9
     C                   KFLD                    KKLOST
     C*
     C     EHKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*
     C     P1KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     PCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DIICD9        KKICD9
     C     *LIKE         DEFINE    DILOST        KKLOST
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    SPKEY         KKKEY
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EOCERT        KKCERT
     C     *LIKE         DEFINE    EOEXT         KKEXT
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KIDO - F9=Pre-Cert Info.
     C*================================================================
     C     KIDO          BEGSR
     C*
     C                   CALL      'U0060R01'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      *ZEROS        #1CO#             3
     C                   PARM      *ZEROS        #1SUB#            5
     C                   PARM      *ZEROS        #1FND             3
     C                   PARM      *ZEROS        #1FYR             3
     C                   PARM      *ZEROS        #1CASE            7
     C                   PARM      *BLANKS       #1CERT           15
     C                   PARM      *BLANKS       #1EXT             3
     C                   PARM      *BLANKS       #1OCST            1
     C                   PARM      *BLANKS       #1CONT            1
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C** GET PROVIDER SPECIALTY CODE (SERVICING PROVIDER)
     C*
     C                   Z-ADD     EOCHRG        SCCHRG
     C                   MOVEL(P)  EOCERT        SCCERT
     C                   MOVE      EOFEIN        KKFEIN
     C                   MOVE      EOMOD         KKMOD
     C*** BEG ADD *** 09/20/02 ****************************************
     C                   MOVE      EOFLIC        SCRGN#
     C*** BEG ADD *** 09/22/08 ****************************************
     C                   MOVE      *OFF          *IN63
     C     EOIDOC        IFEQ      *BLANKS
     C                   MOVE      *OFF          *IN63
     C                   ELSE
     C                   MOVE      *ON           *IN63
     C                   ENDIF
     C     EOIDOC        IFNE      *BLANKS
     C                   MOVE(P)   EOIDOC        SCIDOC
     C                   ENDIF
     C*** END ADD *** 09/22/08 ****************************************
     C*
     C* DISPLAY REGION/LOCALITY ONLY IF IT IS POPULATED
     C*
     C     SCRGN#        IFEQ      *BLANKS
     C                   SETOFF                                       25
     C                   ELSE
     C                   SETON                                        25
     C                   ENDIF
     C*** END ADD *** 09/20/02 ****************************************
     C*
     C     PDKY          CHAIN     PPD01                              99
     C*
     C  N99              MOVEL(P)  PDSPCD        SCSPCD
     C  N99              MOVEL(P)  PDZIP         SCZIP
     C   99              MOVEL(P)  *BLANKS       SCSPCD
     C   99              MOVE(P)   *ZEROS        SCZIP
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   MOVEL(P)  SCSPCD        KKKEY
     C*
     C     TBKY          CHAIN     PSP01                              99
     C*
     C  N99              MOVEL(P)  SPDESC        SCSCDS
     C   99              MOVEL(P)  *BLANKS       SCSCDS
     C*
     C     P1KY          CHAIN     PP101                              99
     C*
     C  N99              MOVEL(P)  P1ZIP         SCZIP
     C  N994             SUBST(P)  P1CNTY:6      SCCNTY
     C   99              MOVEL(P)  *BLANKS       SCCNTY
     C*
     C  N995             SUBST(P)  P1CNTY:1      SCSMSA
     C   99              MOVEL(P)  *BLANKS       SCSMSA
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  XXCNTY        KKKEY
     C*
     C     TBKY          CHAIN     PCC01                              99
     C*
     C  N99              MOVEL(P)  CCDESC        SCCDSC
     C   99              MOVEL(P)  *BLANKS       SCCDSC
     C*
     C     P1ST          CAT(P)    'SM':0        KKTBID
     C                   MOVEL(P)  SCSMSA        KKTKEY
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCSDSC
     C   99              MOVEL(P)  *BLANKS       SCSDSC
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       SCCDSC
     C                   MOVEL(P)  *BLANKS       SCSDSC
     C*
     C                   ENDIF
     C*
     C                   MOVEL     EOAST         SCAST
     C                   MOVEL(P)  EOAST         KKKEY
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C  N99              MOVEL(P)  STNAME        SCASNM
     C*
     C*** BEG ADD *** 04/11/07 ****************************************
     C*
     C                   MOVEL     EOPOST        SCPOST
     C                   MOVEL(P)  EOPOST        KKKEY
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C  N99              MOVEL(P)  STNAME        SCPSNM
     C   99              MOVEL(P)  *BLANKS       SCPSNM
     C*
     C*** END ADD *** 04/11/07 ****************************************
     C*
     C                   MOVE(P)   EOZIPA        SCZIPA
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C     EHKY          CHAIN     PEH01                              99
     C*** Beg Add *** 09/15/15 ****************************************
      /Free
             scd1Ds = *blanks;
             scd2Ds = *blanks;
             scd3Ds = *blanks;
             scd4Ds = *blanks;

             if        EHicdInd = 'Y';
       //*** Diag code 1
               scDia1 = EHicd1001;
               Chain (scDia1:'D') fmICD10l1;
               if %Found(fmICD10l1);
                  scd1Ds = ICdsc1;
                  //*** Begin Add *** 05/20/19 ************************
                  If ICdsc2 <> *blanks;
                     scd1Ds = ICdsc2;
                  Endif;
                  //*** End   Add *** 05/20/19 ************************
               Endif;

       //*** Diag code 2
               scDia2 = EHicd1002;
               Chain (scDia2:'D') fmICD10l1;
               if %Found(fmICD10l1);
                  scd2Ds = ICdsc1;
                  //*** Begin Add *** 05/20/19 ************************
                  If ICdsc2 <> *blanks;
                     scd2Ds = ICdsc2;
                  Endif;
                  //*** End   Add *** 05/20/19 ************************
               Endif;

       //*** Diag code 3
               scDia3 = EHicd1003;
               Chain (scDia3:'D') fmICD10l1;
               if %Found(fmICD10l1);
                  scd3Ds = ICdsc1;
                  //*** Begin Add *** 05/20/19 ************************
                  If ICdsc2 <> *blanks;
                     scd3Ds = ICdsc2;
                  Endif;
                  //*** End   Add *** 05/20/19 ************************
               Endif;

       //*** Diag code 4
               scDia4 = EHicd1004;
               Chain (scDia4:'D') fmICD10l1;
               if %Found(fmICD10l1);
                  scd4Ds = ICdsc1;
                  //*** Begin Add *** 05/20/19 ************************
                  If ICdsc2 <> *blanks;
                     scd4Ds = ICdsc2;
                  Endif;
                  //*** End   Add *** 05/20/19 ************************
               Endif;

             Endif;

       //*** Begin Add *** 09/05/17 *******************************************
       // *** Begin Delete 06/10/19 *******************************
       // visCvt# = *off;
       // DsbCvt# = *on;
       // *** End   Delete 06/10/19 *******************************

       Chain (EOco#:EOsub#:EOfnd) FMCXL;
       If %found(FMCXL) and CXcvt# = 'Y';
          If %open(WMCML1);
             Chain (EOco#:EOfnd:EOfyr:EOcase) WMCML1;
             If %found(WMCML1);
                SCpds# = xCMpds#;
       // *** Begin Delete 06/10/19 *******************************
       //       VisCvt# = *on;
       //       DsbCvt# = *off;
       // *** End   Delete 06/10/19 *******************************
             Endif;
          Endif;
       Endif;
       //*** End   Add *** 09/05/17 *******************************************
      /End-Free
     C*** End Add *** 09/15/15 ****************************************
     C*** Beg Add *** 09/15/15 ****************************************
     C                   if        EHicdInd <> 'Y'
     C*** End Add *** 09/15/15 ****************************************
     C*
     C                   MOVEL(P)  'D'           KKLOST
     C                   MOVE(P)   EHDIA1        SCDIA1
     C                   MOVE(P)   SCDIA1        KKICD9
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD1DS
     C*
     C                   MOVE(P)   EHDIA2        SCDIA2
     C                   MOVE(P)   SCDIA2        KKICD9
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD2DS
     C*
     C                   MOVE(P)   EHDIA3        SCDIA3
     C                   MOVE(P)   SCDIA3        KKICD9
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD3DS
     C*
     C                   MOVE(P)   EHDIA4        SCDIA4
     C                   MOVE(P)   SCDIA4        KKICD9
     C*
     C     DIKY          CHAIN     PDI01                              99
     C  N99              MOVEL     DIDSC1        SCD4DS
     C*** Beg Add *** 09/15/15 ****************************************
     C                   Endif
     C*** End Add *** 09/15/15 ****************************************
     C*
     C     CMKY          CHAIN     PCM01                              99
     C*
     C                   Z-ADD     CMMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMMI
     C*
     C                   MOVEL(P)  EOCERT        KKCERT
     C                   MOVEL(P)  EOEXT         KKEXT
     C*
     C     PCKY          CHAIN     PPC01                              99
     C*
     C     *IN16         IFEQ      *ON
     C                   MOVEL(P)  EOCERT        KKCERT
     C                   MOVEL(P)  EOEXT         KKEXT
     C     PCKY          CHAIN     PPC01                              99
     C   99              SETOFF                                       16
     C                   ENDIF                                                  *IN16 = On
     C*
     C*** BEG ADD *** 05/03/10 ****************************************
     C*
     C***** Beg Chg *** 04/04/17 **************************************
     C                   EVAL      SCSINV = EOSINV
     C*
     C**** *IN33         IFEQ      *ON
     C****               MOVEL     EOSINV        SCSINV
     C****               ENDIF
     C***** End Chg *** 04/04/17 **************************************
     C*
     C*** END ADD *** 05/03/10 ****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKI         CASEQ     *ON           KIDO                           F10=search
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
      /Free
       //*** Begin Add *** 09/05/17 *******************************************
       If %Open(WMCML1);
          Close WMCML1;
       Endif;
       //*** End   Add *** 09/05/17 *******************************************
      /End-Free
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C**  GET SYSTEM INFO
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C*
     C     ##UR          IFEQ      'Y'
     C                   SETON                                        16
     C                   ELSE                                                   ##UR = 'Y'
     C                   SETOFF                                       16
     C                   ENDIF                                                  ##UR = 'Y'
     C*
     C****** BEG ADD ***** 05/03/10 ***********************
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C     XXCLT#        IFEQ      '133'
     C                   SETON                                        33
     C                   ENDIF
     C*
     C****** END ADD ***** 05/03/10 ***********************
     C*
      /Free
       //*** Begin Add *** 09/05/17 *******************************************
       If not %open(WMCML1);
          Open WMCML1;
       Endif;
       //*** End  Add *** 09/05/17 *******************************************
      /End-Free
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C*
     C                   ENDSR
