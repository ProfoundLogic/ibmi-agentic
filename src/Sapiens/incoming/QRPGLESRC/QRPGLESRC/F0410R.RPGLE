     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*                                                               *
     H* Work with CodeReview Pended Bills                             *
     H*                                                               *
     H*****************************************************************
     H* Created on 05/24/2002 by C:62 for Project 114MDC              *
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  06/11/12 C:277 PROJECT 114MER
     H*                 Add Logic to subset EOCRPE = 'C' Or 'F' when @@CALL is blank
     H*                 Add Logic to subset EOCRPE = 'Y' Or 'E' when @@CALL is 'C'
     H*
     H*  11/16/12 C:287 Project: MC367
     H*                Disabled options 7,8,10,11,14,16,84 for GS
     H*
     H*  10/11/02 C:62 PROJECT 114MDC
     H*                EXCLUDE ANY EOBS WHERE EOCRPE = 'T'
     H*
     H*****************************************************************
     FF0410F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFAEOP     IF   E             DISK
     F                                     RENAME(PEO01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
VW#1 FFAEOL51   IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PEO01:DBRCDF)
VW#2 FFAEOL52   IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PEO01:DBRCD2)
VW#3 FFAEOL53   IF   E           K DISK
     F                                     INFDS(DBFDS3)
     F                                     RENAME(PEO01:DBRCD3)
VW#4 FFAEOL54   IF   E           K DISK
     F                                     INFDS(DBFDS4)
     F                                     RENAME(PEO01:DBRCD4)
VW#5 FFAEOL55   IF   E           K DISK
     F                                     INFDS(DBFDS5)
     F                                     RENAME(PEO01:DBRCD5)
     FFMCML1    IF   E           K DISK
     FFAESL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(20) CTDATA PERRCD(1)             Column headings
     D OF              S              1    DIM(34) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(34) ALT(OF)
     D QCA             S             56    DIM(14) CTDATA PERRCD(1)             CMD EXEC/QCMDEXC
     D*
     D DATA            S             74    DIM(5)
     D DTA1            S              1    DIM(74)                              VIEW 1 DATA
     D DTA2            S              1    DIM(74)                              VIEW 2 DATA
     D DTA3            S              1    DIM(74)                              VIEW 3 DATA
     D DTA4            S              1    DIM(74)                              VIEW 4 DATA
     D DTA5            S              1    DIM(74)                              VIEW 5 DATA
     D INF1            S              1    DIM(29)                              INFO 1 DATA
     D INF2            S              1    DIM(29)                              INFO 2 DATA
     D INF3            S              1    DIM(29)                              INFO 3 DATA
     D INF4            S              1    DIM(29)                              INFO 4 DATA
     D***PY SCOPYWW01E
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  38     39
     D                 DS
     D  QQCMD2                 1     56
     D  QQST2                 40     41
     D                 DS
     D  X1DT                   1     10
     D  X1DTM                  1      2
     D  X1DTD                  4      5
     D  X1DTY                  7     10
     D                 DS
     D  X2DT                   1      8
     D  X2DTM                  1      2
     D  X2DTD                  3      4
     D  X2DTY                  5      8
     D*
     D***PY SCOPYWW01I
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*** Beg Chg 06/11/12 ********************************************
     D*** OPTNM           C                   CONST('CodeReview Pended-
     D***                                      Bills')
     D OPTNM           S             23    Inz('CodeReview Pended-
     D                                      Bills')
     D*** End Chg 06/11/12 ********************************************
     D*** Beg Add 06/11/12 ********************************************
     D OPTNM1          S             23    Inz('ClaimCheck Pended-
     D                                      Bills')
     D xxCall          S                   Like(@@Call)
     D*** End Add 06/11/12 ********************************************
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F0350R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(4)
     D VIEWMX          C                   CONST(5)
     D SF1PAG          C                   CONST(8)
     D*================================================================
     D*  Program specific constants
     D*================================================================
     D HEADA           C                   CONST('Dates of Service')
     D CLROPT          C                   CONST('                    -
     D                                            NNN')
     D PCIOPT          C                   CONST('1515=Pre-Cert Info  -
     D                                            YYN')
     D CLMOPT          C                   CONST('8181=Claim info     -
     D                                            YYN')
     D EDIOPT          C                   CONST('1818=EDI Log Info   -
     D                                            YYN')
     D SSQOPT          C                   CONST('1919=Send Subsq 837 -
     D                                            YNN')
     D VCKOPT          C                   CONST('2121=Check info     -
     D                                            YYN')
     D DELETE          C                   CONST('Deleted')
     D*** Beg Add *** 06/11/12 **************************************************
     D/COPY SPRSTRING
     D*** End Add *** 06/11/12 **************************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EOB#            9 0
     C                   PARM                    @@CERT           15
     C                   PARM                    @@SFDT            8
     C                   PARM                    @@STDT            8
     C                   PARM                    @@SFEI            9
     C                   PARM                    @@SMOD            3
     C                   PARM                    @@RCN             3 0
     C                   PARM                    @@ERR             1
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    #2SUB#
     C                   PARM                    #2FND
     C                   PARM                    #2FYR
     C                   PARM                    #2CASE
     C                   PARM                    ##NWST            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     CHGPL         PLIST
     C                   PARM                    QQNUM            80
     C                   PARM                    QQMASK           10
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*
     C* PARM LIST FOR DARTS API
     C*
     C     DRTSPL        PLIST
     C                   PARM      *BLANKS       #1RTCD            1
     C                   PARM      ##WCIN        #1WCIN            1
     C                   PARM      ##WCDK        #1WDCK            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM      *BLANKS       ##KEY            40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C* key lists for eomb transaction file
     C*
     C     KEY           KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKLSN
     C                   KFLD                    KKFSN
     C                   KFLD                    KKFRDT
     C                   KFLD                    KKEOB#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C                   KFLD                    KKEOB#
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSS#
     C                   KFLD                    KKACDT
     C                   KFLD                    KKEOB#
     C*
     C     KEY4          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKEOB#
     C*
     C     KEY5          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKCRDT
     C                   KFLD                    KKEOB#
     C*
     C     ESKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     ESKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKIDT
     C                   KFLD                    KKITIM
     C*
     C* key list for provider demographics file
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     PDKY1         KLIST
     C                   KFLD                    KKFEIN
     C*
     C* key list for claims master file
     C*
     C     CMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* key list for subscriber master file
     C*
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C*
     C* key list for WCMC fund/group master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C* key list for generic table file
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID            4
     C                   KFLD                    KKKEY            10
     C*
     C* key list for Bill Entry Control File
     C*
     C     BEKY          KLIST
     C                   KFLD                    SCAST
     C                   KFLD                    SCSYS
     C                   KFLD                    SCBTYP
     C*
     C     UPKY          KLIST
     C                   KFLD                    KKUSR
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    EOLSN         KKLSN
     C     *LIKE         DEFINE    EOFSN         KKFSN
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EOPNAM        KKPNAM
     C     *LIKE         DEFINE    EOPNAM        HLPNAM
     C     *LIKE         DEFINE    EOFEIN        KKFEIN
     C     *LIKE         DEFINE    EOMOD         KKMOD
     C     *LIKE         DEFINE    EOSS#         KKSS#
     C     *LIKE         DEFINE    EOACDT        KKACDT
     C     *LIKE         DEFINE    EOSUB#        HLSUB#
     C     *LIKE         DEFINE    EOFND         HLFND
     C     *LIKE         DEFINE    EOFYR         HLFYR
     C     *LIKE         DEFINE    EOCASE        HLCASE
     C     *LIKE         DEFINE    EOFEIN        HLFEIN
     C     *LIKE         DEFINE    EOMOD         HLMOD
     C     *LIKE         DEFINE    EOUSER        HLIUSR
     C     *LIKE         DEFINE    EOFRDT        HLFRDT
     C     *LIKE         DEFINE    EOFRDT        HLTODT
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    EOACDT        HLSDOI
     C     *LIKE         DEFINE    EOEOB#        ##EOB#
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    EOAST         ##AST
     C     *LIKE         DEFINE    EOSYS         ##SYS
     C     *LIKE         DEFINE    EOBTYP        ##BTYP
     C     *LIKE         DEFINE    EOCRDT        KKCRDT
     C     *LIKE         DEFINE    EOFRDT        KKFRDT
     C     *LIKE         DEFINE    ESIDT         KKIDT
     C     *LIKE         DEFINE    ESITIM        KKITIM
     C     *LIKE         DEFINE    UPUSR         KKUSR
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
     C     SSSUB#        IFNE      *ZEROS
     C     SSFND         ORNE      *ZEROS
     C     SSFYR         ORNE      *ZEROS
     C     SSCASE        ORNE      *ZEROS
     C     SSEOB#        ORNE      *ZEROS
     C     SSCERT        ORNE      *BLANKS
     C     SSFRDT        ORNE      *ZEROS
     C     SSTODT        ORNE      *ZEROS
     C     SSFEIN        ORNE      *ZEROS
     C     SSMOD         ORNE      *BLANKS
     C     SSBTYP        ORNE      *BLANKS
     C                   SETON                                        11
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* DATSBR - Date check subrountine
     C*================================================================
     C     DATSBR        BEGSR
     C*
     C* THIS SUBROUTINE IS CALLED AFTER EVERY SEARCH TO PROPERLY
     C* FORMAT ALL DATES ON SCREEN
     C*
     C                   Z-ADD     SSFRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSFRDT
     C*
     C                   Z-ADD     SSTODT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSTODT
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* DLOVRD - Delete file overrides
     C*================================================================
     C     DLOVRD        BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(9)
     C                   PARM                    LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(10)
     C                   PARM                    LENGTH
     C*
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(11)
     C                   PARM                    LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(12)
     C                   PARM                    LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(13)
     C                   PARM                    LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(14)
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C*** Beg Add *** 06/11/12 ****************************************
      /Free
           If xxCall = 'C' And Test1l = '2'
             And EOCRNE = 'D';
             WWMSG1 = CenterFld('This EOB was previously processed through +
                      CodeReview and':%size(WWMSG1));
             WWMSG2 = CenterFld('cannot be processed through ClaimCheck in +
                      its current':%size(WWMSG2));
             WWMSG3 = CenterFld('state. To process this EOB through Claim+
                      Check you must re-':%size(WWMSG3));
             WWMSG4 = CenterFld('key the bill using the original bill from the +
                       provider.':%size(WWMSG4));
             WWMSG5 = CenterFld('Press Enter to return.':%size(WWMSG5));
             Exfmt Errwdw;
             *in90 = *on;
             Leavesr;
           Endif;
      /End-Free
     C*** End Add *** 06/11/12 ****************************************
     C*
     C*
     C* DON'T ALLOW A TWO TO CHANGE IF CODE REVIEW PENDED THIS ERROR
     C* DUE TO AN ERROR IN PROCESSING.
     C*
     C     SCSEL         IFEQ      '2'
     C     SCCRPE        ANDEQ     'E'
     C*
     C                   EXSR      MVDB04
     C                   EXFMT     SCR04
     C                   SETON                                        9084
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C* PROCESS SUBSET SEARCHES
     C*
     C                   EXSR      OTHFKS
     C     *IN90         CABEQ     *ON           CK2END
     C*
     C* CLEAR SUBSET DESCRIPTION FIELDS
     C*
     C                   EXSR      SSCLER
     C*
     C* START OF ERROR CHECKING
     C*
     C     SSFND         IFNE      *ZEROS
     C     SSSUB#        ANDEQ     *ZEROS
     C                   SETON                                        9082
     C                   GOTO      CK2END
     C                   ENDIF                                                  SSFND NE *ZEROS
     C*
     C     SSSUB#        IFEQ      *ZEROS
     C     SSFND         ANDNE     *ZEROS
     C     SSFYR         ANDNE     *ZEROS
     C     SSCASE        ANDNE     *ZEROS
     C                   SETON                                        9071
     C                   GOTO      CK2END
     C                   ENDIF                                                  SSSUB# = ZEROS
     C*
     C     SSFND         IFEQ      *ZEROS
     C     SSFYR         ANDNE     *ZEROS
     C     SSCASE        ANDNE     *ZEROS
     C                   SETON                                        9072
     C                   GOTO      CK2END
     C                   ENDIF                                                  SSFND = *ZEROS
     C*
     C     SSFYR         IFEQ      *ZEROS
     C     SSCASE        ANDNE     *ZEROS
     C                   SETON                                        9073
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C     SSFEIN        IFEQ      *ZEROS
     C     SSMOD         ANDNE     *BLANKS
     C                   SETON                                        9074
     C                   GOTO      CK2END
     C                   ENDIF
     C*
     C* validate subscriber number
     C*
     C     SSSUB#        IFNE      *ZEROS
     C                   Z-ADD     SSSUB#        KKSUB#
     C*
     C                   CALL      'FA726R'
     C                   PARM                    ##CO#             3 0
     C                   PARM      KKSUB#        ##SUB#            5 0
     C                   PARM                    ##NAME           40
     C                   PARM                    ##RRN             9 0
     C                   PARM      '0'           ##ERR             1
     C*
     C     ##ERR         IFEQ      '0'
     C                   SETON                                        9075
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SSSUB# NE *ZEROS
     C*
     C* validate group
     C*
     C     SSFND         IFNE      *ZEROS
     C                   Z-ADD     SSFND         KKFND
     C                   Z-ADD     SSSUB#        KKSUB#
     C*
     C                   CALL      'FA727R'
     C                   PARM                    ##CO#             3 0
     C                   PARM      KKSUB#        #2SUB#            5 0
     C                   PARM      KKFND         #2FND             3 0
     C                   PARM                    #2NAME           40
     C                   PARM                    ##RRN             9 0
     C                   PARM                    #2ERR             1
     C*
     C     #2ERR         IFEQ      '0'
     C                   SETON                                        9076
     C                   GOTO      CK2END
     C                   ENDIF                                                  #1ERR = '0'
     C                   ENDIF                                                  SSFND NE *ZEROS
     C*
     C* validate case
     C*
     C     SSCASE        IFNE      *ZEROS
     C                   Z-ADD     SSSUB#        KKSUB#
     C                   Z-ADD     SSFND         KKFND
     C                   Z-ADD     SSFYR         KKFYR
     C                   Z-ADD     SSCASE        KKCASE
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9077
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SSCASE NE *ZEROS
     C*
     C* validate provider
     C*
     C     SSFEIN        IFNE      *ZEROS
     C     SSMOD         ANDNE     *BLANKS
     C                   Z-ADD     SSFEIN        KKFEIN
     C                   MOVEL     SSMOD         KKMOD
     C                   CALL      'FA725R'
     C                   PARM      KKFEIN        #1FEIN            9 0
     C                   PARM      KKMOD         #1MOD             3 0
     C                   PARM                    #1NAME           40
     C                   PARM                    #1ERR             1
     C*
     C     #1ERR         IFEQ      '0'
     C                   SETON                                        9078
     C                   GOTO      CK2END
     C                   ELSE
     C*
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SSFEIN NE *ZEROS
     C*
     C     SSFEIN        IFNE      *ZEROS
     C     SSMOD         ANDEQ     *BLANKS
     C                   Z-ADD     SSFEIN        KKFEIN
     C                   CALL      'FA725R'
     C                   PARM      KKFEIN        #1FEIN            9 0
     C                   PARM                    #1MOD             3 0
     C                   PARM                    #1NAME           40
     C                   PARM                    #1ERR             1
     C*
     C     #1ERR         IFEQ      '0'
     C                   SETON                                        9078
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SSFEIN NE *ZEROS
     C*
     C* validate dates of service
     C*
     C     SSFRDT        IFNE      *ZEROS
     C                   Z-ADD     SSFRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSFRDT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9079
     C                   GOTO      CK2END
     C                   ENDIF                                                  SRERR = '1'
     C                   ENDIF                                                  SSFRDT NE *ZEROS
     C*
     C     SSTODT        IFNE      *ZEROS
     C                   Z-ADD     SSTODT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SSTODT
     C     SRERR         IFEQ      '1'
     C                   SETON                                        9083
     C                   GOTO      CK2END
     C                   ENDIF                                                  SRERR = '1'
     C                   ENDIF                                                  SSFRDT NE *ZEROS
     C*
     C* From Service Date cannot be greater than To Service Date
     C*
     C     SSFRDT        IFGT      *ZEROS
     C     SSTODT        ANDGT     *ZEROS
     C*
     C                   Z-ADD     SSFRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLFRDT
     C*
     C                   Z-ADD     SSTODT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLTODT
     C*
     C     HLFRDT        IFGT      HLTODT
     C                   SETON                                        9084
     C                   GOTO      CK2END
     C                   ENDIF                                                  HLFRDT GT HLTD
     C*
     C                   ENDIF                                                  SSFRDT GT *ZEROS
     C*
     C* validate bill status
     C*
     C     SSBLST        IFNE      *BLANKS
     C                   MOVEL(P)  SSBLST        KKKEY
     C                   MOVEL     'BLST'        KKTBID
     C*
     C                   CALL      'FA728R'
     C                   PARM                    ##CO#             3 0
     C                   PARM      KKTBID        ##TBID            4
     C                   PARM      KKKEY         #1KEY            10
     C                   PARM                    ##DESC           40
     C                   PARM                    ##RRN             9 0
     C                   PARM      '0'           ##ERR             1
     C*
     C     ##ERR         IFEQ      '0'
     C                   SETON                                        9080
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SSBLST NE *BLANKS
     C*
     C* validate bill type
     C*
     C     SSBTYP        IFNE      *BLANKS
     C                   MOVEL(P)  SSBTYP        KKKEY
     C                   MOVEL     'BTYP'        KKTBID
     C*
     C                   CALL      'FA728R'
     C                   PARM                    ##CO#
     C                   PARM      KKTBID        ##TBID
     C                   PARM      KKKEY         #1KEY
     C                   PARM                    ##DESC
     C                   PARM                    ##RRN
     C                   PARM      '0'           ##ERR
     C*
     C     ##ERR         IFEQ      '0'
     C                   SETON                                        9089
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SSBTYP NE *BLANKS
     C*
     C* validate initial user
     C*
     C     SSIUSR        IFNE      *BLANKS
     C                   CALL      'SA013R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      SSIUSR        ##USER           10
     C                   PARM      *BLANKS       ##USNM           50
     C                   PARM      *ZEROS        ##LVL             3 0
     C                   PARM      *BLANKS       ##TYPE            1
     C*
     C     ##RTCD        IFEQ      'F'
     C                   SETON                                        9081
     C                   GOTO      CK2END
     C                   ENDIF                                                  *IN99 = *ON
     C                   ENDIF                                                  SSIUSR NE *BLANKS
     C*
     C* CALL DATE FORMATTING SUBROUTINE
     C*
     C                   EXSR      DATSBR
     C*
     C     CK2END        ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C*
     C                   Z-ADD     0             SSSUB#
     C                   Z-ADD     0             SSFND
     C                   Z-ADD     0             SSFYR
     C                   Z-ADD     0             SSCASE
     C                   Z-ADD     0             #2SUB#            5 0
     C                   Z-ADD     0             #2FND             3 0
     C                   Z-ADD     0             #2FYR             3 0
     C                   Z-ADD     0             #2CASE            7 0
     C     Q1PRMS        IFGT      5
     C     @@SUB#        ANDNE     *ZEROS
     C     @@FND         ANDNE     *ZEROS
     C     @@FYR         ANDNE     *ZEROS
     C     @@CASE        ANDNE     *ZEROS
     C                   Z-ADD     @@SUB#        SSSUB#
     C                   Z-ADD     @@FND         SSFND
     C                   Z-ADD     @@FYR         SSFYR
     C                   Z-ADD     @@CASE        SSCASE
     C*
     C* PROTECT THE SUBSET FIELDS FOR SUB#/FND/FYR/CASE
     C*
     C                   SETON                                        50
     C*
     C* SET CASE PARMS FOR CALL TO ADJPGM WHEN SUBSETTED ON CASE
     C*
     C                   Z-ADD     SSSUB#        #2SUB#
     C                   Z-ADD     SSFND         #2FND
     C                   Z-ADD     SSFYR         #2FYR
     C                   Z-ADD     SSCASE        #2CASE
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       50
     C*
     C                   ENDIF                                                  Q1PARM GT 5
     C*
     C* LOAD EOMB# AND PRE-CERT# INTO SUBSET FIELDS IF THE NUMBER OF
     C* PARMS BEING PASSED IS GREATER THAN 5.
     C*
     C     Q1PRMS        IFGT      5
     C*
     C* LOAD EOMB# INFORMATION INTO SSEOB# AND PROTECT IF NOT BLANK.
     C*
     C                   Z-ADD     @@EOB#        SSEOB#
     C*
     C     SSEOB#        IFNE      *ZEROS
     C*
     C                   SETON                                        16
     C*
     C                   ENDIF                                                  SSEOB# NE *ZEROS
     C*
     C* LOAD PRE-CERT INFORMATION INTO SSCERT AND PROTECT IF NOT BLANK.
     C*
     C     Q1PRMS        IFGT      6
     C*
     C                   MOVEL(P)  @@CERT        SSCERT
     C*
     C     SSCERT        IFNE      *BLANKS
     C*
     C                   SETON                                        17
     C*
     C                   ENDIF                                                  SSCERT NE ' '
     C*
     C                   ENDIF                                                  Q1PRMS GT 6
     C*
     C                   ENDIF                                                  Q1PRMS GT 5
     C     Q1PRMS        IFGE      8
     C                   MOVE      @@SFDT        SSFRDT
     C                   MOVE      @@STDT        SSTODT
     C                   MOVE      @@SFEI        SSFEIN
     C                   MOVE      @@SMOD        SSMOD
     C     SSFEIN        IFEQ      *ZEROS
     C     SSMOD         OREQ      '000'
     C                   MOVE      '   '         SSMOD
     C                   ENDIF                                                  SSFEIN = 0
     C     SSFRDT        IFNE      *ZEROS
     C     SSTODT        ORNE      *ZEROS
     C                   SETON                                        18
     C                   ENDIF                                                  SSFRDT <> 0
     C     SSFEIN        IFNE      *ZEROS
     C     SSMOD         ORNE      *BLANKS
     C                   ENDIF                                                  SSFRDT <> 0
     C                   ENDIF                                                  Q1PRMS GE 8
     C*
     C* CHECK RESTRICTION FLAGS AND SUBSET AND PROTECT
     C*
     C                   MOVEL(P)  Q1USER        KKUSR
     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      *OFF
     C                   SELECT
     C     UPMRGR        WHENEQ    'Y'
     C                   Z-ADD     UPSUB#        SSSUB#
     C                   Z-ADD     UPFND         SSFND
     C                   SETON                                        5152
     C     UPMRSU        WHENEQ    'Y'
     C                   Z-ADD     UPSUB#        SSSUB#
     C                   SETON                                        51
     C                   ENDSL
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C*
     C                   MOVE      *LOVAL        KKLSN
     C                   MOVE      *LOVAL        KKFSN
     C                   MOVE      *LOVAL        KKEOB#
     C                   MOVE      *LOVAL        KKFEIN
     C                   MOVE      *LOVAL        KKMOD
     C                   MOVE      *LOVAL        KKSS#
     C                   MOVE      *LOVAL        KKACDT
     C                   MOVE      *LOVAL        KKCRDT
     C                   Z-ADD     *HIVAL        KKFRDT
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  SCPST1        KKLSN
     C                   MOVEL(P)  SCPSTA        KKFSN
     C*
     C     SCPSTF        IFNE      *ZEROS
     C                   MOVE      SCPSTF        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKFRDT
     C                   ELSE
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKFRDT
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        KKEOB#
     C*
     C     *IN22         WHENEQ    *ON
     C                   MOVEL     SCPST2        KKFEIN
     C                   MOVE      SCPSTB        KKMOD
     C*
     C     *IN23         WHENEQ    *ON
     C                   MOVEL     SCPST3        KKSS#
     C                   MOVEL     SCPSTC        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD                            date
     C                   MOVEL     SRDTO         KKACDT
     C*
     C     *IN24         WHENEQ    *ON
     C                   MOVE      SCPST4        KKEOB#
     C*
     C     *IN25         WHENEQ    *ON
     C                   MOVE      SCPST5        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKCRDT
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCLSN         KKLSN
     C                   MOVEL(P)  SCFSN         KKFSN
     C                   MOVEL(P)  HLPNAM        KKPNAM
     C                   MOVEL     SCFEIN        KKFEIN
     C                   MOVEL     SCMOD         KKMOD
     C                   MOVEL(P)  SCSS#         KKSS#
     C                   Z-ADD     HLSDOI        KKACDT
     C                   Z-ADD     SCEOB#        KKEOB#
     C                   MOVEL(P)  SCCRDT        KKCRDT
     C                   Z-ADD     SCFRD         KKFRDT
     C*
     C                   ENDSR
     C*================================================================
     C* KJDO - F10 Search Subroutine
     C*================================================================
     C     KJDO          BEGSR
     C*
     C* Indicators for cursor positioning
     C*
     C                   SETOFF                                       303132
     C                   SETOFF                                       333435
     C                   SETOFF                                       363738
     C*
     C* Subscriber Search
     C*
     C     HFLD          IFEQ      'SSSUB#'
     C                   Z-ADD     SSSUB#        HLSUB#
     C                   MOVEL     SSSMNM        HLSMNM           30
     C                   Z-ADD     0             ##RRN
     C                   CALL      'F0055R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   Z-ADD     HLSUB#        SSSUB#
     C                   MOVEL     HLSMNM        SSSMNM
     C                   SETON                                        9030
     C                   ELSE
     C*
     C                   CALL      'FA726R'
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##NAME           40
     C                   PARM                    ##RRN             9 0
     C                   PARM      '1'           ##ERR             1
     C*
     C                   MOVEL(P)  ##NAME        SSSMNM
     C                   Z-ADD     ##SUB#        SSSUB#
     C                   SETON                                        9031
     C                   ENDIF                                                  ##KC = '1'
     C*
     C                   EXSR      DATSBR
     C                   ENDIF                                                  HFLD = SSSUB#
     C*
     C* Group Search
     C*
     C     HFLD          IFEQ      'SSFND'
     C                   Z-ADD     0             ##RRN
     C                   Z-ADD     SSFND         HLFND
     C                   MOVEL     SSFNNM        HLFNNM           30
     C                   CALL      'F0060R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##RRN
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   Z-ADD     HLFND         SSFND
     C                   MOVEL     HLFNNM        SSFNNM
     C                   SETON                                        9031
     C                   ELSE
     C*
     C                   CALL      'FA727R'
     C                   PARM                    ##CO#             3 0
     C                   PARM                    #2SUB#            5 0
     C                   PARM                    #2FND             3 0
     C                   PARM                    #2NAME           40
     C                   PARM                    ##RRN             9 0
     C                   PARM      '1'           #2ERR             1
     C*
     C                   Z-ADD     #2FND         SSFND
     C                   MOVEL(P)  #2NAME        SSFNNM
     C                   SETON                                        9032
     C                   ENDIF                                                  ##KC = '1'
     C*
     C                   EXSR      DATSBR
     C                   ENDIF                                                  HFLD = SSFND
     C*
     C* Fund Year/Case Search
     C*
     C     HFLD          IFEQ      'SSFYR'
     C     HFLD          OREQ      'SSCASE'
     C                   Z-ADD     SSFYR         HLFYR
     C                   Z-ADD     SSCASE        HLCASE
     C                   CALL      'F0269R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##SUB#            5 0
     C                   PARM      *ZEROS        ##FND
     C                   PARM      *ZEROS        ##FYR             3 0
     C                   PARM      *ZEROS        ##CASE            7 0
     C                   PARM      *BLANKS       ##SS#             9
     C                   PARM      *ZEROS        ##ACDT            8 0
     C                   PARM      ' '           ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FYR         OREQ      *ZEROS
     C     ##CASE        OREQ      *ZEROS
     C                   Z-ADD     HLFYR         SSFYR
     C                   Z-ADD     HLCASE        SSCASE
     C                   SETON                                        9032
     C                   ELSE
     C                   Z-ADD     ##FYR         SSFYR
     C                   Z-ADD     ##CASE        SSCASE
     C                   Z-ADD     ##SUB#        SSSUB#
     C                   Z-ADD     ##FND         SSFND
     C                   Z-ADD     ##SUB#        KKSUB#
     C                   Z-ADD     ##FND         KKFND
     C                   Z-ADD     ##FYR         KKFYR
     C                   Z-ADD     ##CASE        KKCASE
     C*
     C* get claimaint name
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CMLSN         ##LAST           56
     C                   MOVEL(P)  CMFSN         ##FRST           15
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL(P)  ##LAST        SSNAME
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   CALL      'FA726R'
     C                   PARM                    ##CO#             3 0
     C                   PARM      KKSUB#        ##SUB#            5 0
     C                   PARM                    ##NAME           40
     C                   PARM                    ##RRN             9 0
     C                   PARM      '0'           ##ERR             1
     C*
     C                   MOVEL(P)  ##NAME        SSSMNM
     C*
     C                   CALL      'FA727R'
     C                   PARM                    ##CO#             3 0
     C                   PARM      KKSUB#        #2SUB#            5 0
     C                   PARM      KKFND         #2FND             3 0
     C                   PARM                    #2NAME           40
     C                   PARM                    ##RRN             9 0
     C                   PARM                    #2ERR             1
     C*
     C                   MOVEL(P)  #2NAME        SSFNNM
     C*
     C*
     C                   SETON                                        9033      pos cursor
     C                   ENDIF                                                  ##RTCD = A
     C                   EXSR      DATSBR
     C                   ENDIF                                                  HFLD = SSFYR
     C*
     C* Provider Search
     C*
     C     HFLD          IFEQ      'SSFEIN'
     C     HFLD          OREQ      'SSMOD'
     C                   MOVE      SSFEIN        HLFEIN
     C                   MOVE      SSMOD         HLMOD
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *BLANKS       ##FEIN            9
     C                   PARM      *BLANKS       ##MOD             3
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM      *BLANKS       ##TYPE            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FEIN        OREQ      *BLANKS
     C                   MOVE      HLFEIN        SSFEIN
     C                   MOVE      HLMOD         SSMOD
     C                   SETON                                        9033
     C                   ELSE
     C                   MOVE      ##FEIN        SSFEIN
     C                   MOVE      ##MOD         SSMOD
     C                   SETON                                        9034
     C                   ENDIF                                                  ##RTCD = A
     C*
     C                   Z-ADD     SSFEIN        KKFEIN
     C                   MOVEL     SSMOD         KKMOD
     C                   CALL      'FA725R'
     C                   PARM      KKFEIN        #1FEIN            9 0
     C                   PARM      KKMOD         #1MOD             3 0
     C                   PARM                    #1NAME           40
     C                   PARM                    #1ERR             1
     C*
     C     #1ERR         IFNE      '0'
     C                   MOVEL(P)  #1NAME        SSPDNM           20
     C                   ENDIF
     C*
     C                   EXSR      DATSBR
     C                   ENDIF                                                  HGLD = SSFEIN
     C*
     C     HFLD          IFEQ      'SSBLST'
     C                   MOVE      SSBLST        HLBLST            1
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *BLANKS       ##KC
     C                   PARM      *BLANKS       ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      'BLST'        ##TBID
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   MOVE      HLBLST        SSBLST
     C                   MOVEL     SSBSDS        HLBSDS           30
     C                   SETON                                        9035
     C                   ELSE
     C                   MOVEL     HLBSDS        SSBSDS           30
     C                   SETON                                        9037
     C*
     C                   CALL      'FA728R'
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##TBID            4
     C                   PARM                    #1KEY            10
     C                   PARM                    ##DESC           40
     C                   PARM                    ##RRN             9 0
     C                   PARM      '1'           ##ERR             1
     C*
     C                   MOVEL     #1KEY         SSBLST
     C                   MOVEL     ##DESC        SSBSDS
     C                   ENDIF                                                  ##KC = 1
     C*
     C                   EXSR      DATSBR
     C                   ENDIF                                                  HFLD = SSBLST
     C*
     C     HFLD          IFEQ      'SSBTYP'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *BLANKS       ##KC
     C                   PARM      *BLANKS       ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      'BTYP'        ##TBID
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9037
     C                   ELSE
     C                   MOVEL     HLBSDS        SSBSDS           30
     C                   SETON                                        9036
     C*
     C                   CALL      'FA728R'
     C                   PARM                    ##CO#
     C                   PARM                    ##TBID
     C                   PARM      *BLANKS       #1KEY
     C                   PARM      *BLANKS       ##DESC
     C                   PARM                    ##RRN
     C                   PARM      '1'           ##ERR
     C*
     C                   MOVEL     #1KEY         SSBTYP
     C                   MOVEL     ##DESC        SCBTDS
     C*
     C                   ENDIF                                                  ##KC = 1
     C*
     C                   EXSR      DATSBR
     C                   ENDIF                                                  HFLD = SSBTYP
     C*
     C* Initial User Search
     C*
     C     HFLD          IFEQ      'SSIUSR'
     C                   MOVEL(P)  SSIUSR        HLIUSR
     C                   MOVEL(P)  SSUSNM        HLUSNM           40
     C                   CALL      'S0005R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      *ZEROS        ##VIEW            1 0
     C     ##RRN         IFEQ      *ZEROS
     C                   MOVEL(P)  HLIUSR        SSIUSR
     C                   MOVEL(P)  HLUSNM        SSUSNM
     C                   SETON                                        9036
     C                   ELSE
     C                   CALL      'SA013R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM      ##RRN         ##RRN
     C                   PARM      *BLANKS       ##USER
     C                   PARM      *BLANKS       ##USNM
     C                   PARM      *ZEROS        ##LVL
     C                   PARM      *BLANKS       ##TYPE
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVEL(P)  ##USER        SSIUSR
     C                   MOVEL(P)  ##USNM        SSUSNM
     C                   ELSE
     C                   MOVEL     *BLANKS       SSIUSR
     C                   MOVEL     *BLANKS       SSUSNM
     C                   ENDIF
     C                   SETON                                        90
     C                   ENDIF                                                  ##RRN = ZEROS
     C*
     C                   EXSR      DATSBR
     C                   ENDIF                                                  HFLD = SSIUSR
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* OPT7 - Work with Detail
     C*================================================================
     C     OPT7          BEGSR
     C*
     C                   MOVE      'D'           ##MODE
     C*
     C                   EXSR      OVRIDE
     C                   MOVE      ' '           ##KL
     C*
     C                   CALL      'FA730R'
     C                   PARM      SCAST         ##AST
     C                   PARM      SCSYS         ##SYS
     C                   PARM      SCBTYP        ##BTYP
     C                   PARM                    ##DEPG           10
     C*
     C                   MOVE(P)   ##DEPG        ##PGM
     C                   MOVE      *BLANKS       ##PGM2           10
     C*
     C                   CALL      'F0390R'
     C                   PARM                    ##MODE
     C                   PARM                    ##KL
     C                   PARM      SCDBRR        ##RRN
     C                   PARM                    ##PGM
     C                   PARM                    ##PGM2
     C                   EXSR      DLOVRD
     C*
     C                   ENDSR
     C*================================================================
     C* OPT8 - Work with Amounts
     C*================================================================
     C     OPT8          BEGSR
     C*
     C                   CALL      'F0396R'
     C                   PARM      ' '           ##CALL
     C                   PARM                    SCDBRR
     C*
     C                   ENDSR
     C*================================================================
     C* OPT10 - Work with Bill Status History
     C*================================================================
     C     OPT10         BEGSR
     C*
     C                   EXSR      OVRIDE
     C*
     C                   CALL      'F0395R'
     C                   PARM      ' '           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*
     C                   EXSR      DLOVRD
     C*
     C                   ENDSR
     C*================================================================
     C* OPT11 - Work with EOB Notes
     C*================================================================
     C     OPT11         BEGSR
     C*
     C                   CALL      'F0355R'
     C                   PARM      '1'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM      SCDBRR        ##RRN2            9 0
     C*
     C                   ENDSR
     C*================================================================
     C* OPT14 - DARTS
     C*================================================================
     C     OPT14         BEGSR
     C*
     C                   MOVE(P)   ##CO#         #1CO#
     C                   MOVE(P)   SCSUB#        #1SUB#
     C                   MOVE(P)   SCFND         #1FND
     C                   MOVE(P)   SCFYR         #1FYR
     C                   MOVE(P)   SCCASE        #1CASE
     C*
     C                   CALL      'FA703R'      DRTSPL
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* OPT16 - History
     C*================================================================
     C     OPT16         BEGSR
     C*
     C*  Call new history pgm F0391R
     C*
     C                   CALL      'F0391R'
     C                   PARM      ' '           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM                    ##CO#
     C                   PARM                    SCSUB#
     C                   PARM                    SCFND
     C                   PARM                    SCFYR
     C                   PARM                    SCCASE
     C                   PARM                    SCEOB#
     C*
     C                   ENDSR
     C*================================================================
     C* OVRIDE - Overide files based  on adjudication state
     C*================================================================
     C     OVRIDE        BEGSR
     C*
     C                   Z-ADD     56            LENGTH           15 5
     C                   MOVEL(P)  QCA(1)        QQCMD
     C                   MOVEL(P)  SCAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH
     C                   MOVEL(P)  QCA(2)        QQCMD
     C                   MOVEL(P)  SCAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH
     C                   MOVEL(P)  QCA(3)        QQCMD
     C                   MOVEL(P)  SCAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH
     C*
     C                   MOVEL(P)  QCA(4)        QQCMD
     C                   MOVEL(P)  SCAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH
     C                   MOVEL(P)  QCA(5)        QQCMD2
     C                   MOVEL(P)  SCAST         QQST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD2
     C                   PARM                    LENGTH
     C                   MOVEL(P)  QCA(6)        QQCMD2
     C                   MOVEL(P)  SCAST         QQST2
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD2
     C                   PARM                    LENGTH
     C                   MOVEL(P)  QCA(7)        QQCMD
     C                   MOVEL(P)  SCAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH
     C*
     C                   ENDSR
     C*================================================================
     C* MVDB04 - Move data base fields to SCR04
     C*================================================================
     C     MVDB04        BEGSR
     C*
     C** Get standard screen headings
     C*
     C     SCDBRR        CHAIN     PFRCDF                             99
     C*
     C                   MOVEL     EOSPCD        SCSPCD
     C                   MOVEL     EONTWK        SCNWCD
     C                   MOVEL     EOFLIC        SCSVZP
     C*
     C                   SETOFF                                       69
     C     SCNWCD        IFNE      *BLANKS
     C                   SETON                                        69
     C                   ENDIF
     C*
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C                   Z-ADD     SCEOB#        KKEOB#
     C                   MOVEL     *HIVAL        KKIDT
     C                   MOVEL     *HIVAL        KKITIM
     C     ESKY2         SETGT     PES01
     C     ESKY          READPE    PES01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     ESREAS        SCREAS
     C                   ELSE
     C                   MOVEL     *BLANKS       SCREAS
     C                   ENDIF
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD
     C                   PARM                    #1CO#
     C                   PARM                    #1SUB#
     C                   PARM                    #1FND
     C                   PARM                    #1FYR
     C                   PARM                    #1CASE
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C*
     C                   CALL      'FA707R'
     C                   PARM                    ##RTCD
     C                   PARM      SCDBRR        ##RRN2
     C                   PARM                    #1L1
     C*
     C                   MOVEL     #1L1          SCHD3
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     EOCRPE        SCCRPE
     C                   Z-ADD     EOCRDT        SCCRDT
     C                   Z-ADD     EOFRDT        SCFRD
     C                   MOVEL     EOAST         SCAST
     C                   MOVEL     EOSYS         SCSYS
     C                   MOVEL     EOBTYP        SCBTYP
     C                   MOVEL(P)  EOLSN         SCLSN
     C                   MOVEL(P)  EOFSN         SCFSN
     C                   MOVEL(P)  SCLSN         ##LAST           56            first & last
     C                   MOVEL(P)  SCFSN         ##FRST           15                name
     C                   MOVE      '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL     ##LAST        SCNAME
     C*
     C                   MOVE      EOEOB#        SCEOB#
     C                   Z-ADD     EORCN         SCRCN
     C                   MOVEL(P)  EOEOB#        QQNUM
     C                   MOVE      *BLANKS       QQMASK
     C                   CALL      'SA020R'      CHGPL
     C                   MOVEL     QQNUM         ALEOB#            9
     C*
     C                   Z-ADD     EOFEIN        KKFEIN
     C                   Z-ADD     EOMOD         KKMOD
     C                   CALL      'FA725R'
     C                   PARM      KKFEIN        #1FEIN            9 0
     C                   PARM      KKMOD         #1MOD             3 0
     C                   PARM                    #1NAME           40
     C                   PARM                    #1ERR             1
     C*
     C     #1ERR         IFNE      '0'
     C                   MOVEL(P)  #1NAME        SCPNAM
     C                   ENDIF
     C*
     C** 'HLPNAM' FIELD IS USED TO HOLD ENTIRE PROVIDER NAME FOR
     C**  USE IN KKPNAM KEY FIELD FOR PAGING (KEYSFL SUBROUTINE)
     C*
     C                   MOVEL     #1NAME        HLPNAM
     C*
     C                   Z-ADD     EOFRDT        SRDTI                          from service
     C                   CALL      'SRCVTDAT'    DTMDYY                            date
     C                   MOVEL     SRDTO         HLDATE            8
     C                   MOVEL(P)  HLDATE        QQNUM
     C                   MOVEL(P)  'DATE'        QQMASK
     C                   CALL      'SA020R'      CHGPL
     C                   MOVEL     QQNUM         SCFRDT
     C*
     C                   Z-ADD     EOTODT        SRDTI                          to service
     C                   CALL      'SRCVTDAT'    DTMDYY                            date
     C                   MOVEL     SRDTO         HLDATE
     C                   MOVEL(P)  HLDATE        QQNUM
     C                   MOVEL(P)  'DATE'        QQMASK
     C                   CALL      'SA020R'      CHGPL
     C                   MOVEL     QQNUM         SCTODT
     C*
     C                   Z-ADD     EOACDT        SRDTI                          to service
     C                   Z-ADD     EOACDT        HLSDOI
     C                   CALL      'SRCVTDAT'    DTMDYY                            date
     C                   MOVEL     SRDTO         HLDATE
     C                   MOVEL     SRDTO         SHACDT
     C                   MOVEL(P)  HLDATE        QQNUM
     C                   MOVEL(P)  'DATE'        QQMASK
     C                   CALL      'SA020R'      CHGPL
     C                   MOVEL     QQNUM         SCACDT
     C*
     C                   Z-ADD     EOCRDT        SRDTI                          from service
     C                   CALL      'SRCVTDAT'    DTMDYY                            date
     C                   MOVEL     SRDTO         HLDATE            8
     C                   MOVEL(P)  HLDATE        QQNUM
     C                   MOVEL(P)  'DATE'        QQMASK
     C                   CALL      'SA020R'      CHGPL
     C                   MOVEL     QQNUM         XXCRDT           10
     C*
     C     EOBLST        IFEQ      'D'
     C                   MOVEL(P)  'DEL'         SCBLST
     C                   ELSE
     C                   MOVEL(P)  EOBLST        SCBLST                         bill status
     C                   ENDIF                                                  EOBLST = D
     C*
     C                   MOVE      EOFEIN        SCFEIN
     C                   MOVE      SCFEIN        ALFEIN            9
     C                   MOVE      EOMOD         SCMOD
     C                   MOVE      SCMOD         ALMOD             3
     C                   MOVEL(P)  EOSS#         SCSS#
     C*
     C                   MOVEL(P)  EOCHRG        QQNUM
     C                   MOVEL(P)  'X,XXX.XX'    QQMASK
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL     QQNUM         SCCHRG
     C                   MOVE      SCCHRG        ALCHRG           10
     C*
     C                   MOVEL(P)  EOPAID        QQNUM
     C                   MOVEL(P)  'X,XXX.XX'    QQMASK
     C                   CALL      'SA020R'
     C                   PARM                    QQNUM
     C                   PARM                    QQMASK
     C                   MOVEL     QQNUM         SCPAID
     C                   MOVE      SCPAID        ALPAID           10
     C*
     C                   Z-ADD     EOSUB#        SCSUB#
     C                   Z-ADD     EOFND         SCFND
     C                   Z-ADD     EOFYR         SCFYR
     C                   Z-ADD     EOCASE        SCCASE
     C*
     C                   CLEAR                   SCDATA
     C*
     C                   SELECT
     C*  I had to change the HD#s to accomidate the new Control
     C*  Number view.
     C*
     C*** LOAD VIEW# 1 FIELDS
     C*
     C     HD#           WHENEQ    1
     C     HD#           OREQ      6
     C     HD#           OREQ      11
     C     HD#           OREQ      16
     C                   Z-ADD     1             Y                 2 0
     C                   MOVEA     SCNAME        DTA1(Y)
     C                   ADD       21            Y
     C                   MOVEA     ALEOB#        DTA1(Y)
     C                   ADD       11            Y
     C                   MOVE      SCFEIN        X1FEIN            9
     C                   MOVEA     X1FEIN        DTA1(Y)
     C                   ADD       9             Y
     C                   MOVEA     ' '           DTA1(Y)
     C                   ADD       1             Y
     C                   MOVE      SCMOD         X1MOD             3
     C                   MOVEA     X1MOD         DTA1(Y)
     C                   MOVEA(P)  DTA1          SCDATA
     C*
     C*** LOAD VIEW# 2 FIELDS
     C*
     C     HD#           WHENEQ    2
     C     HD#           OREQ      7
     C     HD#           OREQ      12
     C     HD#           OREQ      17
     C                   Z-ADD     1             Y
     C                   MOVEA     ALFEIN        DTA2(Y)
     C                   ADD       10            Y
     C                   MOVEA     ALMOD         DTA2(Y)
     C                   ADD       5             Y
     C                   MOVEA     SCNAME        DTA2(Y)
     C                   ADD       21            Y
     C                   MOVEA     ALEOB#        DTA2(Y)
     C                   MOVEA(P)  DTA2          SCDATA
     C*
     C*** LOAD VIEW# 3 FIELDS
     C*
     C     HD#           WHENEQ    3
     C     HD#           OREQ      8
     C     HD#           OREQ      13
     C     HD#           OREQ      18
     C                   Z-ADD     1             Y
     C                   MOVEA     SCSS#         DTA3(Y)
     C                   ADD       11            Y
     C                   MOVEA     SCACDT        DTA3(Y)
     C                   ADD       11            Y
     C                   MOVEA     ALEOB#        DTA3(Y)
     C                   ADD       10            Y
     C                   MOVE      SCFEIN        X1FEIN            9
     C                   MOVEA     X1FEIN        DTA3(Y)
     C                   ADD       9             Y
     C                   MOVEA     ' '           DTA3(Y)
     C                   ADD       1             Y
     C                   MOVE      SCMOD         X1MOD             3
     C                   MOVEA     X1MOD         DTA3(Y)
     C                   MOVEA(P)  DTA3          SCDATA
     C*
     C*** LOAD VIEW# 4 FIELDS
     C*
     C     HD#           WHENEQ    4
     C     HD#           OREQ      9
     C     HD#           OREQ      14
     C     HD#           OREQ      19
     C                   Z-ADD     1             Y
     C                   MOVEA     ALEOB#        DTA4(Y)
     C                   ADD       11            Y
     C                   MOVEA     SCNAME        DTA4(Y)
     C                   ADD       21            Y
     C                   MOVE      SCFEIN        X1FEIN            9
     C                   MOVEA     X1FEIN        DTA4(Y)
     C                   ADD       9             Y
     C                   MOVEA     ' '           DTA4(Y)
     C                   ADD       1             Y
     C                   MOVE      SCMOD         X1MOD             3
     C                   MOVEA     X1MOD         DTA4(Y)
     C                   MOVEA(P)  DTA4          SCDATA
     C*
     C*** LOAD VIEW# 5 FIELDS
     C*
     C     HD#           WHENEQ    5
     C     HD#           OREQ      10
     C     HD#           OREQ      15
     C     HD#           OREQ      20
     C                   Z-ADD     2             Y
     C                   MOVEA     XXCRDT        DTA5(Y)
     C                   ADD       14            Y
     C                   MOVEA     SCNAME        DTA5(Y)
     C                   ADD       21            Y
     C                   MOVEA     ALEOB#        DTA5(Y)
     C                   MOVEA(P)  DTA5          SCDATA
     C*
     C                   ENDSL
     C*
     C                   SELECT
     C*
     C* LOAD INFO #1
     C*
     C     INFO#         WHENEQ    1
     C                   Z-ADD     2             X                 2 0
     C                   MOVEA     SCFRDT        INF1(X)
     C                   ADD       12            X
     C                   MOVEA     SCTODT        INF1(X)
     C                   ADD       13            X
     C                   MOVEA     SCBLST        INF1(X)
     C                   MOVEA     INF1          HLINF1           29
     C                   MOVE      HLINF1        SCDATA
     C*
     C* LOAD INFO #2
     C*
     C     INFO#         WHENEQ    2
     C                   Z-ADD     1             X
     C                   MOVEA     ALCHRG        INF2(X)
     C                   Z-ADD     12            X
     C                   MOVEA     ALPAID        INF2(X)
     C                   ADD       12            X
     C                   MOVEA     SCBTYP        INF2(X)
     C                   ADD       3             X
     C                   MOVEA     EOAST         INF2(X)
     C                   MOVEA     INF2          HLINF2           29
     C                   MOVE      HLINF2        SCDATA
     C*
     C* LOAD INFO #3
     C*
     C     INFO#         WHENEQ    3
     C                   Z-ADD     2             X                 2 0
     C                   MOVEA     EOCNTL        INF3(X)
     C                   ADD       20            X
     C                   MOVEA     SCBLST        INF3(X)
     C                   MOVEA     INF3          HLINF3           29
     C                   MOVE      HLINF3        SCDATA
     C*
     C* LOAD INFO #4
     C*
     C     INFO#         WHENEQ    4
     C                   Z-ADD     2             X                 2 0
     C                   MOVEA     EOPNAM        INF4(X)
     C                   ADD       24            X
     C                   MOVEA     ' '           INF4(X)
     C                   ADD       1             X
     C                   MOVEA     SCBLST        INF4(X)
     C                   MOVEA     INF4          HLINF4           29
     C                   MOVE      HLINF4        SCDATA
     C*
     C*
     C                   ENDSL
     C*
     C     INFO#         IFEQ      1
     C                   MOVEL     HEADA         SCHDLA
     C                   ELSE
     C                   CLEAR                   SCHDLA
     C                   ENDIF
     C*
     C* THE FOLLOWING ARE HIDDEN FIELDS THAT ARE NOT DISPLAYED BUT
     C* USED IN THIS PROGARM
     C*
     C                   MOVEL(P)  EOCERT        SCCERT
     C                   Z-ADD     EOEXT         SCEXT
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C                   SETOFF                                       70
     C*
     C     *INKJ         CASEQ     *ON           KJDO                           F10=Search
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '7'           OPT7
     C     SCSEL         CASEQ     '8'           OPT8
     C     SCSEL         CASEQ     '10'          OPT10
     C     SCSEL         CASEQ     '11'          OPT11
     C     SCSEL         CASEQ     '14'          OPT14
     C     SCSEL         CASEQ     '16'          OPT16                          16 = History
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C     QQFLAG        IFEQ      '0'
     C* Call CL to release the lock on Data area FDAILYA
     C                   CALL      'F0350C03'
     C                   ENDIF
     C*
     C     STOP          IFEQ      *ON
     C                   MOVE      *OFF          STOP              1
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C* Call CL to place a lock on Data area FDAILYA
     C*
     C                   CALL      'F0350C02'
     C                   PARM      '0'           QQFLAG            1
     C*
     C     QQFLAG        IFEQ      '1'
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
     C*
     C                   SETON                                        12
     C                   MOVE      *OFF          STOP              1
     C*
     C* CALL API FOR SYSTEM FLAGS
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           ##WCIF            1
     C                   PARM      ' '           ##MCIF            1
     C                   PARM      ' '           ##WCIN            1
     C                   PARM      ' '           ##AR              1
     C                   PARM      ' '           ##NTWK            1
     C                   PARM      ' '           ##UR              1
     C                   PARM      ' '           ##MED             1
     C                   PARM      ' '           ##EDI             1
     C                   PARM      ' '           ##WCDK            1
     C                   PARM      ' '           ##RHAB            1
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILL
     C                   PARM                    ##NPWC            1            NON-PDS WC  LS
     C*
     C** GET INITIAL VIEW FROM CONTROL FILE SFBPF (THEIR ARE 4 POSSIBLE VIEWS)
     C*
     C                   CALL      'SA009R'
 1   C                   PARM      *ZEROS        ##EOBD            3 0          EOMB DELETE SECURITY
 2   C                   PARM      ' '           ##EOBI            1            EOMB INTERACIVE PROC
 3   C                   PARM      ' '           ##ARIN            1            AUDIT/RECON INSTALLE
 4   C                   PARM      ' '           ##ACIS            1            AUTO CHECK ISSUANCE
 5   C                   PARM      ' '           ##PAY0            1            PRINT WCMC EOB PAY>0
 6   C                   PARM      ' '           ##SDTS            1            ALLOW FUTURE SERV DT
 7   C                   PARM      ' '           ##MCIV            1            MED CASES INIT VIEW
 8   C                   PARM      ' '           ##PBIV            1            PROV BILLS ININT VIE
     C*
     C                   Z-ADD     5             VIEW#
     C*
     C*** Beg Add *** 06/11/12 ****************************************
      /Free
         If @@Call = 'C';
            Optnm = Optnm1;
            xxCall = @@Call;
            @@Call = *blanks;
         Endif;
      /End-Free
     C*** End Add *** 06/11/12 ****************************************
     C*
     C*** Beg Add *** MC367 *** 11/16/12 ***********************************
      /Free
      /If Not Defined(WEB)
           XXOpfn = '1';  //These two fields remain the same for all options
           XXMIS = 'NNN'; //being conditioned here
       //Disable option 07 - Work w/ Detail
           XXNmbr = '07';
           Exsr UPDOFA;
       //Disable option 08 - Work w/ Amounts
           XXNmbr = '08';
           Exsr UPDOFA;
       //Disable option 10 - EOMB status
           XXNmbr = '10';
           Exsr UPDOFA;
       //Disable option 11 - EOB/Notes
           XXNmbr = '11';
           Exsr UPDOFA;
       //Disable option 14 - DARTS
           XXNmbr = '14';
           Exsr UPDOFA;
       //Disable option 16 - History
           XXNmbr = '16';
           Exsr UPDOFA;
      /EndIf
      /End-Free
     C*** End Add *** MC367 *** 11/16/12 ***********************************
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C*
     C   21KEY           READE     DBRCDF                                 95
     C   22KEY           READE     DBRCD2                                 95
     C   23KEY           READE     DBRCD3                                 95
     C   24KEY           READE     DBRCD4                                 95
     C   25KEY           READE     DBRCD5                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C*
     C   21KEY           READPE    DBRCDF                                 95
     C   22KEY           READPE    DBRCD2                                 95
     C   23KEY           READPE    DBRCD3                                 95
     C   24KEY           READPE    DBRCD4                                 95
     C   25KEY           READPE    DBRCD5                                 95
     C*
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCD2                             96
     C   23KEY3          SETGT     DBRCD3                             96
     C   24KEY4          SETGT     DBRCD4                             96
     C   25KEY5          SETGT     DBRCD5                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCD2                             96
     C   23KEY3          SETLL     DBRCD3                             96
     C   24KEY4          SETLL     DBRCD4                             96
     C   25KEY5          SETLL     DBRCD5                             96
     C*
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   Z-ADD     SCSUB#        @@SUB#
     C                   Z-ADD     SCFND         @@FND
     C                   Z-ADD     SCFYR         @@FYR
     C                   Z-ADD     SCCASE        @@CASE
     C                   Z-ADD     SCEOB#        @@EOB#
     C*
     C     Q1PRMS        IFGT      11
     C                   Z-ADD     SCRCN         @@RCN
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* SSCLER - Clear subset description fields
     C*================================================================
     C     SSCLER        BEGSR
     C*
     C* CLEAR ALL SUBSET DESCRIPTIONS IF CORRESPONDING SUBSET FIELD
     C* IS EMPTY
     C*
     C     SSSUB#        IFEQ      *ZEROS
     C                   CLEAR                   SSSMNM
     C                   ENDIF
     C*
     C     SSFND         IFEQ      *ZEROS
     C                   CLEAR                   SSFNNM
     C                   ENDIF
     C*
     C     SSFYR         IFEQ      *ZEROS
     C     SSCASE        ANDEQ     *ZEROS
     C                   CLEAR                   SSNAME
     C                   ENDIF
     C*
     C     SSBLST        IFEQ      *BLANKS
     C                   CLEAR                   SSBSDS
     C                   ENDIF
     C*
     C     SSFEIN        IFEQ      *ZEROS
     C     SSMOD         ANDEQ     *BLANKS
     C                   CLEAR                   SSPDNM
     C                   ENDIF
     C*
     C     SSIUSR        IFEQ      *BLANKS
     C                   CLEAR                   SSUSNM
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
     C     SSSUB#        IFNE      *ZEROS
     C     SSSUB#        ANDNE     EOSUB#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSSUB# NE *ZEROS
     C*
     C     SSFND         IFNE      *ZEROS
     C     SSFND         ANDNE     EOFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFND NE *ZEROS
     C*
     C     SSFYR         IFNE      *ZEROS
     C     SSFYR         ANDNE     EOFYR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFYR NE *ZEROS
     C*
     C     SSCASE        IFNE      *ZEROS
     C     SSCASE        ANDNE     EOCASE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSCASE NE *ZEROS
     C*
     C     SSFEIN        IFNE      *ZEROS
     C     SSFEIN        ANDNE     EOFEIN
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFEIN NE * ZEROS
     C*
     C                   MOVEL     SSMOD         XXMOD             3 0
     C     SSMOD         IFNE      *BLANKS
     C     XXMOD         ANDNE     EOMOD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSMOD NE *ZEROS
     C*
     C     SSEOB#        IFNE      *ZEROS
     C     SSEOB#        ANDNE     EOEOB#
     C*
     C                   MOVEL(P)  *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C     SSCERT        IFNE      *BLANKS
     C     SSCERT        ANDNE     EOCERT
     C*
     C                   MOVEL(P)  *OFF          MEETSS
     C*
     C                   ENDIF
     C*
     C* SUBSET DATE OF SERVICE SHOULD FALL WITHIN EOMB SERVICE DATE
     C* RANGE (EOFRDT - EOTODT)
     C*
     C                   Z-ADD     SSFRDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLFRDT
     C                   Z-ADD     SSTODT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLTODT
     C*
     C* SUBSET CRITERIA WHEN ONLY FROM DATE IS ENTERED
     C*
     C     HLFRDT        IFGT      *ZEROS
     C     HLTODT        ANDEQ     *ZEROS
     C     EOFRDT        IFLT      HLFRDT
     C     EOTODT        ANDLT     HLFRDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  EOFRDT LT HLFRDT
     C                   ENDIF                                                  HLFRDT GT *ZEROS
     C*
     C* SUBSET CRITERIA WHEN RANGE IS SPECIFIED
     C*
     C     HLFRDT        IFGT      *ZEROS
     C     HLTODT        ANDGT     *ZEROS
     C*
     C     EOTODT        IFLT      HLFRDT
     C     EOFRDT        ORGT      HLTODT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  EOTODT LT HLFRDT
     C*
     C                   ENDIF                                                  HLFRDT GT *ZEROS
     C*
     C     SSBLST        IFNE      *BLANKS
     C     SSBLST        ANDNE     EOBLST
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSBLST = *BLANKS
     C*
     C     SSBTYP        IFNE      *BLANKS
     C     SSBTYP        ANDNE     EOBTYP
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSBTYP = *BLANKS
     C*
     C     SSIUSR        IFNE      *BLANKS
     C     SSIUSR        ANDNE     EOIUSR
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSIUSR = *BLANKS
     C*
     C     Q1PRMS        IFGT      12
     C     @@ERR         IFNE      *BLANKS
     C     @@ERR         ANDNE     EOCRPE
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  @@ERR = *BLANKS
     C                   ENDIF
     C*** BEGIN ADD *** 10/11/02 **************************************
     C     EOCRPE        IFEQ      'T'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  @@ERR = *BLANKS
     C*** END   ADD *** 10/11/02 **************************************
     C*** Beg Add *** 06/11/12 ****************************************
      /Free
         //This picks up current Code Review Bills

         If xxCall = *Blanks;
         // If EoCrPe <> 'C' Or EoCrPe <> 'F'; ***////
            If EoCrPe <> 'C' and EoCrPe <> 'F';
              Meetss = *Off;
            Endif;
         Endif;

         //This picks up current Claim Check Bills

         If xxCall = 'C';
         //*If EoCrPe <> 'Y' Or EoCrPe <> 'E'; ***//
            If EoCrPe <> 'Y' and EoCrPe <> 'E';
              Meetss = *Off;
            Endif;
         Endif;
      /End-Free
     C*** End Add *** 06/11/12 ****************************************
     C*
     C                   ENDSR
** Column headings
Opt Claimant                EOMB#    Provider       From          To      Sts
Opt Provider       Claimant                EOMB#    From          To      Sts
Opt Soc Sec #     DOI        EOMB#   Provider       From          To      Sts
Opt    EOMB#   Claimant             Provider        From          To      Sts
Opt Carrier Rcv Dt Claimant                EOMB#    From          To      Sts
Opt Claimant                EOMB#    Provider    Charge Amt   Paid Amt Typ AST
Opt Provider       Claimant                EOMB# Charge Amt   Paid Amt Typ AST
Opt Soc Sec #     DOI        EOMB#   Provider    Charge Amt   Paid Amt Typ AST
Opt    EOMB#   Claimant             Provider     Charge Amt   Paid Amt Typ AST
Opt Carrier Rcv Dt Claimant                EOMB# Charge Amt   Paid Amt Typ AST
Opt Claimant                EOMB#    Provider     Control Number     Sts
Opt Provider       Claimant                EOMB#  Control Number     Sts
Opt Soc Sec #     DOI        EOMB#   Provider     Control Number     Sts
Opt    EOMB#   Claimant             Provider      Control Number     Sts
Opt Carrier Rcv Dt Claimant                EOMB#  Control Number     Sts
Opt Claimant                EOMB#    Provider     Provider's Name         Sts
Opt Provider       Claimant                EOMB#  Provider's Name         Sts
Opt Soc Sec #     DOI        EOMB#   Provider     Provider's Name         Sts
Opt    EOMB#   Claimant             Provider      Provider's Name         Sts
Opt Carrier Rcv Dt Claimant                EOMB#  Provider's Name         Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
1077=Work w/ Detail         YYY
1088=Work w/ Amounts        YYN
11010=EOMB status           YYY
11111=EOB/Notes             YYY
11414=DARTS                 YNN
11616=History               YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
209F9=Change Info           YYN
211F11=Change view          YYN
212F12=Cancel               NNY
215F15=Fold/Truncate        YYN
217F17=Subset               YYY
3
**  (QCMDEX)
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
OVRDBF FILE(FTPSL) TOFILE(*LIBL/FTPSL  )
OVRDBF FILE(FMSVP) TOFILE(*LIBL/FMSVP  )
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
OVRDBF FILE(FMSVL2) TOFILE(*LIBL/FMSVL2  )
OVRDBF FILE(FMPRP) TOFILE(*LIBL/FMPRP  )
DLTOVR FILE(FTABL)
DLTOVR FILE(FMPRL)
DLTOVR FILE(FTPSL)
DLTOVR FILE(FMSVP)
DLTOVR FILE(FMSVL1)
DLTOVR FILE(FMSVL2)
DLTOVR FILE(FMPRP)
