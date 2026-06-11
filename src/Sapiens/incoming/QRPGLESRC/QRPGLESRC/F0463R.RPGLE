     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0463R - Work With EOMBs Detail Information              *
     H*****************************************************************
     H* F0463R - Work With EOMBs Detail Information                   *
     H* Project: FL007  C:117                                         *
     H*                                                               *
     H*  Created:  C:117  01/15/2000                                  *
     H*                                                               *
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*  04/07/09 C:079 SUPPORT: 67538                                *
     H*                 Update Paid Revenue Code                      *
     H*                                                               *
     H*  10/16/08 C:191 PROJECT: TX045 PRISS #26                      *
     H*                 Add Place of service                          *
     H*                                                               *
     H*  04/30/08 C:191 PROJECT: TX045                                *
     H*                 Add new fields                                *
     H*                                                               *
     H*  03/29/07 C:188 PROJECT: CA002 EDI 837                        *
     H*                 MODIFIED HCPCS TO ALLOW FILE SELECTION        *
     H*                                                               *
     H*  04/23/02 C:79 Support: MC048 32199                           *
     H*                1. Use billed code fields instead of Paid code.*
     H*                Note: Make sure FIX32199 is run prior to instal*
     H*                2. Chain to correct record in FDDUP            *
     H*                                                               *
     H*****************************************************************
     FF0463F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFDDUP     UF A E             DISK
     F                                     RENAME(PDU01:DBRCDF)
     FFDDUL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFAEOP     IF   E             DISK
     FFDCAP     IF   E             DISK
     FFAEUL     IF   E           K DISK
     F***FTABL   IF  E           K        DISK
     F***         PRC01                             KIGNORE
     F***         PPS01                             KIGNORE
     F*  WCMC Control File
     FFMCXL     IF   E           K DISK
     F* Service Code File
     FFMSVL1    IF   E           K DISK
     F                                     INFDS(DBFSV)
     F*** BEG ADD ****** 04/30/08 ************************************
     FFMPRLTX   IF   E           K DISK
     F                                     RENAME(PPR01:PTX01)
     FFTABLTX   IF   E           K DISK
     F                                     IGNORE(PAC01)
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PEB01)
     F                                     IGNORE(PFT01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PPS01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PTS01)
     F*** END ADD ****** 04/30/08 ************************************
     F*** BEG ADD ****** 10/16/08 ************************************
     F*  Place of Service Table File
     FFTPSL     IF   E           K DISK
     F*** END ADD ****** 10/16/08 ************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D                 DS
     D  K1CMSV                 1     11
     D  K1RVCD                 1      3
     D  K1HCPC                 4      8
     D  K1UNTS                 9     11
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Detail - Data Subm C-
     D                                     hgs')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D DBFPR           DS
     D  PRRRN                397    400B 0
     D DBFSV           DS
     D  SVRRN                397    400B 0
     D*** BEG ADD ****** 04/30/08 ************************************
     D NT0             C                   CONST('Field was changed fo-
     D                                     r EDI correction:')
     D NT1             C                   CONST('Original Value:')
     D FL1const        C                   CONST('Revenue code/DRG pai-
     D                                     d code')
     D FL2const        C                   CONST('HCPCS/CPT4 code')
     D FL3const        C                   CONST('Mod paid code')
     D*** BEG ADD ****** 10/16/08 ************************************
     D FL4const        C                   CONST('Place of service')
     D*** END ADD ****** 10/16/08 ************************************
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
     C                   PARM                    @@RRN             9 0          FDDHP
     C                   PARM                    @@RRN2            9 0          FAEOP
     C                   PARM                    @@RRN4            9 0          FDCAP
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C*
     C     APIPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM      EOSUB#        #2SUB#            5 0
     C                   PARM      EOFND         #2FND             3 0
     C                   PARM      EOFYR         #2FYR             3 0
     C                   PARM      EOCASE        #2CASE            7 0
     C                   PARM      EOEOB#        #2EOB#            9 0
     C                   PARM      EORCN         #2RCN             3 0
     C                   PARM      DUDSEQ        #2DSEQ            5 0
     C                   PARM      'D'           #2CALL            1
     C                   PARM                    #2OVAL           70
     C                   PARM                    #2NVAL           70
     C*** END ADD ****** 04/30/08 ************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKFDAT
     C*
     C     RCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C     PRKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKCPT4
     C*
     C     CAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     EOKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     DIKY          KLIST
     C                   KFLD                    XXDIA
     C*
     C*  Key List for WCMC Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*  Key List for Header File
     C*
     C     EUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C*  Key List for Detail File
     C*
     C     DUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*** BEG ADD *** 04/23/02 ****************************************
     C                   KFLD                    KKDSEQ
     C*** END ADD *** 04/23/02 ****************************************
     C*
     C     SVKY1         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    K1CMSV
     C                   KFLD                    K1EFDT
     C*
     C     SVKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    K1CMSV
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY            10
     C*** END ADD ****** 04/30/08 ************************************
     C*** BEG ADD ****** 10/16/08 ************************************
     C     PSKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKPOS
     C*** END ADD ****** 10/16/08 ************************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    DUCO#         KKCO#
     C     *LIKE         DEFINE    DUSUB#        KKSUB#
     C     *LIKE         DEFINE    DUFND         KKFND
     C     *LIKE         DEFINE    DUFYR         KKFYR
     C     *LIKE         DEFINE    DUCASE        KKCASE
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    CACPT4        KKCPT4
     C     *LIKE         DEFINE    DUSS#         KKSS#
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EODIA1        XXDIA
     C     *LIKE         DEFINE    EOCNTL        QQCNTL
     C     *LIKE         DEFINE    CACPT4        HLCPT4
     C     *LIKE         DEFINE    SVCMSV        KKCMSV
     C     *LIKE         DEFINE    SVEFDT        K1EFDT
     C*** BEG ADD *** 04/23/02 ****************************************
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C*** END ADD *** 04/23/02 ****************************************
     C*** BEG ADD ****** 04/30/08 *************************************
     C     *LIKE         DEFINE    DUBPRC        XOBPRC
     C     *LIKE         DEFINE    DURATE        XORTHC
     C     *LIKE         DEFINE    DUPMOD        XOPMOD
     C*** END ADD ****** 04/30/08 *************************************
     C*** BEG ADD ****** 10/16/08 ************************************
     C     *LIKE         DEFINE    DUPOS         XOPOS
     C     *LIKE         DEFINE    PSPOS         KKPOS
     C*** END ADD ****** 10/16/08 ************************************
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
     C* BOTH REVENUE CODE AND CPT/HCPCS CANNOT BE EMPTY
     C*
     C  N90SCRVCD        IFEQ      *ZEROS
     C*** BEG CHG *** 04/07/09 ****************************************
     C*********  SCRATE    ANDEQ*BLANKS
     C     SCBRAT        ANDEQ     *BLANKS
     C*** END CHG *** 04/07/09 ****************************************
     C                   SETON                                        9073
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCRVCD = *ZEROS
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C* VALIDATE FOR TEXAS
     C     EOAST         IFEQ      'TX'
     C*
     C*** BEG CHG *** 04/07/09 ****************************************
     C*********  SCRATE    IFNE *BLANKS
     C     SCBRAT        IFNE      *BLANKS
     C*** END CHG *** 04/07/09 ****************************************
     C*** BEG CHG *** 04/07/09 ****************************************
     C*******************  MOVE SCRATE    KKCPT4
     C                   MOVE      SCBRAT        KKCPT4
     C*** END CHG *** 04/07/09 ****************************************
     C     PRKY          SETLL     PTX01
     C     PRKY          READE     PTX01                                  99
     C     *IN99         DOWEQ     *OFF
     C     PRSTAT        IFNE      'I'
     C                   MOVEL(P)  PRDSC1        SCDSC1
     C                   LEAVE
     C                   ENDIF
     C     PRKY          READE     PTX01                                  99
     C                   ENDDO
     C* NOT FOUND
     C     *IN99         IFEQ      *ON
     C     *IN99         OREQ      *OFF
     C     PR1AG         ANDEQ     *ON
     C                   SETON                                        9089
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  SCBRAT=*ZEROS
     C*
     C     SCRTHC        IFNE      *BLANKS
     C                   MOVE      SCRTHC        KKCPT4
     C     PRKY          SETLL     PTX01
     C     PRKY          READE     PTX01                                  99
     C     *IN99         DOWEQ     *OFF
     C     PRSTAT        IFNE      'I'
     C                   MOVEL(P)  PRDSC1        SCDESC
     C                   LEAVE
     C                   ENDIF
     C     PRKY          READE     PTX01                                  99
     C                   ENDDO
     C* NOT FOUND
     C     *IN99         IFEQ      *ON
     C     *IN99         OREQ      *OFF
     C     PR1AG         ANDEQ     *ON
     C                   SETON                                        9076
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF                                                  SCRTHC=*ZEROS
     C*
     C     SCPMOD        IFNE      *BLANKS
     C                   MOVEL(P)  SCPMOD        KKKEY
     C     TBKY          CHAIN     PPM01                              99
     C     *IN99         IFEQ      *ON
     C     PMSTAT        OREQ      'I'
     C                   SETON                                        9078
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCPMOD IFNE *BLANKS
     C*** BEG ADD ****** 10/16/08 ************************************
     C* Validate place of service for Texas
     C     SCPOS         IFNE      *ZEROS
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9082
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 = *ON
     C                   MOVEL(P)  PSDESC        SCPODS
     C                   ENDIF                                                  SCPOS IFEQ BLANKS
     C*
     C*** END ADD ****** 10/16/08 ************************************
     C                   ENDIF                                                  EOAST IFEQ 'TX'
     C*** END ADD ****** 04/30/08 ************************************
     C*
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
     C                   SELECT
     C*
     C* REVENUE CODE SEARCH
     C*
     C     HFLD          WHENEQ    'SCRVCD'
     C                   MOVEL     SCRVCD        HLRVCD            3 0
     C*** BEG CHG *** 04/07/09 ****************************************
     C*******************  MOVELSCRATE    HLRATE  5
     C                   MOVEL     SCBRAT        HLRATE            5
     C*** END CHG *** 04/07/09 ****************************************
     C                   MOVEL     SCDSC1        HLDSC1           40
     C                   MOVE      EOAST         ##ST              2
     C                   MOVE      *BLANKS       ##CMSV           15
     C                   MOVE      *BLANKS       ##CDSC           40
     C*
     C     SCRVCD        IFNE      *ZEROS
     C*
     C                   MOVEL(P)  SCRVCD        ##CMSV
     C*
     C                   ENDIF
     C*
     C                   CALL      'F2320R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##ST
     C                   PARM                    ##CMSV
     C                   PARM                    ##CDSC
     C*
     C     ##KL          IFEQ      '1'
     C     ##CMSV        OREQ      *BLANKS
     C                   MOVEL     HLRVCD        SCRVCD
     C*** BEG CHG *** 04/07/09 ****************************************
     C*******************  MOVELHLRATE    SCRATE
     C                   MOVEL     HLRATE        SCBRAT
     C*** END CHG *** 04/07/09 ****************************************
     C                   MOVEL(P)  HLDSC1        SCDSC1
     C*** BEG CHG *** 04/07/09 ****************************************
     C******************   SETON                     9071
     C                   SETON                                        9074
     C*** END CHG *** 04/07/09 ****************************************
     C                   ENDIF                                                  ##KL = '1'
     C*
     C     ##CMSV        IFNE      *BLANKS
     C                   MOVEL     ##CMSV        SCRVCD
     C*** BEG CHG *** 04/07/09 ****************************************
     C*********  5         SUBST##CMSV:4  SCRATE
     C     5             SUBST     ##CMSV:4      SCBRAT
     C*** END CHG *** 04/07/09 ****************************************
     C                   MOVEL(P)  ##CDSC        SCDSC1
     C*** BEG CHG *** 04/07/09 ****************************************
     C*******************  SETON                     9072
     C                   SETON                                        9077
     C*** END CHG *** 04/07/09 ****************************************
     C                   ENDIF                                                  ##CMSV NE *BLANKS
     C*
     C* HCPCS/CPT4 CODE SEARCH
     C*
     C*** BEG CHG *** 04/07/09 ****************************************
     C********   HFLD      WHEQ 'SCRATE'
     C     HFLD          WHENEQ    'SCBRAT'
     C*** END CHG *** 04/07/09 ****************************************
     C                   CALL      'F2016R'
     C                   PARM      '1'           ##MODE            1
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C*** BEG CHG *** 04/07/09 ****************************************
     C******************   PARM           ##CMSV
     C                   PARM      *BLANKS       ##CMSV
     C*** END CHG *** 04/07/09 ****************************************
     C                   PARM                    ##CDSC
     C                   PARM      ' '           @@RTCD            1
     C                   PARM                    ##ST
     C*
     C     ##CMSV        IFNE      *BLANKS
     C*
     C* If selection was from the procedure code file don't pop rev cod
     C*
     C     @@RTCD        IFNE      'C'
     C                   MOVEL     ##CMSV        SCRVCD
     C                   ENDIF
     C*
     C*** BEG CHG *** 04/07/09 ****************************************
     C********** 5         SUBST##CMSV:4  SCRATE
     C     5             SUBST     ##CMSV:4      SCBRAT
     C*** END CHG *** 04/07/09 ****************************************
     C                   MOVEL(P)  ##CDSC        SCDSC1
     C*** BEG CHG *** 04/07/09 ****************************************
     C******************   SETON                     9071
     C                   SETON                                        9074
     C*** END CHG *** 04/07/09 ****************************************
     C                   ELSE
     C                   SETON                                        9072
     C                   ENDIF                                                  ##CMSV NE *BLANKS
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C     HFLD          WHENEQ    'SCRTHC'
     C     EOAST         ANDEQ     'TX'
     C                   MOVE(P)   SCRTHC        XXRTHC            5
     C                   MOVE(P)   SCDESC        XXDESC           40
     C                   MOVE      *BLANKS       ##PROC            5
     C                   MOVE      *BLANKS       ##PROD           50
     C                   Z-ADD     0             #FRRN1            9 0
     C                   MOVE      EOAST         #FAST             2
     C                   CALL      'F2013R'
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    #FRRN1
     C                   PARM                    ##PROC
     C                   PARM                    ##PROD
     C                   PARM      ' '           ##RTCD
     C                   PARM                    #FAST
     C*
     C     ##RTCD        IFEQ      '1'
     C     ##PROD        OREQ      *BLANKS
     C                   MOVEL(P)  XXRTHC        SCRTHC
     C                   MOVEL(P)  XXDESC        SCDESC
     C                   SETON                                        9075      pos cursor
     C                   ELSE
     C                   MOVEL(P)  ##PROC        SCRTHC
     C                   MOVEL(P)  ##PROD        SCDESC
     C                   SETON                                        9077      pos cursor
     C                   ENDIF                                                  #FCODE<>' '
     C*
     C     HFLD          WHENEQ    'SCPMOD'
     C     EOAST         ANDEQ     'TX'
     C                   MOVE      EOAST         #FAST             2
     C                   MOVE(P)   *BLANKS       #FKEY            15
     C                   MOVE(P)   SCPMOD        XXMOD             2
     C                   CALL      'F2315R'
     C                   PARM      '2'           #FCALL            1
     C                   PARM      ' '           #FKC              1
     C                   PARM      ' '           #FKL              1
     C                   PARM                    #FAST
     C                   PARM                    #FKEY            15
     C*
     C     #FKC          IFEQ      '1'
     C     #FKL          OREQ      '1'
     C     #FKEY         OREQ      *BLANKS
     C                   MOVE      XXMOD         SCPMOD
     C                   SETON                                        9077
     C                   ELSE
     C                   MOVEL     #FKEY         SCPMOD
     C                   SETON                                        90
     C*** BEG ADD ****** 10/16/08 ************************************
     C                   SETON                                        80
     C*** END ADD ****** 10/16/08 ************************************
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCBPRC'
     C     EOAST         ANDEQ     'TX'
     C                   SETON                                        9079
     C*
     C*** END ADD ****** 04/30/08 ************************************
     C*** BEG ADD ****** 10/16/08 ************************************
     C     HFLD          WHENEQ    'SCPOS'
     C     EOAST         ANDEQ     'TX'
     C                   MOVE      EOAST         ##AST
     C                   MOVE      *BLANKS       ##POS
     C                   MOVEL     SCPOS         HLPOS            15
     C                   MOVEL     SCPODS        HLPODS           40
     C                   CALL      'F2370R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##AST             2
     C                   PARM                    ##POS            15
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##POS         OREQ      *BLANKS
     C                   MOVEL(P)  HLPOS         SCPOS
     C                   MOVEL(P)  HLPODS        SCPODS
     C                   SETON                                        9080      pos cursor
     C                   ELSE
     C                   MOVEL     ##POS         TPPOS             3
     C                   MOVE      TPPOS         SCPOS
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C                   SETON                                        90        pos cursor
     C                   ENDIF                                                  ##KC IFEQ 1
     C*** END ADD ****** 10/16/08 ************************************
     C                   ENDSL                                                  ##CMSV NE *BLANKS
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*
     C     @@RRN2        CHAIN     PEO01                              90
     C*
     C                   MOVEL     EOCO#         KKCO#
     C                   MOVEL     EOSUB#        KKSUB#
     C                   MOVEL     EOFND         KKFND
     C                   MOVEL     EOFYR         KKFYR
     C                   MOVEL     EOCASE        KKCASE
     C                   MOVEL     EOEOB#        KKEOB#
     C*
     C** Get 'Amount & type code' headings
     C*
     C     CXKY          CHAIN     PCX01                              99
     C                   MOVEL     CXHMRA        SCHMR1
     C                   MOVEL     CXHNET        SCHNTW
     C                   MOVEL     CXHOT1        SCHOT1
     C                   MOVEL     CXHOT2        SCHOT2
     C*
     C                   MOVEL(P)  EOSUB#        XXSUB#            5
     C                   MOVEL(P)  EOFND         XXFND             3
     C                   MOVEL(P)  EOFYR         XXFYR             3
     C                   MOVEL(P)  EOCASE        XXCASE            7
     C                   MOVEL     EOEOB#        SCEOMB
     C                   MOVEL     EOCNTL        SCCNTL
     C*
     C*** BEG DEL *** 04/23/02 ****************************************
     C********** DUKY      CHAINPDU01               N60
     C*** END DEL *** 04/23/02 ****************************************
     C*
     C     CDKY          CHAIN     PCD01                              60
     C     EUKY          CHAIN     PEU01                              60
     C     @@RRN4        CHAIN     PCA01                              60
     C*
     C*** BEG ADD *** 04/23/02 ****************************************
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     DUKY          CHAIN(N)  PDU01                              60
     C*** END ADD *** 04/23/02 ****************************************
     C*
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCFDAT
     C*** BEG CHG *** 04/23/02 ****************************************
     C******************** MOVELDURVCD    SCRVCD    P
     C******************** MOVELDUDESC    SCDSC2    P
     C******************** MOVELDURATE    SCRATE
     C*** BEG CHG *** 04/07/09 ****************************************
     C*******************  MOVELDUBPRC    SCRVCD    P
     C*******************  MOVELDUBRAT    SCRATE
     C                   MOVEL(P)  DURVCD        SCRVCD
     C                   MOVEL     DURATE        SCRTHC
     C*** END CHG *** 04/07/09 ****************************************
     C*** END CHG *** 04/23/02 ****************************************
     C*** BEG ADD ****** 04/30/08 ************************************
     C                   MOVEL(P)  DUBPRC        SCBPRC
     C*** BEG CHG *** 04/07/09 ****************************************
     C*****************    MOVELDURATE    SCRTHC    P
     C                   MOVEL(P)  DUBRAT        SCBRAT
     C*** END CHG *** 04/07/09 ****************************************
     C                   MOVEL(P)  DUPMOD        SCPMOD
     C                   MOVEL(P)  DUBPRC        XOBPRC
     C                   MOVEL(P)  DURATE        XORTHC
     C                   MOVEL(P)  DUPMOD        XOPMOD
     C                   MOVEL(P)  DUBPRD        SCDSC1
     C                   MOVEL(P)  DUDESC        SCDESC
     C*** END ADD ****** 04/30/08 ************************************
     C*** BEG ADD ****** 10/16/08 ************************************
     C                   MOVE      DUPOS         XOPOS
     C                   MOVE      DUPOS         SCPOS
     C                   MOVE(P)   SCPOS         KKPOS
     C     PSKY          CHAIN     PPS01                              99
     C  N99              MOVEL(P)  PSDESC        SCPODS
     C*** END ADD ****** 10/16/08 ************************************
     C                   MOVE      SCRVCD        K1RVCD
     C*** BEG CHG *** 04/07/09 ****************************************
     C*******************  MOVE SCRATE    K1HCPC
     C                   MOVE      SCBRAT        K1HCPC
     C*** END CHG *** 04/07/09 ****************************************
     C                   MOVE      *ZEROS        K1UNTS
     C                   Z-ADD     SCFDAT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         K1EFDT
     C     SVKY1         SETGT     PSV01
     C     SVKY2         READPE    PSV01                                  99
     C     *IN99         IFEQ      *OFF
     C     SVSTAT        ANDEQ     'A'
     C                   MOVEL(P)  SVDESC        SCDSC1
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   Z-ADD     CACHRG        SCTOTC
     C                   Z-ADD     CAPAID        SCTOTP
     C                   Z-ADD     CAMRA         SCMRA
     C                   Z-ADD     CANDIS        SCNTWK
     C                   Z-ADD     CAOTHR        SCOTH1
     C                   Z-ADD     CAOTH2        SCOTH2
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*** BEG ADD ****** 04/30/08 ************************************
     C*** BEG DEL ****** 04/07/09 ************************************
     C*******************  MOVELSCBPRC    DUBPRC    P
     C*******************  MOVELSCRTHC    DURATE    P
     C*******************  MOVELSCPMOD    DUPMOD    P
     C*** END DEL ****** 04/07/09 ************************************
     C*** END ADD ****** 04/30/08 ************************************
     C*** BEG ADD ****** 04/07/09 ************************************
     C                   MOVEL(P)  SCRVCD        DURVCD
     C                   MOVEL     SCRTHC        DURATE
     C                   MOVEL(P)  SCDESC        DUDESC
     C                   MOVEL(P)  SCPMOD        DUPMOD
     C                   MOVEL(P)  SCBPRC        DUBPRC
     C                   MOVEL     SCBRAT        DUBRAT
     C                   MOVEL(P)  SCDSC1        DUBPRD
     C*** END DEL ****** 04/07/09 ************************************
     C*** BEG ADD ****** 10/16/08 ************************************
     C                   MOVE(P)   SCPOS         DUPOS
     C*** END ADD ****** 10/16/08 ************************************
     C*** BEG ADD *** 04/23/02 ****************************************
     C*******************  MOVELSCRVCD    DUBPRC    P
     C*******************  MOVELSCRATE    DUBRAT
     C*******************  MOVELSCDSC1    DUBPRD    P
     C*** END DEL ****** 04/07/09 ************************************
     C*
     C*** END ADD *** 04/23/02 ****************************************
     C*
     C                   MOVE      Q1USER        DUUSER
     C                   MOVE      Q1PGM         DUPGM
     C                   MOVE      DSTIME        DUTIME
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DUDT
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
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C*** BEG ADD ****** 04/30/08 ************************************
     C     EOAST         IFEQ      'TX'
     C                   SETON                                        59
     C                   ELSE
     C                   SETOFF                                       59
     C                   ENDIF
     C*** END ADD ****** 04/30/08 ************************************
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
     C*******    @@RRN     CHAINDBRCDF               99
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
     C*** BEG ADD ****** 04/30/08 ************************************
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   EXSR      SRWREP
     C*** END ADD ****** 04/30/08 ************************************
E01  C                   ENDIF
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FDDUP
     C*** BEG ADD ****** 04/30/08 ************************************
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   EXSR      SRWREP
     C*** END ADD ****** 04/30/08 ************************************
     C                   ENDSR
     C*** BEG ADD ****** 04/30/08 ************************************
     C*================================================================
     C* SRWREP - Write header note record
     C*================================================================
     C     SRWREP        BEGSR
     C* IF FIELDS CHANGED THEN CALL API
     C*
     C     DUBPRC        IFNE      XOBPRC
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL1const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XOBPRC        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  DHCPT4
     C*
     C     DURATE        IFNE      XORTHC
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL2const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XORTHC        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  DHMOD1
     C*
     C     DUPMOD        IFNE      XOPMOD
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL3const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XOPMOD        XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  DHMOD2
     C*
     C*** BEG ADD ****** 10/16/08 ************************************
     C     DUPOS         IFNE      XOPOS
     C* CHANGED FIELD DESCRIPTION
     C                   MOVEL(P)  NT0           XXNT1
     C                   MOVEL(P)  FL4const      XXNT2
     C                   MOVEL(P)  XXNOT1        #2OVAL
     C* ACTUAL ORIGINAL VALUE
     C                   MOVEL(P)  NT1           XXNT21
     C                   MOVEL(P)  XOPOS         XXNT22
     C                   MOVEL(P)  XXNOT2        #2NVAL
     C* CALL FA758R WITH THE CALL PARAMETER SET TO 'D'
     C                   CALL      'FA758R'      APIPL
     C                   ENDIF                                                  DUPOS
     C*** END ADD ****** 10/16/08 ************************************
     C                   ENDSR
     C*** END ADD ****** 04/30/08 ************************************
